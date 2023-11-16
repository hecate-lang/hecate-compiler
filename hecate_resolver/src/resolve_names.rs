use std::collections::{HashMap, HashSet};

use hecate_parser::types::GenericType;
use hecate_util::{ast::{Module, AstInfo, Function, Expression, Expr, Statement, BinaryOp, UnaryOp}, span::{Spanned, Span}};

use crate::{FullyResolved, RefId, ResolvedType, ModData, ResolvedRef};

struct Scope<'a> {
    variables: HashMap<&'a str, RefId<ResolvedRef>>
}

impl<'a> Scope<'a> {
    fn new() -> Self {
        Self {
            variables: HashMap::new()
        }
    }
}

pub fn resolve_names<'a>(module: &'a Module<'a, RawSymbols<'a>>) -> Result<Module<'a, NamesResolved<'a>>, ()> {
    let mut resolver = NameResolver {
        scope_stack: vec![],
        info: ModData {
            references: HashMap::new(),
        }
    };
    resolver.scope_stack.push(Scope::new());
    let mod_name = RefId::new();
    resolver.info.references.insert(mod_name, *module.name);
    let p32 = resolver.add_var("__print_i32");
    resolver.info.references.insert(p32, "__print_i32");
    
    
    // register functions
    module.functions.iter().for_each(|f| { resolver.add_var(&f.name); });
    // resolve functions
    let functions = module.functions.iter()
        .map(|function| 
            resolver.resolve_function(function)
                .map(|f| function.use_span_for(f))
        )
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Module {
        name: module.name.use_span_for(mod_name), 
        functions, 
        data: resolver.info 
    })
}

struct NameResolver<'a> {
    scope_stack: Vec<Scope<'a>>,
    info: ModData<'a>
}

impl<'a> NameResolver<'a> {
    fn get_var(&self, name: &str) -> Result<RefId<ResolvedRef>, ()> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(var) = scope.variables.get(name) {
                return Ok(*var)
            }
        }
        Err(())
    }

    fn add_var(&mut self, name: &'a str) -> RefId<ResolvedRef> {
        let id = RefId::new();
        self.info.references.insert(id, name);
        // unwrap should be ok since the stack should hold at least 1 element
        self.scope_stack.last_mut().unwrap().variables.insert(name, id);
        id
    }

    fn resolve_function(&mut self, function: &'a Function<'a, RawSymbols<'a>>) -> Result<Function<'a, NamesResolved<'a>>, ()> {
        self.scope_stack.push(Scope::new());
        let name = RefId::new();
        self.info.references.insert(name, *function.name);
        let mut args = vec![];
        let mut arg_set = HashSet::new();
        for (arg, ty) in &function.args {
            if arg_set.contains(&**arg) {
                todo!()
            }
            let arg_id = self.add_var(**arg);
            let arg_ty_id = RefId::new();
            arg_set.insert(**arg);
            args.push((arg.use_span_for(arg_id), ty.use_span_for(arg_ty_id)));
        }
        let body = self.resolve_expression(&function.body)?;
        self.scope_stack.pop();
        Ok(Function {
            name: function.name.use_span_for(name), 
            args,
            ret: Span::dummied(RefId::new()), 
            body
        })
    }

    fn resolve_expression(&mut self, expression: &'a Spanned<Expression<RawSymbols>>) -> Result<Spanned<'a, Expression<'a, NamesResolved<'a>>>, ()> {
        Ok(expression.use_span_for(Expression { 
            expr: match &expression.expr {
                Expr::Binary(op, a, b) => self.resolve_binary(op, a, b)?,
                Expr::Unary(op, expr) => self.resolve_unary(op, expr)?,
                Expr::Variable(v) => Expr::Variable(v.use_span_for(self.get_var(**v)?)),
                Expr::FunctionCall(func, args) => self.resolve_function_call(func, args)?,
                Expr::If(if_do, otherwise) => self.resolve_if(if_do, otherwise)?,
                Expr::Block(stmts, expr) => self.resolve_block(stmts, expr)?,
                Expr::Return(expr) => self.resolve_return(expr)?,
                Expr::Literal(v) => Expr::Literal(*v),
            }, 
            ty: RefId::new() 
        }))
    }

    #[allow(clippy::type_complexity)]
    fn resolve_if(&mut self, if_do: &'a Vec<(Spanned<Expression<RawSymbols>>, Spanned<Expression<RawSymbols>>)>, otherwise: &'a Spanned<Expression<RawSymbols>>) -> Result<Expr<'a, NamesResolved<'a>>, ()> {
        let if_do = if_do.iter()
            .map(|(cond, cont)| {
                let cond = self.resolve_expression(cond)?;
                let cont = self.resolve_expression(cont)?;
                Ok((cond, cont))
            }).collect::<Result<Vec<_>, _>>()?;
        let otherwise = self.resolve_expression(otherwise)?; 
        Ok(Expr::If(if_do, Box::new(otherwise)))
    }

    fn resolve_return(&mut self, expression: &'a Spanned<Expression<RawSymbols>>) -> Result<Expr<'a, NamesResolved<'a>>, ()> {
        Ok(Expr::Return(Box::new(self.resolve_expression(expression)?)))
    }

    fn resolve_block(&mut self, statements: &'a Vec<Spanned<Statement<RawSymbols>>>, expression: &'a Option<Box<Spanned<Expression<RawSymbols>>>>) -> Result<Expr<'a, NamesResolved<'a>>, ()> {
        self.scope_stack.push(Scope::new());
        let statements = statements.iter()
            .map(|statement|
                self.resolve_statement(statement)
                    .map(|s| statement.use_span_for(s))
            ).collect::<Result<Vec<_>, _>>()?;
        let expression = expression.as_ref()
            .map(|expression| self.resolve_expression(expression)
                .map(Box::new))
            .transpose()?;
        self.scope_stack.pop();
        Ok(Expr::Block(statements, expression))
    }

    fn resolve_binary(&mut self, operator: &'a Spanned<BinaryOp>, a: &'a Spanned<Expression<RawSymbols>>, b: &'a Spanned<Expression<RawSymbols>>) -> Result<Expr<'a, NamesResolved<'a>>, ()> {
        Ok(Expr::Binary(
            operator.clone(), 
            Box::new(self.resolve_expression(a)?), 
            Box::new(self.resolve_expression(b)?)
        ))
    }

    fn resolve_unary(&mut self, operator: &'a Spanned<UnaryOp>, expression: &'a Spanned<Expression<RawSymbols>>) -> Result<Expr<'a, NamesResolved<'a>>, ()> {
        Ok(Expr::Unary(operator.clone(), Box::new(self.resolve_expression(expression)?)))
    }

    fn resolve_function_call(&mut self, func: &'a Spanned<&'a str>, args: &'a Vec<Spanned<Expression<RawSymbols>>>) -> Result<Expr<'a, NamesResolved<'a>>, ()> {
        let args = args.iter().map(|expression| 
            self.resolve_expression(expression)
        ).collect::<Result<Vec<_>, _>>()?;
        Ok(Expr::FunctionCall(func.use_span_for(self.get_var(&func)?), args))
    }


    fn resolve_statement(&mut self, statement: &'a Spanned<Statement<RawSymbols>>) -> Result<Statement<'a, NamesResolved<'a>>, ()> {
        Ok(match &**statement {
            Statement::Expression(e) => Statement::Expression(self.resolve_expression(e)?),
            Statement::Let(var, ty, expr) => self.resolve_let_assign(var, ty, expr)?,
        })
    }

    fn resolve_let_assign(&mut self, var: &'a Spanned<&str>, ty: &'a Spanned<GenericType<&str>>, expression: &'a Spanned<Expression<RawSymbols>>) -> Result<Statement<'a, NamesResolved<'a>>, ()> {
        let id = self.add_var(var);
        let expression = self.resolve_expression(expression)?;
        Ok(Statement::Let(var.use_span_for(id), ty.use_span_for(RefId::new()), expression))
    }
}

pub struct RawSymbols<'a>(std::marker::PhantomData<&'a()>);

impl<'a> AstInfo for RawSymbols<'a> {
    type Type = GenericType<'a, &'a str>;
    type Ident = &'a str;
    type ModuleData = ();
}

#[derive(Debug)]
pub struct NamesResolved<'a>(std::marker::PhantomData<&'a()>);


impl<'a> AstInfo for NamesResolved<'a> {
    type Type = RefId<ResolvedType>;
    type Ident = RefId<ResolvedRef>;
    type ModuleData = ModData<'a>;
}