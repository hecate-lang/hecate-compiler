<div align="center">
    <h1>Hecate Compiler</h1>
<img src="https://github.com/hecate-lang/.github/blob/main/hecate_banner_upscaled_10.png" alt="" style="width: 80%; image-rendering: pixelated">
    
[![Rust](https://img.shields.io/github/actions/workflow/status/hecate-lang/hecate-compiler/rust.yml?style=flat-square&label=tests)](https://github.com/hecate-lang/hecate-compiler/actions/workflows/rust.yml)
[![License](https://img.shields.io/github/license/hecate-lang/hecate-compiler?style=flat-square)](https://github.com/hecate-lang/hecate-compiler/blob/main/LICENSE)
![Platform](https://img.shields.io/badge/platform-linux%20|%20windows-blueviolet?style=flat-square)
[![Stars](https://img.shields.io/github/stars/hecate-lang/hecate-compiler?style=flat-square)](https://github.com/hecate-lang/hecate-compiler/stargazers)
[![Forks](https://img.shields.io/github/forks/hecate-lang/hecate-compiler?style=flat-square)](https://github.com/hecate-lang/hecate-compiler/network/members)
![GitHub repo size](https://img.shields.io/github/repo-size/hecate-lang/hecate-compiler?style=flat-square)
<!--
![Lines of code](https://img.shields.io/tokei/lines/github/hecate-lang/hecate-compiler?style=flat-square)
-->
</div>

## Instructions LLVM 15
1. Download LLVM 15
    - Download from LLVM official repo
        - [github release](https://github.com/llvm/llvm-project/releases/tag/llvmorg-15.0.6)
        - [llvm download page](https://releases.llvm.org/download.html)
    - Prebuilt binaries for windows
        - https://github.com/mun-lang/llvm-package-windows/releases/tag/v16.0.5
            - `-md.7z` stands for dynamic and `-mt.7z` stands for static
            - available with `msvc16` and `msvc17`
            - which of the 2x2 combnations work may be highly dependant on setup, `llvm-15.0.6-windows-x64-msvc16-mt.7z` has proven to be working so far

2. Set environment variable `LLVM_150_PREFIX=/path/to/llvm/root`
## Sturcture
- [hecate_lexer]
    - `Input` -> `Token`
- [hecate_parser]
    - `Token` -> `AST`
- [hecate_resolver]
    - `AST` -> `AST (low level)`
- [hecate_ir_gen]
    - `AST` -> `IR`
- [hecate_llvm_gen]
    - `IR` -> `LLVM_IR` -> `binary`
