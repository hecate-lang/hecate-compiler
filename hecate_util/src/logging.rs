#[macro_export]
macro_rules! hecate_message {
    ($tag:literal) => {
        println!("[{}]", $tag);
    };
    ($tag:literal $($arg:tt)*) => {
        println!("[{}] {}", $tag, format!($($arg)*));
    };
}

#[macro_export(local_inner_macros)]
macro_rules! hecate_log {
    ($($arg:tt)*) => {
        hecate_message!("LOG" $($arg)*);
    };
}

#[macro_export(local_inner_macros)]
macro_rules! hecate_debug {
    ($($arg:tt)*) => {
        hecate_message!("DEBUG" $($arg)*);
    };
}
