macro_rules! profile {
    ($name:expr, $block:block) => {{
        #[cfg(target_os = "zkvm")]
        {
            println!("cycle-tracker-start: {}", $name);
            let result = (|| $block)();
            println!("cycle-tracker-end: {}", $name);
            result
        }

        #[cfg(not(target_os = "zkvm"))]
        {
            $block
        }
    }};
}
