#![cfg_attr(target_os = "zkvm", no_std)]

/// Checks if a number is prime.
pub fn is_prime(n: u32) -> bool {
    if n <= 1 {
        return false;
    }
    if n <= 3 {
        return true;
    }
    if n % 2 == 0 || n % 3 == 0 {
        return false;
    }
    let mut i = 5;
    while i * i <= n {
        if n % i == 0 || n % (i + 2) == 0 {
            return false;
        }
        i += 6;
    }
    true
}

/// Counts the number of prime numbers up to a given limit.
pub fn count_primes(limit: u32) -> u32 {
    let mut count = 0;
    for i in 2..limit {
        if is_prime(i) {
            count += 1;
        }
    }
    count
}
