// Copyright 2023 RISC Zero, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#![no_main]
#![no_std]

extern crate powdr_riscv_runtime;

pub struct PasswordRequest {
    pub password: &'static str,
    pub salt: [u8; 32],
}

#[no_mangle]
pub fn main() {
    let request = PasswordRequest {
        // Uncomment \/ to see it fail
        //password: "12345678",
        password: "S00perSecr1t!!!",
        salt: [0xaa; 32],
    };

    let policy = PasswordPolicy {
        min_length: 3,
        max_length: 64,
        min_numeric: 2,
        min_uppercase: 2,
        min_lowercase: 2,
        min_special_chars: 1,
    };

    assert!(policy.is_valid(&request.password));
}

struct PasswordPolicy {
    pub min_length: usize,
    pub max_length: usize,
    pub min_uppercase: usize,
    pub min_lowercase: usize,
    pub min_numeric: usize,
    pub min_special_chars: usize,
}

impl PasswordPolicy {
    pub fn is_valid(&self, pw: &str) -> bool {
        let metrics = PasswordMetrics::new(pw);
        self.correct_length(pw)
            && (metrics.numeric >= self.min_numeric)
            && (metrics.uppercase >= self.min_uppercase)
            && (metrics.lowercase >= self.min_lowercase)
            && (metrics.special >= self.min_special_chars)
    }

    fn correct_length(&self, password: &str) -> bool {
        password.len() > (self.min_length - 1) && password.len() < (self.max_length + 1)
    }
}

struct PasswordMetrics {
    pub numeric: usize,
    pub special: usize,
    pub uppercase: usize,
    pub lowercase: usize,
}

impl PasswordMetrics {
    pub fn new(password: &str) -> Self {
        let mut numeric = 0;
        let mut special = 0;
        let mut uppercase = 0;
        let mut lowercase = 0;
        for ch in password.chars() {
            if ch.is_ascii_digit() {
                numeric += 1;
            }
            if ch.is_ascii_punctuation() {
                special += 1;
            }
            if ch.is_ascii_uppercase() {
                uppercase += 1;
            }
            if ch.is_ascii_lowercase() {
                lowercase += 1;
            }
        }
        PasswordMetrics {
            numeric,
            special,
            uppercase,
            lowercase,
        }
    }
}
