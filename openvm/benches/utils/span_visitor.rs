use std::fmt::Display;

use tracing::field::{Field, Visit};

/// Extracts `id` and `air_name` fields from a span's metadata.
pub struct Visitor {
    pub id: Option<usize>,
    pub air_name: Option<String>,
}

impl Visit for Visitor {
    // Called for fields recorded as string literals (e.g. air_name).
    fn record_str(&mut self, field: &Field, value: &str) {
        if field.name() == "air_name" {
            self.air_name = Some(value.to_string());
        }
    }

    // Called when the macro passes a Debug-able value (e.g. id).
    fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
        if field.name() == "id" {
            // Debug formatting of a usize will be just the digits, so we can parse.
            if let Ok(n) = format!("{value:?}").parse::<usize>() {
                self.id = Some(n);
            }
        }
    }

    // In case it ever comes through as u64 or i64, handle those too
    fn record_u64(&mut self, field: &Field, value: u64) {
        if field.name() == "id" {
            self.id = Some(value as usize);
        }
    }
    fn record_i64(&mut self, field: &Field, value: i64) {
        if field.name() == "id" && value >= 0 {
            self.id = Some(value as usize);
        }
    }

    // We don’t care about the other types
    fn record_bool(&mut self, _field: &Field, _value: bool) {}
    fn record_f64(&mut self, _field: &Field, _value: f64) {}
}

impl Display for Visitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(id) = self.id {
            write!(f, "id:{id}")?;
        }
        if let Some(ref air_name) = self.air_name {
            if self.id.is_some() {
                write!(f, " ")?;
            }
            write!(f, "air:{air_name}")?;
        }
        Ok(())
    }
}
