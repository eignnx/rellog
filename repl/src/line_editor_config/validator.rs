use reedline::{DefaultValidator, ValidationResult, Validator};

use super::RellogReplConfigHandle;

impl Validator for RellogReplConfigHandle {
    fn validate(&self, text: &str) -> ValidationResult {
        match text.lines().last() {
            Some(l) if l.starts_with(['-', '|']) => ValidationResult::Incomplete,
            _ => DefaultValidator.validate(text),
        }
    }
}
