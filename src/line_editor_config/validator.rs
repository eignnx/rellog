use reedline::{DefaultValidator, ValidationResult, Validator};

use super::RellogReplConfigHandle;

impl Validator for RellogReplConfigHandle {
    fn validate(&self, line: &str) -> ValidationResult {
        DefaultValidator.validate(line)
    }
}
