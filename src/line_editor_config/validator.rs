use reedline::{ValidationResult, Validator};

pub struct RellogReplValidator;

impl Validator for RellogReplValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        ValidationResult::Complete
    }
}
