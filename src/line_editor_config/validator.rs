use reedline::{DefaultValidator, ValidationResult, Validator};

pub struct RellogReplValidator;

impl Validator for RellogReplValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        DefaultValidator.validate(line)
    }
}
