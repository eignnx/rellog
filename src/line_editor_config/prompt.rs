use std::borrow::Cow;

use reedline::{DefaultPrompt, Prompt, PromptEditMode, PromptHistorySearch};

pub struct RellogPrompt {
    delegate: DefaultPrompt,
}

impl RellogPrompt {
    pub const fn new() -> Self {
        Self {
            delegate: DefaultPrompt,
        }
    }
}

impl Default for RellogPrompt {
    fn default() -> Self {
        Self::new()
    }
}

impl Prompt for RellogPrompt {
    fn render_prompt_left(&self) -> Cow<str> {
        self.delegate.render_prompt_left()
    }

    fn render_prompt_right(&self) -> Cow<str> {
        self.delegate.render_prompt_right()
    }

    fn render_prompt_indicator(&self, edit_mode: PromptEditMode) -> Cow<str> {
        self.delegate.render_prompt_indicator(edit_mode)
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        self.delegate.render_prompt_multiline_indicator()
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<str> {
        self.delegate
            .render_prompt_history_search_indicator(history_search)
    }
}
