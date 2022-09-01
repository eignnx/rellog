use std::borrow::Cow;

use reedline::{DefaultPrompt, Prompt, PromptEditMode, PromptHistorySearch, PromptViMode};

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
        "".into()
    }

    fn render_prompt_right(&self) -> Cow<str> {
        "".into()
    }

    fn render_prompt_indicator(&self, edit_mode: PromptEditMode) -> Cow<str> {
        // self.delegate.render_prompt_indicator(edit_mode)
        match edit_mode {
            PromptEditMode::Default => "-- ".into(),
            PromptEditMode::Emacs => "(emacs)-- ".into(),
            PromptEditMode::Vi(PromptViMode::Normal) => "(vi:N)-- ".into(),
            PromptEditMode::Vi(PromptViMode::Insert) => "-- ".into(),
            PromptEditMode::Custom(c) => format!("{c}").into(),
        }
    }

    fn render_prompt_multiline_indicator(&self) -> Cow<str> {
        ".. ".into()
    }

    fn render_prompt_history_search_indicator(
        &self,
        history_search: PromptHistorySearch,
    ) -> Cow<str> {
        self.delegate
            .render_prompt_history_search_indicator(history_search)
    }
}
