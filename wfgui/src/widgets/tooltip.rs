use wfmath::Box2;

use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::Frame;
use crate::core::Layout;
use crate::core::Wrap;
use crate::widgets::theme::Theme;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TooltipTheme {
    pub border_color: u32,
    pub background_color: u32,
    pub text_color: u32,
    pub border: f32,
    pub padding: f32,
}

#[inline]
pub fn tooltip(frame: &mut Frame, id: CtrlId, text: &str) {
    tooltip_ex(frame, id, text, &Theme::DEFAULT.tooltip)
}

pub fn tooltip_ex(frame: &mut Frame, id: CtrlId, text: &str, theme: &TooltipTheme) {
    frame.begin_overlay();

    let parent_size = frame.ctrl_inner_size();
    let cursor_position = frame.cursor_position();

    frame.push_ctrl(id);
    frame.ctrl_set_flags(CtrlFlags::RESIZE_TO_FIT_HORIZONTAL | CtrlFlags::RESIZE_TO_FIT_VERTICAL);
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(
        cursor_position.x,
        cursor_position.y,
        // Set to parent size so that the text layout can happen with realistic
        // clipping. This rect is however resized to fit the text during the
        // layout phase.
        f32::max(0.0, parent_size.x - cursor_position.x),
        parent_size.y,
    ));
    // Padding is not set, because there's no child controls, and the text
    // layout computes uses its own inset.
    frame.ctrl_set_border(theme.border);

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(theme.border_color);
    frame.ctrl_set_draw_self_background_color(theme.background_color);
    frame.ctrl_draw_text_inset_and_extend_content_rect(
        text,
        // Horizontal aligns don't make much sense with text tooltips.
        Align::Start,
        // Vertical align does not make sense with shrunk-to-fit controls.
        Align::Start,
        Wrap::Word,
        theme.text_color,
        theme.border + theme.padding,
    );

    frame.pop_ctrl();

    frame.end_overlay();
}
