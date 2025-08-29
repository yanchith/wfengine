use wfmath::Box2;

use crate::core::CtrlFlags;
use crate::core::Frame;
use crate::core::Layout;
use crate::id;
use crate::widgets::theme::Theme;

#[inline]
pub fn separator(frame: &mut Frame, id: u32) {
    separator_with_theme(frame, id, &Theme::DEFAULT)
}

pub fn separator_with_theme(frame: &mut Frame, id: u32, theme: &Theme) {
    let parent_size = frame.ctrl_inner_size();

    let x = parent_size.x * 0.1 - theme.separator_margin;
    let width = f32::max(0.0, parent_size.x * 0.8 - theme.separator_margin);

    frame.push_ctrl(id!(id));
    frame.ctrl_set_flags(CtrlFlags::NONE);
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(x, 0.0, width, theme.separator_height));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(0.0);
    frame.ctrl_set_margin(theme.separator_margin);

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_background_color(theme.separator_color);

    frame.pop_ctrl();
}
