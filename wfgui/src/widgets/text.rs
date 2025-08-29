use wfmath::Box2;

use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::Frame;
use crate::core::Layout;
use crate::core::Wrap;
use crate::widgets::theme::Theme;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextTheme {
    pub border_color: u32,
    pub background_color: u32,
    pub text_color: u32,
    pub margin: f32,
    pub border: f32,
    pub padding: f32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextSettings<'a> {
    pub align: Align,
    pub wrap: Wrap,
    pub theme: &'a TextTheme,
}

impl Default for TextSettings<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            align: Align::Start,
            wrap: Wrap::Word,
            theme: &Theme::DEFAULT.text,
        }
    }
}

#[macro_export]
macro_rules! text {
    ($frame:expr, $text:expr
     $(,align = $align:expr)?
     $(,wrap = $wrap:expr)?
     $(,theme = $theme:expr)?
     $(,)?
    ) => {
        $crate::text_ex($frame, $crate::id!(0), $text, $crate::TextSettings {
            $(align: $align,)?
            $(wrap: $wrap,)?
            $(theme: $theme,)?
            ..$crate::TextSettings::default()
        })
    };
}

#[inline]
pub fn text(frame: &mut Frame, id: CtrlId, text: &str) {
    text_ex(frame, id, text, TextSettings::default())
}

#[inline]
pub fn text_ex(frame: &mut Frame, id: CtrlId, text: &str, settings: TextSettings) {
    let parent_size = frame.ctrl_inner_size();

    frame.push_ctrl(id);

    frame.ctrl_set_flags(CtrlFlags::RESIZE_TO_FIT_HORIZONTAL | CtrlFlags::RESIZE_TO_FIT_VERTICAL);
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, parent_size.x, parent_size.y));

    // Padding is not set through the control, but applied with drawing,
    // because the text layout uses its own inset.
    frame.ctrl_set_border(settings.theme.border);
    frame.ctrl_set_margin(settings.theme.margin);

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(settings.theme.border_color);
    frame.ctrl_set_draw_self_background_color(settings.theme.background_color);
    frame.ctrl_draw_text_inset_and_extend_content_rect(
        text,
        settings.align,
        // Vertical align does not make sense with shrunk-to-fit controls.
        Align::Start,
        settings.wrap,
        settings.theme.text_color,
        settings.theme.border + settings.theme.padding,
    );

    frame.pop_ctrl();
}
