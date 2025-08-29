use wfmath::Box2;

use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::Frame;
use crate::core::Inputs;
use crate::core::Layout;
use crate::core::Wrap;
use crate::id;
use crate::widgets::Theme;
use crate::widgets::TooltipTheme;
use crate::widgets::tooltip;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ButtonTheme {
    pub border_color: u32,
    pub border_color_hovered: u32,
    pub border_color_active: u32,
    pub background_color: u32,
    pub background_color_hovered: u32,
    pub background_color_active: u32,
    pub text_color: u32,
    pub text_color_hovered: u32,
    pub text_color_active: u32,
    pub height: f32,
    pub margin: f32,
    pub border: f32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ButtonSettings<'a> {
    pub tooltip: &'a str,
    pub theme: &'a ButtonTheme,
    pub tooltip_theme: &'a TooltipTheme,
}

impl Default for ButtonSettings<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            tooltip: "",
            theme: &Theme::DEFAULT.button,
            tooltip_theme: &Theme::DEFAULT.tooltip,
        }
    }
}

#[macro_export]
macro_rules! button {
    ($frame:expr, $label:expr
     $(,tooltip = $tooltip:expr)?
     $(,theme = $theme:expr)?
     $(,tooltip_theme = $tooltip_theme:expr)?
     $(,)?
    ) => {
        $crate::button_ex($frame, id!(0), $label, $crate::ButtonSettings {
            $(tooltip: $tooltip,)?
            $(theme: $theme,)?
            $(tooltip_theme: $tooltip_theme,)?
            ..$crate::ButtonSettings::default()
        })
    };
}

pub fn button(frame: &mut Frame, id: CtrlId, label: &str) -> bool {
    button_ex(frame, id, label, ButtonSettings::default())
}

pub fn button_ex(frame: &mut Frame, id: CtrlId, label: &str, settings: ButtonSettings) -> bool {
    let parent_size = frame.ctrl_inner_size();
    let lmb_pressed = frame.inputs_pressed() == Inputs::MB_LEFT;
    let lmb_released = frame.inputs_released() == Inputs::MB_LEFT;

    let width = f32::max(0.0, parent_size.x - 2.0 * settings.theme.margin);
    let height = settings.theme.height;
    let border = settings.theme.border;
    let margin = settings.theme.margin;

    frame.push_ctrl(id);
    frame.ctrl_set_flags(CtrlFlags::CAPTURE_HOVER | CtrlFlags::CAPTURE_ACTIVE);
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, width, height));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(border);
    frame.ctrl_set_margin(margin);

    let hovered = frame.ctrl_is_hovered();
    let active = frame.ctrl_is_active();

    let (active, changed) = if active && lmb_released {
        frame.ctrl_set_active(false);
        if hovered {
            // Make the control inactive once again after release, as the
            // platform may not be running us on every frame, but only for
            // new events. Also better latency this way.
            (false, true)
        } else {
            (false, false)
        }
    } else if hovered && lmb_pressed {
        frame.ctrl_set_active(true);
        (true, false)
    } else {
        (active, false)
    };

    let (text_color, background_color, border_color) = match (hovered, active) {
        (false, false) => (
            settings.theme.text_color,
            settings.theme.background_color,
            settings.theme.border_color,
        ),
        (true, false) => (
            settings.theme.text_color_hovered,
            settings.theme.background_color_hovered,
            settings.theme.border_color_hovered,
        ),
        (_, true) => (
            settings.theme.text_color_active,
            settings.theme.background_color_active,
            settings.theme.border_color_active,
        ),
    };

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(border_color);
    frame.ctrl_set_draw_self_background_color(background_color);

    frame.ctrl_draw_text(label, Align::Center, Align::Center, Wrap::Word, text_color);

    if settings.tooltip.len() > 0 {
        if hovered {
            tooltip::tooltip_ex(frame, id!(0), settings.tooltip, settings.tooltip_theme);
        }
    }

    frame.pop_ctrl();

    changed
}
