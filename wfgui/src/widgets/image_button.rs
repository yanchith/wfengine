use wfmath::Box2;

use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::Frame;
use crate::core::Inputs;
use crate::core::Layout;
use crate::id;
use crate::widgets::Theme;
use crate::widgets::TooltipTheme;
use crate::widgets::tooltip;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImageButtonTheme {
    pub border_color: u32,
    pub border_color_hovered: u32,
    pub border_color_active: u32,
    pub background_color: u32,
    pub background_color_hovered: u32,
    pub background_color_active: u32,
    pub margin: f32,
    pub border: f32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ImageButtonSettings<'a> {
    pub texture_rect: Option<Box2>,
    pub tooltip: &'a str,
    pub theme: &'a ImageButtonTheme,
    pub tooltip_theme: &'a TooltipTheme,
}

impl Default for ImageButtonSettings<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            texture_rect: None,
            tooltip: "",
            theme: &Theme::DEFAULT.image_button,
            tooltip_theme: &Theme::DEFAULT.tooltip,
        }
    }
}

#[macro_export]
macro_rules! image_button {
    ($frame:expr, $texture_id:expr, $width:expr, $height:expr
     $(,texture_rect = $texture_rect:expr)?
     $(,tooltip = $tooltip:expr)?
     $(,theme = $theme:expr)?
     $(,tooltip_theme = $tooltip_theme:expr)?
     $(,)?
    ) => {
        $crate::image_button_ex($frame, $crate::id!(0), $texture_id, $width, $height, $crate::ImageButtonSettings {
            $(texture_rect: Some($texture_rect),)?
            $(tooltip: $tooltip,)?
            $(theme: $theme,)?
            $(tooltip_theme: $tooltip_theme,)?
            ..$crate::ImageButtonSettings::default()
        })
    };
}

pub fn image_button(frame: &mut Frame, id: CtrlId, texture_id: u64, width: f32, height: f32) -> bool {
    image_button_ex(frame, id, texture_id, width, height, ImageButtonSettings::default())
}

pub fn image_button_ex(
    frame: &mut Frame,
    id: CtrlId,
    texture_id: u64,
    width: f32,
    height: f32,
    settings: ImageButtonSettings,
) -> bool {
    let lmb_pressed = frame.inputs_pressed() == Inputs::MB_LEFT;
    let lmb_released = frame.inputs_released() == Inputs::MB_LEFT;

    frame.push_ctrl(id);
    frame.ctrl_set_flags(CtrlFlags::CAPTURE_HOVER | CtrlFlags::CAPTURE_ACTIVE);
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, width, height));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(settings.theme.border);
    frame.ctrl_set_margin(settings.theme.margin);

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

    let (background_color, border_color) = match (hovered, active) {
        (false, false) => (settings.theme.background_color, settings.theme.border_color),
        (true, false) => (
            settings.theme.background_color_hovered,
            settings.theme.border_color_hovered,
        ),
        (_, true) => (
            settings.theme.background_color_active,
            settings.theme.border_color_active,
        ),
    };

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(border_color);
    frame.ctrl_set_draw_self_background_color(background_color);

    let texture_rect = settings.texture_rect.unwrap_or(Box2::ONE);
    frame.ctrl_draw_rect(Box2::new(0.0, 0.0, width, height), texture_rect, 0xffffffff, texture_id);

    if settings.tooltip.len() > 0 {
        if hovered {
            tooltip::tooltip_ex(frame, id!(0), settings.tooltip, settings.tooltip_theme);
        }
    }

    frame.pop_ctrl();

    changed
}
