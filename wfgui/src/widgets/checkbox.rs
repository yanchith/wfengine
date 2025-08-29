use wfmath::Box2;

use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::Frame;
use crate::core::Inputs;
use crate::core::Layout;
use crate::core::Wrap;
use crate::widgets::theme::Theme;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CheckboxTheme {
    pub handle_color: u32,
    pub handle_color_hovered: u32,
    pub handle_color_active: u32,
    pub handle_color_readonly: u32,
    pub text_color: u32,
    pub text_color_hovered: u32,
    pub text_color_active: u32,
    pub text_color_readonly: u32,
    pub width: f32,
    pub height: f32,
    pub margin: f32,
    pub border: f32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CheckboxSettings<'a> {
    pub readonly: bool,
    pub theme: &'a CheckboxTheme,
}

impl Default for CheckboxSettings<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            readonly: false,
            theme: &Theme::DEFAULT.checkbox,
        }
    }
}

#[inline]
pub fn checkbox(frame: &mut Frame, id: CtrlId, value: &mut bool, label: &str) -> bool {
    checkbox_ex(frame, id, value, label, CheckboxSettings::default())
}

pub fn checkbox_ex(frame: &mut Frame, id: CtrlId, value: &mut bool, label: &str, settings: CheckboxSettings) -> bool {
    let texture_id = frame.font_atlas_texture_id();
    let parent_size = frame.ctrl_inner_size();
    let lmb_pressed = frame.inputs_pressed() == Inputs::MB_LEFT;
    let lmb_released = frame.inputs_released() == Inputs::MB_LEFT;

    let width = f32::max(0.0, parent_size.x - 2.0 * settings.theme.margin);

    frame.push_ctrl(id);
    if !settings.readonly {
        frame.ctrl_set_flags(CtrlFlags::CAPTURE_HOVER | CtrlFlags::CAPTURE_ACTIVE);
    }
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, width, settings.theme.height));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(settings.theme.border);
    frame.ctrl_set_margin(settings.theme.margin);

    let hovered = frame.ctrl_is_hovered();
    let active = frame.ctrl_is_active();

    let (active, changed) = if settings.readonly {
        (active, false)
    } else if active && lmb_released {
        frame.ctrl_set_active(false);
        if hovered {
            // Make the control inactive once again after release, as the
            // platform may not be running us on every frame, but only for
            // new events. Also better latency this way.
            *value = !*value;
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

    let (handle_color, text_color) = if settings.readonly {
        (settings.theme.handle_color_readonly, settings.theme.text_color_readonly)
    } else if active {
        (settings.theme.handle_color_active, settings.theme.text_color_active)
    } else if hovered {
        (settings.theme.handle_color_hovered, settings.theme.text_color_hovered)
    } else {
        (settings.theme.handle_color, settings.theme.text_color)
    };

    const CHECKBOX_LEFT_PADDING: f32 = 5.0;
    const CHECKBOX_INNER_DIM: f32 = 12.0;
    const CHECKBOX_OUTER_DIM: f32 = 18.0;

    frame.ctrl_set_draw_self(false);
    frame.ctrl_draw_rect(
        Box2::new(
            CHECKBOX_LEFT_PADDING,
            0.5 * settings.theme.height - 0.5 * CHECKBOX_OUTER_DIM,
            CHECKBOX_OUTER_DIM,
            CHECKBOX_OUTER_DIM,
        ),
        Box2::ZERO,
        handle_color,
        texture_id,
    );

    if *value {
        frame.ctrl_draw_rect(
            Box2::new(
                CHECKBOX_LEFT_PADDING + 0.5 * (CHECKBOX_OUTER_DIM - CHECKBOX_INNER_DIM),
                0.5 * settings.theme.height - 0.5 * CHECKBOX_INNER_DIM,
                CHECKBOX_INNER_DIM,
                CHECKBOX_INNER_DIM,
            ),
            Box2::ZERO,
            0xffffffff,
            texture_id,
        );
    }

    frame.ctrl_draw_text_fitted(
        label,
        Align::Start,
        Align::Center,
        Wrap::Word,
        text_color,
        Box2::new(40.0, 0.0, f32::max(width - 40.0, 0.0), settings.theme.height),
    );

    frame.pop_ctrl();

    changed
}
