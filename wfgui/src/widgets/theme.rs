use crate::widgets::ButtonTheme;
use crate::widgets::CheckboxTheme;
use crate::widgets::FloatSliderTheme;
use crate::widgets::ImageButtonTheme;
use crate::widgets::IntSliderTheme;
use crate::widgets::TextTheme;
use crate::widgets::TooltipTheme;
use crate::widgets::WindowTheme;

// TODO(jt): Values for margin, border and padding could be split into horizontal and vertical, or
// even per rect side, but only do that if it is actually useful as it otherwise takes a lot of
// space in the Ctrl struct.

// TODO(jt): Split theme into themes for each component, so that when the user wants to edit
// something in the theme, they don't have to copy the whole struct.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Theme {
    pub button: ButtonTheme,
    pub image_button: ImageButtonTheme,
    pub checkbox: CheckboxTheme,
    pub tooltip: TooltipTheme,
    pub int_slider: IntSliderTheme,
    pub float_slider: FloatSliderTheme,
    pub window: WindowTheme,
    pub text: TextTheme,

    pub text_input_border_color: u32,
    pub text_input_border_color_hovered: u32,
    pub text_input_border_color_active: u32,
    pub text_input_border_color_readonly: u32,
    pub text_input_background_color: u32,
    pub text_input_background_color_hovered: u32,
    pub text_input_background_color_active: u32,
    pub text_input_background_color_readonly: u32,
    pub text_input_text_color: u32,
    pub text_input_text_color_hovered: u32,
    pub text_input_text_color_active: u32,
    pub text_input_text_color_readonly: u32,
    pub text_input_height: f32,
    pub text_input_margin: f32,
    pub text_input_border: f32,
    pub text_input_overlay_max_height: f32,

    pub dropdown_border_color: u32,
    pub dropdown_border_color_hovered: u32,
    pub dropdown_border_color_active: u32,
    pub dropdown_background_color: u32,
    pub dropdown_background_color_hovered: u32,
    pub dropdown_background_color_active: u32,
    pub dropdown_text_color: u32,
    pub dropdown_text_color_hovered: u32,
    pub dropdown_text_color_active: u32,
    pub dropdown_height: f32,
    pub dropdown_margin: f32,
    pub dropdown_border: f32,
    pub dropdown_overlay_max_height: f32,

    pub panel_border_color: u32,
    pub panel_background_color: u32,
    pub panel_margin: f32,
    pub panel_border: f32,
    pub panel_padding: f32,
    pub panel_header_text_color: u32,
    pub panel_header_background_color: u32,
    pub panel_header_height: f32,

    pub separator_color: u32,
    pub separator_height: f32,
    pub separator_margin: f32,
}

const TRANSPARENT: u32 = 0xffffff00;

const WINDOW_BACKGROUND_COLOR: u32 = 0x404040ff;
const WINDOW_BORDER_COLOR: u32 = 0x606060ff;
const WINDOW_HEADER_BACKGROUND_COLOR: u32 = 0x7676deff;

const BORDER_COLOR: u32 = 0x606060ff;
const BORDER_COLOR_HOVERED: u32 = 0x808080ff;
const BORDER_COLOR_ACTIVE: u32 = 0xa0a0a0ff;
const BORDER_COLOR_READONLY: u32 = 0x404040ff;

const BACKGROUND_COLOR: u32 = 0x404040ff;
const BACKGROUND_COLOR_HOVERED: u32 = 0x808080ff;
const BACKGROUND_COLOR_ACTIVE: u32 = 0xa0a0a0ff;

const TEXT_COLOR: u32 = 0xe3e3e3ff;
const TEXT_COLOR_READONLY: u32 = 0xb8b8b8ff;
const TEXT_COLOR_HEADER: u32 = 0xf0f0f0ff;

impl Theme {
    pub const DEFAULT: Self = Self {
        button: ButtonTheme {
            border_color: BORDER_COLOR,
            border_color_hovered: BORDER_COLOR_HOVERED,
            border_color_active: BORDER_COLOR_ACTIVE,
            background_color: BACKGROUND_COLOR,
            background_color_hovered: BACKGROUND_COLOR_HOVERED,
            background_color_active: BACKGROUND_COLOR_ACTIVE,
            text_color: TEXT_COLOR,
            text_color_hovered: TEXT_COLOR,
            text_color_active: TEXT_COLOR,
            height: 30.0,
            margin: 2.0,
            border: 1.0,
        },

        image_button: ImageButtonTheme {
            border_color: BORDER_COLOR,
            border_color_hovered: BORDER_COLOR_HOVERED,
            border_color_active: BORDER_COLOR_ACTIVE,
            background_color: BACKGROUND_COLOR,
            background_color_hovered: BACKGROUND_COLOR_HOVERED,
            background_color_active: BACKGROUND_COLOR_ACTIVE,
            margin: 2.0,
            border: 1.0,
        },

        checkbox: CheckboxTheme {
            handle_color: BORDER_COLOR,
            handle_color_hovered: BORDER_COLOR_HOVERED,
            handle_color_active: BORDER_COLOR_ACTIVE,
            handle_color_readonly: BORDER_COLOR_READONLY,
            text_color: TEXT_COLOR,
            text_color_hovered: TEXT_COLOR,
            text_color_active: TEXT_COLOR,
            text_color_readonly: TEXT_COLOR_READONLY,
            width: 250.0,
            height: 30.0,
            margin: 2.0,
            border: 1.0,
        },

        tooltip: TooltipTheme {
            border_color: BORDER_COLOR,
            background_color: WINDOW_BACKGROUND_COLOR,
            text_color: TEXT_COLOR,
            border: 1.0,
            padding: 5.0,
        },

        int_slider: IntSliderTheme {
            border_color: BORDER_COLOR,
            border_color_hovered: BORDER_COLOR_HOVERED,
            border_color_active: BORDER_COLOR_ACTIVE,
            border_color_readonly: BORDER_COLOR_READONLY,
            background_color: BACKGROUND_COLOR,
            background_color_hovered: BACKGROUND_COLOR_HOVERED,
            background_color_active: BACKGROUND_COLOR_ACTIVE,
            background_color_readonly: BACKGROUND_COLOR,
            text_color: TEXT_COLOR,
            text_color_hovered: TEXT_COLOR,
            text_color_active: TEXT_COLOR,
            text_color_readonly: TEXT_COLOR_READONLY,
            height: 30.0,
            margin: 2.0,
            border: 1.0,
        },

        float_slider: FloatSliderTheme {
            border_color: BORDER_COLOR,
            border_color_hovered: BORDER_COLOR_HOVERED,
            border_color_active: BORDER_COLOR_ACTIVE,
            border_color_readonly: BORDER_COLOR_READONLY,
            background_color: BACKGROUND_COLOR,
            background_color_hovered: BACKGROUND_COLOR_HOVERED,
            background_color_active: BACKGROUND_COLOR_ACTIVE,
            background_color_readonly: BACKGROUND_COLOR,
            text_color: TEXT_COLOR,
            text_color_hovered: TEXT_COLOR,
            text_color_active: TEXT_COLOR,
            text_color_readonly: TEXT_COLOR_READONLY,
            height: 30.0,
            margin: 2.0,
            border: 1.0,
        },

        window: WindowTheme {
            border_color: BORDER_COLOR,
            border_color_hovered: WINDOW_BORDER_COLOR,
            background_color: WINDOW_BACKGROUND_COLOR,
            background_color_hovered: WINDOW_BACKGROUND_COLOR,
            border: 1.0,
            padding: 5.0,
        },

        text: TextTheme {
            border_color: TRANSPARENT,
            background_color: TRANSPARENT,
            text_color: TEXT_COLOR,
            margin: 0.0,
            border: 0.0,
            padding: 5.0,
        },

        text_input_border_color: BORDER_COLOR,
        text_input_border_color_hovered: BORDER_COLOR_HOVERED,
        text_input_border_color_active: BORDER_COLOR_ACTIVE,
        text_input_border_color_readonly: BORDER_COLOR_READONLY,
        text_input_background_color: BACKGROUND_COLOR,
        text_input_background_color_hovered: BACKGROUND_COLOR_HOVERED,
        text_input_background_color_active: BACKGROUND_COLOR_ACTIVE,
        text_input_background_color_readonly: BACKGROUND_COLOR,
        text_input_text_color: TEXT_COLOR,
        text_input_text_color_hovered: TEXT_COLOR,
        text_input_text_color_active: TEXT_COLOR,
        text_input_text_color_readonly: TEXT_COLOR_READONLY,
        text_input_height: 30.0,
        text_input_margin: 2.0,
        text_input_border: 1.0,
        text_input_overlay_max_height: 400.0,

        dropdown_border_color: BORDER_COLOR,
        dropdown_border_color_hovered: BORDER_COLOR_HOVERED,
        dropdown_border_color_active: BORDER_COLOR_ACTIVE,
        dropdown_background_color: BACKGROUND_COLOR,
        dropdown_background_color_hovered: BACKGROUND_COLOR_HOVERED,
        dropdown_background_color_active: BACKGROUND_COLOR_ACTIVE,
        dropdown_text_color: TEXT_COLOR,
        dropdown_text_color_hovered: TEXT_COLOR,
        dropdown_text_color_active: TEXT_COLOR,
        dropdown_height: 30.0,
        dropdown_margin: 2.0,
        dropdown_border: 1.0,
        dropdown_overlay_max_height: 400.0,

        panel_border_color: TRANSPARENT,
        panel_background_color: WINDOW_BACKGROUND_COLOR,
        panel_margin: 5.0,
        panel_border: 0.0,
        panel_padding: 5.0,
        panel_header_text_color: TEXT_COLOR_HEADER,
        panel_header_background_color: WINDOW_HEADER_BACKGROUND_COLOR,
        panel_header_height: 20.0,

        separator_color: BORDER_COLOR,
        separator_height: 1.0,
        separator_margin: 8.0,
    };
}
