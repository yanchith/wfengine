use core::fmt::Write;
use core::slice;
use core::str::FromStr;

use arrayvec::ArrayString;
use wfcommon::cast_u32;
use wfmath::Box2;
use wftime::Nanos;

use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::CtrlState;
use crate::core::Frame;
use crate::core::Inputs;
use crate::core::Layout;
use crate::core::Wrap;
use crate::core::draw_text;
use crate::core::update_text;
use crate::id;
use crate::widgets::theme::Theme;

const DOUBLE_CLICK_THRESHOLD: Nanos = Nanos::from_millis(250);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntSliderTheme {
    pub border_color: u32,
    pub border_color_hovered: u32,
    pub border_color_active: u32,
    pub border_color_readonly: u32,
    pub background_color: u32,
    pub background_color_hovered: u32,
    pub background_color_active: u32,
    pub background_color_readonly: u32,
    pub text_color: u32,
    pub text_color_hovered: u32,
    pub text_color_active: u32,
    pub text_color_readonly: u32,
    pub height: f32,
    pub margin: f32,
    pub border: f32,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IntSliderSettings<'a> {
    pub readonly: bool,
    pub speed: f32,
    pub min: i32,
    pub max: i32,
    pub show_text_input_on_double_click: bool,
    pub theme: &'a IntSliderTheme,
}

impl Default for IntSliderSettings<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            readonly: false,
            speed: 1.0,
            min: i32::MIN,
            max: i32::MAX,
            show_text_input_on_double_click: true,
            theme: &Theme::DEFAULT.int_slider,
        }
    }
}

#[inline]
pub fn int_slider(frame: &mut Frame, id: CtrlId, value: &mut i32, label: &str) -> bool {
    do_int_slider(frame, id, slice::from_mut(value), label, IntSliderSettings::default())
}

#[inline]
pub fn int_slider_ex(frame: &mut Frame, id: CtrlId, value: &mut i32, label: &str, settings: IntSliderSettings) -> bool {
    do_int_slider(frame, id, slice::from_mut(value), label, settings)
}

#[inline]
pub fn int2_slider(frame: &mut Frame, id: CtrlId, value: &mut [i32; 2], label: &str) -> bool {
    do_int_slider(frame, id, value, label, IntSliderSettings::default())
}

#[inline]
pub fn int2_slider_ex(
    frame: &mut Frame,
    id: CtrlId,
    value: &mut [i32; 2],
    label: &str,
    settings: IntSliderSettings,
) -> bool {
    do_int_slider(frame, id, value, label, settings)
}

#[inline]
pub fn int3_slider(frame: &mut Frame, id: CtrlId, value: &mut [i32; 3], label: &str) -> bool {
    do_int_slider(frame, id, value, label, IntSliderSettings::default())
}

#[inline]
pub fn int3_slider_ex(
    frame: &mut Frame,
    id: CtrlId,
    value: &mut [i32; 3],
    label: &str,
    settings: IntSliderSettings,
) -> bool {
    do_int_slider(frame, id, value, label, settings)
}

#[inline]
pub fn int4_slider(frame: &mut Frame, id: CtrlId, value: &mut [i32; 4], label: &str) -> bool {
    do_int_slider(frame, id, value, label, IntSliderSettings::default())
}

#[inline]
pub fn int4_slider_ex(
    frame: &mut Frame,
    id: CtrlId,
    value: &mut [i32; 4],
    label: &str,
    settings: IntSliderSettings,
) -> bool {
    do_int_slider(frame, id, value, label, settings)
}

fn do_int_slider(
    frame: &mut Frame,
    id: CtrlId,
    value_mut: &mut [i32],
    label: &str,
    settings: IntSliderSettings,
) -> bool {
    const LABEL_WIDTH_RATIO: f32 = 0.35;
    const LABEL_SPACING: f32 = 5.0;
    const INPUT_SPACING: f32 = 2.0;

    let mut s: ArrayString<256> = ArrayString::new();

    let parent_size = frame.ctrl_inner_size();
    let cursor_position = frame.cursor_position();
    let inputs_pressed = frame.inputs_pressed();
    let inputs_released = frame.inputs_released();
    let modifiers = frame.modifiers();
    let received_characters: ArrayString<32> = ArrayString::from(frame.received_characters()).unwrap();
    let clipboard_getter = frame.get_clipboard_getter();
    let clipboard_setter = frame.get_clipboard_setter();
    let current_frame_time = frame.current_frame_time();

    let len = value_mut.len() as f32;
    let width = f32::max(0.0, parent_size.x - 2.0 * settings.theme.margin);
    let label_width = LABEL_WIDTH_RATIO * width;
    let inner_width = f32::max(
        0.0,
        (width - label_width - LABEL_SPACING - INPUT_SPACING * (len - 1.0)) / len,
    );

    frame.push_ctrl(id);
    frame.ctrl_set_flags(CtrlFlags::NONE);
    // TODO(yan): There's a TODO in ui layout that will allow us to put
    // horizontal layout here, but for now we do the layout by ourselves and
    // position both inner controls manually.
    frame.ctrl_set_layout(Layout::Free);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, width, settings.theme.height));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(0.0);
    frame.ctrl_set_margin(settings.theme.margin);

    frame.ctrl_set_draw_self(false);
    frame.ctrl_draw_text_fitted(
        label,
        Align::Start,
        Align::Center,
        Wrap::Word,
        settings.theme.text_color,
        Box2::new(0.0, 0.0, label_width, settings.theme.height),
    );

    let mut changed = false;
    for (i, value_mut_slot) in value_mut.iter_mut().enumerate() {
        frame.push_ctrl(id!(cast_u32(i)));
        if !settings.readonly {
            frame.ctrl_set_flags(CtrlFlags::CAPTURE_HOVER);
        }
        frame.ctrl_set_layout(Layout::Vertical);
        frame.ctrl_set_rect(Box2::new(
            label_width + LABEL_SPACING + (inner_width + INPUT_SPACING) * i as f32,
            0.0,
            inner_width,
            settings.theme.height,
        ));
        frame.ctrl_set_padding(0.0);
        frame.ctrl_set_border(settings.theme.border);
        frame.ctrl_set_margin(0.0);

        let hovered = frame.ctrl_is_hovered();
        let active = frame.ctrl_is_active();
        let state = cast_state(frame.ctrl_state());

        let (active, changed_i, double_clicked) = if settings.readonly {
            (active, false, false)
        } else if active {
            if state.show_text_input == 0 {
                let value = state.value;
                let x = state.x;
                let delta = cursor_position.x - x;

                let new_active = if inputs_released == Inputs::MB_LEFT {
                    frame.ctrl_set_active(false);
                    false
                } else {
                    true
                };

                let old_value = *value_mut_slot;
                let new_value = i32::clamp(
                    libm::roundf(value as f32 + delta * settings.speed) as i32,
                    settings.min,
                    settings.max,
                );

                *value_mut_slot = new_value;
                (new_active, old_value != new_value, false)
            } else {
                (active, false, false)
            }
        } else if hovered && inputs_pressed == Inputs::MB_LEFT {
            frame.ctrl_set_active(true);

            let state = cast_state_mut(frame.ctrl_state_mut());

            let time_since_last_click = current_frame_time - state.last_click_time;
            let double_clicked = inputs_pressed == Inputs::MB_LEFT && time_since_last_click <= DOUBLE_CLICK_THRESHOLD;

            state.x = cursor_position.x;
            state.value = *value_mut_slot;
            state.last_click_time = current_frame_time;
            state.show_text_input = 0;

            (true, false, double_clicked)
        } else {
            let state = cast_state_mut(frame.ctrl_state_mut());
            state.show_text_input = 0;

            (active, false, false)
        };

        if active {
            frame.request_capture_keyboard();
        }

        changed |= changed_i;

        let (text_color, background_color, border_color) = if settings.readonly {
            (
                settings.theme.text_color_readonly,
                settings.theme.background_color_readonly,
                settings.theme.border_color_readonly,
            )
        } else if active {
            (
                settings.theme.text_color_active,
                settings.theme.background_color_active,
                settings.theme.border_color_active,
            )
        } else if hovered {
            (
                settings.theme.text_color_hovered,
                settings.theme.background_color_hovered,
                settings.theme.border_color_hovered,
            )
        } else {
            (
                settings.theme.text_color,
                settings.theme.background_color,
                settings.theme.border_color,
            )
        };

        frame.ctrl_set_draw_self(true);
        frame.ctrl_set_draw_self_border_color(border_color);
        frame.ctrl_set_draw_self_background_color(background_color);

        if settings.show_text_input_on_double_click && double_clicked {
            let state = cast_state_mut(frame.ctrl_state_mut());

            state.text_cursor = 0;
            state.text_selection_start = 0;
            state.text_selection_end = 0;
            state.show_text_input = 1;

            let string = frame.ctrl_string_mut();
            string.clear();
            let _ = write!(string, "{value_mut_slot}");
        }

        let state = cast_state_mut(frame.ctrl_state_mut());
        let mut deactivated_from_kb = false;
        let mut confirmed_from_kb = false;
        if state.show_text_input > 0 {
            if inputs_pressed == Inputs::KB_ESCAPE || inputs_pressed == Inputs::KB_ENTER {
                state.text_cursor = 0;
                state.text_selection_start = 0;
                state.text_selection_end = 0;
                state.show_text_input = 0;

                deactivated_from_kb = true;
            }

            if inputs_pressed == Inputs::KB_ENTER {
                confirmed_from_kb = true;
            }
        }

        if state.show_text_input > 0 {
            // TODO(jt): @Cleanup @Hack We are copying the string here (and copying it back later)
            // to circumvent the borrowchecker, because we also need to borrow the state. Find a
            // better way!
            let mut string = *frame.ctrl_string();
            let state = cast_state_mut(frame.ctrl_state_mut());

            update_text(
                &mut string,
                &mut state.text_cursor,
                &mut state.text_selection_start,
                &mut state.text_selection_end,
                inputs_pressed,
                modifiers,
                &received_characters,
                clipboard_getter,
                clipboard_setter,
            );

            let text_cursor = state.text_cursor;
            let text_selection_start = state.text_selection_start;
            let text_selection_end = state.text_selection_end;

            draw_text(
                frame,
                &string,
                text_cursor,
                text_selection_start,
                text_selection_end,
                Align::Center,
                Align::Center,
                text_color,
            );

            *frame.ctrl_string_mut() = string;
        } else {
            s.clear();
            let _ = write!(s, "{value_mut_slot}");
            frame.ctrl_draw_text(&s, Align::Center, Align::Center, Wrap::Word, text_color);
        }

        if confirmed_from_kb {
            let string = frame.ctrl_string();
            match i32::from_str(string) {
                Ok(mut new_value) => {
                    new_value = i32::clamp(new_value, settings.min, settings.max);

                    if *value_mut_slot != new_value {
                        *value_mut_slot = new_value;
                        changed = true;
                    }
                }
                Err(_) => (),
            }
        }

        if deactivated_from_kb {
            frame.ctrl_set_active(false);
        }

        frame.pop_ctrl();
    }

    frame.pop_ctrl();

    changed
}

#[repr(C)]
#[derive(Clone, Copy)]
#[derive(bytemuck::AnyBitPattern, bytemuck::NoUninit)]
struct State {
    // TODO(jt): Don't use usize for indices, as it might break compilation on targets with
    // different align for usize.
    text_cursor: usize,
    text_selection_start: usize,
    text_selection_end: usize,
    x: f32,
    value: i32,
    last_click_time: Nanos,
    show_text_input: u8,
    _pad0: u8,
    _pad1: u16,
    _pad2: u32,
    _pad3: u64,
}

fn cast_state(state: &CtrlState) -> &State {
    bytemuck::from_bytes(&state.0[..size_of::<State>()])
}

fn cast_state_mut(state: &mut CtrlState) -> &mut State {
    bytemuck::from_bytes_mut(&mut state.0[..size_of::<State>()])
}
