use arrayvec::ArrayString;
use wfcommon::cast_u32;
use wfmath::Box2;

use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::CtrlState;
use crate::core::Frame;
use crate::core::Inputs;
use crate::core::Layout;
use crate::core::TextStorage;
use crate::core::Wrap;
use crate::core::draw_text;
use crate::core::update_text;
use crate::id;
use crate::widgets::button::ButtonTheme;
use crate::widgets::button::button;
use crate::widgets::theme::Theme;

const LABEL_WIDTH_RATIO: f32 = 0.35;
const LABEL_SPACING: f32 = 5.0;

// TODO(yan): Migrate all other widgets to widget_ex and WidgetSettings to get
// rid of the combinatorial explosion of options.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextInputSettings<'a> {
    pub readonly: bool,
    pub autocomplete_candidates: &'a [&'a str],
    pub theme: &'a Theme,
    pub button_theme: &'a ButtonTheme,
}

impl Default for TextInputSettings<'_> {
    fn default() -> Self {
        Self {
            readonly: false,
            autocomplete_candidates: &[],
            theme: &Theme::DEFAULT,
            button_theme: &Theme::DEFAULT.button,
        }
    }
}

// TODO(yan): Even if this is already much better than the result callback,
// consider doing away with the action too and changing how text_input works as
// follows:
//
// When activated, copy the input into our own memory. Edit it there, but report
// no change. Mutate the input and report the change only upon confirmation.
//
// This is somewhat inspired by JAI's get_rect, which does or used to do
// something similar.
//
// To do this, we'll have to find a way get a huge chunk of memory (potentially
// large enough for any use) somewhere. get_rect used to use global malloc/free
// for this, but maybe we can have something that falls into our allocation
// patterns?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TextInputAction {
    None,
    Confirm,
    Cancel,
}

#[inline]
pub fn text_input(frame: &mut Frame, id: CtrlId, text: &mut dyn TextStorage, label: &str) -> (bool, TextInputAction) {
    text_input_ex(frame, id, text, label, TextInputSettings::default())
}

pub fn text_input_ex(
    frame: &mut Frame,
    id: CtrlId,
    text: &mut dyn TextStorage,
    label: &str,
    settings: TextInputSettings,
) -> (bool, TextInputAction) {
    let parent_size = frame.ctrl_inner_size();
    let cursor_position = frame.cursor_position();
    let inputs = frame.inputs_pressed();
    let modifiers = frame.modifiers();
    let received_characters: ArrayString<32> = ArrayString::from(frame.received_characters()).unwrap();
    let clipboard_getter = frame.get_clipboard_getter();
    let clipboard_setter = frame.get_clipboard_setter();

    let outer_width = f32::max(0.0, parent_size.x - 2.0 * settings.theme.text_input_margin);
    let label_width = LABEL_WIDTH_RATIO * outer_width;
    let inner_width = f32::max(0.0, outer_width - label_width - LABEL_SPACING);

    frame.push_ctrl(id);
    frame.ctrl_set_flags(CtrlFlags::NONE);
    frame.ctrl_set_layout(Layout::Horizontal);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, outer_width, settings.theme.text_input_height));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(0.0);
    frame.ctrl_set_margin(settings.theme.text_input_margin);

    frame.ctrl_set_draw_self(false);
    frame.ctrl_draw_text_fitted(
        label,
        Align::Start,
        Align::Center,
        Wrap::Word,
        settings.theme.text_input_text_color,
        Box2::new(0.0, 0.0, label_width, settings.theme.text_input_height),
    );

    frame.push_ctrl(id!(0));
    if !settings.readonly {
        frame.ctrl_set_flags(CtrlFlags::CAPTURE_SCROLL | CtrlFlags::CAPTURE_HOVER);
    }
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(
        label_width + LABEL_SPACING,
        0.0,
        inner_width,
        settings.theme.text_input_height,
    ));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(settings.theme.text_input_border);
    frame.ctrl_set_margin(0.0);

    let hovered = frame.ctrl_is_hovered();
    let mut active = frame.ctrl_is_active();

    let state = cast_state(frame.ctrl_state());
    let mut text_cursor = state.text_cursor;
    let mut text_selection_start = state.text_selection_start;
    let mut text_selection_end = state.text_selection_end;

    let mut deactivated_from_kb = false;

    let (changed, action) = if settings.readonly {
        (false, TextInputAction::None)
    } else if active {
        let changed = update_text(
            text,
            &mut text_cursor,
            &mut text_selection_start,
            &mut text_selection_end,
            inputs,
            modifiers,
            &received_characters,
            clipboard_getter,
            clipboard_setter,
        );

        let action = match inputs {
            Inputs::KB_ENTER => TextInputAction::Confirm,
            Inputs::KB_ESCAPE => TextInputAction::Cancel,
            _ => TextInputAction::None,
        };

        if action != TextInputAction::None {
            active = false;
            deactivated_from_kb = true;
        }

        (changed, action)
    } else if hovered && inputs == Inputs::MB_LEFT {
        frame.ctrl_set_active(true);
        active = true;

        text_cursor = text.len();
        text_selection_start = text_cursor;
        text_selection_end = text_cursor;

        (false, TextInputAction::None)
    } else {
        (false, TextInputAction::None)
    };

    let state = cast_state_mut(frame.ctrl_state_mut());
    state.text_cursor = text_cursor;
    state.text_selection_start = text_selection_start;
    state.text_selection_end = text_selection_end;

    let mut autocomplete_open = state.autocomplete_open;

    if active {
        state.autocomplete_open = AUTOCOMPLETE_OPEN;
        autocomplete_open = AUTOCOMPLETE_OPEN;
    }

    if deactivated_from_kb {
        state.autocomplete_open = AUTOCOMPLETE_CLOSED;
        autocomplete_open = AUTOCOMPLETE_CLOSED;
        frame.ctrl_set_active(false);
    }

    if active {
        frame.request_capture_keyboard();
    }

    let (text_color, background_color, border_color) = if settings.readonly {
        (
            settings.theme.text_input_text_color_readonly,
            settings.theme.text_input_background_color_readonly,
            settings.theme.text_input_border_color_readonly,
        )
    } else if active {
        (
            settings.theme.text_input_text_color_active,
            settings.theme.text_input_background_color_active,
            settings.theme.text_input_border_color_active,
        )
    } else if hovered {
        (
            settings.theme.text_input_text_color_hovered,
            settings.theme.text_input_background_color_hovered,
            settings.theme.text_input_border_color_hovered,
        )
    } else {
        (
            settings.theme.text_input_text_color,
            settings.theme.text_input_background_color,
            settings.theme.text_input_border_color,
        )
    };

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(border_color);
    frame.ctrl_set_draw_self_background_color(background_color);

    if active {
        draw_text(
            frame,
            text,
            text_cursor,
            text_selection_start,
            text_selection_end,
            Align::Center,
            Align::Center,
            text_color,
        );
    } else {
        frame.ctrl_draw_text(text, Align::Center, Align::Center, Wrap::None, text_color);
    }

    let mut changed_from_autocomplete = false;
    let mut overlay_contains_cursor = false;
    if autocomplete_open == AUTOCOMPLETE_OPEN && settings.autocomplete_candidates.len() > 0 {
        let absolute_position = frame.ctrl_absolute_position();

        frame.begin_overlay();

        // We configure parts this control only after we have computed the
        // number of autocomplete candidates we want to show, because we don't
        // know the size at this point.
        frame.push_ctrl(id);
        frame.ctrl_set_flags(CtrlFlags::CAPTURE_SCROLL | CtrlFlags::CAPTURE_HOVER);
        frame.ctrl_set_layout(Layout::Vertical);

        // Margin is zero, because we'll be setting an absolute position.
        frame.ctrl_set_padding(0.0);
        frame.ctrl_set_border(settings.theme.text_input_border);
        frame.ctrl_set_margin(0.0);

        let mut candidate_count = 0;
        for (i, candidate) in settings.autocomplete_candidates.iter().enumerate() {
            // TODO(yan): Improve string matching. We should do at least
            // contains_ignore_ascii_case, but also fuzzy matching,
            // e.g. Levenshtein, both of which we'd have to build ourselves.
            let t: &str = text.deref();
            if candidate.contains(t) || candidate.eq_ignore_ascii_case(t) {
                candidate_count += 1;

                if button(frame, id!(cast_u32(i)), candidate) {
                    text.truncate(0);
                    let _ = text.try_extend(candidate);

                    changed_from_autocomplete = true;
                }
            }
        }

        if candidate_count > 0 {
            let overlay_rect = {
                const OVERLAY_SPACING: f32 = 5.0;

                let window_size = frame.window_size();
                let overlay_y = absolute_position.y + settings.theme.text_input_height + OVERLAY_SPACING;

                let available_height_up = overlay_y;
                let available_height_down = f32::max(window_size.y - overlay_y, 0.0);

                let overlay_height_requested = f32::min(
                    candidate_count as f32 * (settings.button_theme.height + 2.0 * settings.button_theme.margin),
                    settings.theme.text_input_overlay_max_height,
                );

                if overlay_height_requested > available_height_down {
                    if available_height_down > available_height_up {
                        Box2::new(absolute_position.x, overlay_y, inner_width, available_height_down)
                    } else {
                        let height = f32::min(available_height_up, overlay_height_requested);
                        Box2::new(
                            absolute_position.x,
                            absolute_position.y - height - OVERLAY_SPACING,
                            inner_width,
                            height,
                        )
                    }
                } else {
                    Box2::new(absolute_position.x, overlay_y, inner_width, overlay_height_requested)
                }
            };

            overlay_contains_cursor = overlay_rect.contains_point(cursor_position);

            frame.ctrl_set_rect(overlay_rect);

            frame.ctrl_set_draw_self(true);
            frame.ctrl_set_draw_self_border_color(settings.theme.text_input_border_color);
            frame.ctrl_set_draw_self_background_color(settings.theme.text_input_background_color);
        } else {
            // Don't draw anything, if there's no child controls.
            frame.ctrl_set_rect(Box2::ZERO);

            frame.ctrl_set_draw_self(false);
            frame.ctrl_set_draw_self_border_color(0);
            frame.ctrl_set_draw_self_background_color(0);
        }

        frame.pop_ctrl();

        frame.end_overlay();
    }

    // TODO(yan): @Cleanup @Hack We have to track the open state of our
    // autocomplete dropdown manually, because we can't rely on us being active
    // after we render the overlay with the autcomplete choices, as those
    // buttons take away the focus from us the moment they are clicked. Is there
    // a better way?
    let clicked_outside_input_or_overlay = inputs == Inputs::MB_LEFT && !hovered && !overlay_contains_cursor;
    if changed_from_autocomplete || deactivated_from_kb || clicked_outside_input_or_overlay {
        let state = cast_state_mut(frame.ctrl_state_mut());
        state.autocomplete_open = AUTOCOMPLETE_CLOSED;

        // TODO(yan): @Cleanup A bit of borrowchecker hell disallows us from
        // doing this. Maybe merge Frame and Ctrl?
        //
        // inner_ctrl.set_active(false);
    }

    frame.pop_ctrl();
    frame.pop_ctrl();

    (changed || changed_from_autocomplete, action)
}

const AUTOCOMPLETE_CLOSED: u32 = 0;
const AUTOCOMPLETE_OPEN: u32 = 1;

#[repr(C)]
#[derive(Clone, Copy)]
#[derive(bytemuck::AnyBitPattern, bytemuck::NoUninit)]
struct State {
    // TODO(yan): @Cleanup Should these be platform sized? Padding calculations
    // change based on platform this way. We could do u16 or u32, but then we'd
    // have to truncate manually.
    text_cursor: usize,
    text_selection_start: usize,
    text_selection_end: usize,
    autocomplete_open: u32,
    _pad0: u32,
}

fn cast_state(state: &CtrlState) -> &State {
    bytemuck::from_bytes(&state.0[..size_of::<State>()])
}

fn cast_state_mut(state: &mut CtrlState) -> &mut State {
    bytemuck::from_bytes_mut(&mut state.0[..size_of::<State>()])
}
