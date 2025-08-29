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
use crate::core::Modifiers;
use crate::core::Wrap;
use crate::core::draw_text;
use crate::core::update_text;
use crate::id;
use crate::widgets::button::ButtonSettings;
use crate::widgets::button::ButtonTheme;
use crate::widgets::button::button;
use crate::widgets::button::button_ex;
use crate::widgets::theme::Theme;

const LABEL_WIDTH_RATIO: f32 = 0.35;
const LABEL_SPACING: f32 = 5.0;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DropdownSettings<'a> {
    pub readonly: bool,
    pub activate_when_new: bool,
    pub enable_unselect: bool,
    pub enable_search: bool,
    pub theme: &'a Theme,
    pub button_theme: &'a ButtonTheme,
}

impl Default for DropdownSettings<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            readonly: false,
            activate_when_new: false,
            enable_unselect: false,
            enable_search: false,
            theme: &Theme::DEFAULT,
            button_theme: &Theme::DEFAULT.button,
        }
    }
}

#[inline]
pub fn dropdown(frame: &mut Frame, id: CtrlId, label: &str, options: &[&str], selected: &mut Option<usize>) -> bool {
    dropdown_ex(frame, id, label, options, selected, DropdownSettings::default())
}

pub fn dropdown_ex(
    frame: &mut Frame,
    id: CtrlId,
    label: &str,
    options: &[&str],
    selected: &mut Option<usize>,
    settings: DropdownSettings,
) -> bool {
    const OVERLAY_SPACING: f32 = 5.0;

    let parent_size = frame.ctrl_inner_size();
    let window_size = frame.window_size();
    let cursor_position = frame.cursor_position();
    let inputs = frame.inputs_pressed();
    let modifiers = frame.modifiers();
    let received_characters: ArrayString<32> = ArrayString::from(frame.received_characters()).unwrap();
    let clipboard_getter = frame.get_clipboard_getter();
    let clipboard_setter = frame.get_clipboard_setter();
    let outer_width = f32::max(0.0, parent_size.x - 2.0 * settings.theme.dropdown_margin);
    let label_width = LABEL_WIDTH_RATIO * outer_width;
    let inner_width = f32::max(0.0, outer_width - label_width - LABEL_SPACING);

    frame.push_ctrl(id);
    frame.ctrl_set_flags(CtrlFlags::NONE);
    frame.ctrl_set_layout(Layout::Horizontal);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, outer_width, settings.theme.dropdown_height));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(0.0);
    frame.ctrl_set_margin(settings.theme.dropdown_margin);

    frame.ctrl_set_draw_self(false);
    frame.ctrl_draw_text_fitted(
        label,
        Align::Start,
        Align::Center,
        Wrap::Word,
        settings.theme.dropdown_text_color,
        Box2::new(0.0, 0.0, label_width, settings.theme.dropdown_height),
    );

    frame.push_ctrl(id!(0));
    if !settings.readonly {
        frame.ctrl_set_flags(CtrlFlags::CAPTURE_HOVER | CtrlFlags::CAPTURE_ACTIVE);
    }
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(
        label_width + LABEL_SPACING,
        0.0,
        inner_width,
        settings.theme.dropdown_height,
    ));
    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(settings.theme.dropdown_border);
    frame.ctrl_set_margin(0.0);

    let absolute_position = frame.ctrl_absolute_position();

    let overlay_y = absolute_position.y + settings.theme.dropdown_height + OVERLAY_SPACING;

    let available_height_up = overlay_y;
    let available_height_down = f32::max(window_size.y - overlay_y, 0.0);

    let overlay_height_for_one = settings.button_theme.height + 2.0 * settings.button_theme.margin;
    let overlay_height_for_all = options.len() as f32 * overlay_height_for_one;
    let overlay_height_requested = f32::min(overlay_height_for_all, settings.theme.dropdown_overlay_max_height);

    let overlay_rect = if overlay_height_requested > available_height_down {
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
    };

    let hovered = frame.ctrl_is_hovered();
    let mut active = frame.ctrl_is_active();

    let state = cast_state_mut(frame.ctrl_state_mut());
    let mut open = state.open != 0;

    if open {
        if inputs == Inputs::KB_UP_ARROW || modifiers == Modifiers::CTRL && inputs == Inputs::KB_P {
            state.kb_selected_option_idx = state.kb_selected_option_idx.saturating_sub(1);
        }

        if inputs == Inputs::KB_DOWN_ARROW || modifiers == Modifiers::CTRL && inputs == Inputs::KB_N {
            state.kb_selected_option_idx = state.kb_selected_option_idx.saturating_add(1);
        }
    } else {
        state.kb_selected_option_idx = 0;
    }

    let displayed_options_count = if settings.enable_search {
        let t: &str = frame.ctrl_string();
        options
            .iter()
            .filter(|option| option.contains(t) || option.eq_ignore_ascii_case(t))
            .count()
    } else {
        options.len()
    };

    // Clamp the selected autocomplete option to the number we are actually
    // going to display.
    let state = cast_state_mut(frame.ctrl_state_mut());
    state.kb_selected_option_idx = usize::min(state.kb_selected_option_idx, displayed_options_count.saturating_sub(1));

    let kb_selected_option_idx = state.kb_selected_option_idx;

    if !settings.readonly && inputs == Inputs::MB_LEFT {
        if open {
            if !overlay_rect.contains_point(cursor_position) {
                state.open = 0;
                frame.ctrl_set_active(false);
                active = false;
                open = false;
            }
        } else if hovered {
            state.open = 1;
            frame.ctrl_set_active(true);
            active = true;
            open = true;
        }
    }

    let (text_color, background_color, border_color) = match (hovered, active) {
        (false, false) => (
            settings.theme.dropdown_text_color,
            settings.theme.dropdown_background_color,
            settings.theme.dropdown_border_color,
        ),
        (true, false) => (
            settings.theme.dropdown_text_color_hovered,
            settings.theme.dropdown_background_color_hovered,
            settings.theme.dropdown_border_color_hovered,
        ),
        (_, true) => (
            settings.theme.dropdown_text_color_active,
            settings.theme.dropdown_background_color_active,
            settings.theme.dropdown_border_color_active,
        ),
    };

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(border_color);
    frame.ctrl_set_draw_self_background_color(background_color);

    let selected_text = if let Some(selected) = selected {
        options[*selected]
    } else {
        ""
    };

    if settings.enable_search {
        if active {
            // TODO(jt): @Cleanup @Hack We are copying the string here (and copying it back at the
            // end of the block) to circumvent the borrowchecker, because we also need to borrow the
            // state. Find a better way!
            let mut search_buffer = *frame.ctrl_string();
            let state = cast_state_mut(frame.ctrl_state_mut());

            update_text(
                &mut search_buffer,
                &mut state.search_text_cursor,
                &mut state.search_text_selection_start,
                &mut state.search_text_selection_end,
                inputs,
                modifiers,
                &received_characters,
                clipboard_getter,
                clipboard_setter,
            );

            let text_cursor = state.search_text_cursor;
            let text_selection_start = state.search_text_selection_start;
            let text_selection_end = state.search_text_selection_end;

            draw_text(
                frame,
                &search_buffer,
                text_cursor,
                text_selection_start,
                text_selection_end,
                Align::Center,
                Align::Center,
                text_color,
            );

            *frame.ctrl_string_mut() = search_buffer;
        } else {
            frame.ctrl_draw_text(selected_text, Align::Center, Align::Center, Wrap::Word, text_color);
        }
    } else {
        frame.ctrl_draw_text(selected_text, Align::Center, Align::Center, Wrap::Word, text_color);
    }

    // TODO(jt): @Cleanup Activating here instead of immediately, so that we don't process input
    // (like inputs or received characters) that might have caused us to open. We are introducing a
    // frame of lag, though. Is there a better way?
    if !settings.readonly && settings.activate_when_new {
        if frame.ctrl_is_new() {
            cast_state_mut(frame.ctrl_state_mut()).open = 1;
            frame.ctrl_set_active(true);
        }
    }

    if inputs == Inputs::KB_ESCAPE {
        cast_state_mut(frame.ctrl_state_mut()).open = 0;
        frame.ctrl_set_active(false);
        active = false;
    }

    if active {
        frame.request_capture_keyboard();
    }

    let mut changed = false;

    if open {
        // TODO(yan): @Cleanup @Hack This is copied to circumvent the
        // borrowchecker. search_buffer is required for searching the text, but
        // at the same time we are drawing other UI stuff, which requires
        // mutable access to the context. We should have a way to split the
        // borrows.
        let search_buffer = *frame.ctrl_string();

        frame.begin_overlay();

        frame.push_ctrl(id);
        frame.ctrl_set_flags(CtrlFlags::CAPTURE_SCROLL | CtrlFlags::CAPTURE_HOVER);
        frame.ctrl_set_layout(Layout::Vertical);
        frame.ctrl_set_rect(overlay_rect);

        // Margin is zero, because we are setting an absolute position.
        frame.ctrl_set_padding(0.0);
        frame.ctrl_set_border(settings.theme.dropdown_border);
        frame.ctrl_set_margin(0.0);

        frame.ctrl_set_draw_self(true);
        frame.ctrl_set_draw_self_border_color(settings.theme.dropdown_border_color);
        frame.ctrl_set_draw_self_background_color(settings.theme.dropdown_background_color);

        let apply_overlay_scroll = inputs == Inputs::KB_UP_ARROW
            || inputs == Inputs::KB_DOWN_ARROW
            || modifiers == Modifiers::CTRL && inputs == Inputs::KB_P
            || modifiers == Modifiers::CTRL && inputs == Inputs::KB_N
            || received_characters.len() > 0;

        if apply_overlay_scroll {
            let mut overlay_scroll = frame.ctrl_scroll_offset_y();

            let overlay_height_for_displayed = displayed_options_count as f32 * overlay_height_for_one;
            if overlay_height_for_displayed > overlay_rect.height {
                let position = kb_selected_option_idx as f32 * overlay_height_for_one;
                let position_plus_height = position + overlay_height_for_one;

                const OVERLAY_SCROLL_RELATIVE_TO_HEIGHT: f32 = 0.25;

                if position < overlay_scroll {
                    overlay_scroll -= OVERLAY_SCROLL_RELATIVE_TO_HEIGHT * overlay_rect.height;
                }

                if position_plus_height > overlay_scroll + overlay_rect.height {
                    overlay_scroll += OVERLAY_SCROLL_RELATIVE_TO_HEIGHT * overlay_rect.height;
                }

                // Fallback if none of the above worked. This can happen when we
                // scroll with mouse and escape our happy little scroll window.

                if position < overlay_scroll {
                    overlay_scroll = position - OVERLAY_SCROLL_RELATIVE_TO_HEIGHT * overlay_rect.height;
                }

                if position_plus_height > overlay_scroll + overlay_rect.height {
                    overlay_scroll = position - (1.0 - OVERLAY_SCROLL_RELATIVE_TO_HEIGHT) * overlay_rect.height;
                }

                overlay_scroll = f32::clamp(
                    overlay_scroll,
                    0.0,
                    f32::max(overlay_height_for_displayed - overlay_rect.height, 0.0),
                );
            } else {
                overlay_scroll = 0.0;
            }

            frame.ctrl_set_scroll_offset_y(overlay_scroll);
        }

        if settings.enable_unselect {
            if button(frame, id!(0), "") {
                *selected = None;
                changed = true;
            }
        }

        let autocomplete_selected_button_theme = ButtonTheme {
            border_color: settings.button_theme.border_color_hovered,
            background_color: settings.button_theme.background_color_hovered,
            text_color: settings.button_theme.text_color_hovered,

            ..*settings.button_theme
        };

        if settings.enable_search {
            let mut displayed_option_idx = 0;
            for (i, option) in options.iter().enumerate() {
                // TODO(yan): Improve string matching. We should do at least
                // contains_ignore_ascii_case, but also fuzzy matching,
                // e.g. Levenshtein, both of which we'd have to build ourselves.
                let t: &str = &search_buffer;
                if option.contains(t) || option.eq_ignore_ascii_case(t) {
                    if displayed_option_idx == kb_selected_option_idx {
                        if button_ex(
                            frame,
                            id!(1 + cast_u32(i)),
                            option,
                            ButtonSettings {
                                theme: &autocomplete_selected_button_theme,
                                ..ButtonSettings::default()
                            },
                        ) || inputs == Inputs::KB_ENTER
                        {
                            *selected = Some(i);
                            changed = true;
                        }
                    } else if button(frame, id!(1 + cast_u32(i)), option) {
                        *selected = Some(i);
                        changed = true;
                    }

                    displayed_option_idx += 1;
                }
            }
        } else {
            for (i, option) in options.iter().enumerate() {
                if i == kb_selected_option_idx {
                    if button_ex(
                        frame,
                        id!(1 + cast_u32(i)),
                        option,
                        ButtonSettings {
                            theme: &autocomplete_selected_button_theme,
                            ..ButtonSettings::default()
                        },
                    ) || inputs == Inputs::KB_ENTER
                    {
                        *selected = Some(i);
                        changed = true;
                    }
                } else if button(frame, id!(1 + cast_u32(i)), option) {
                    *selected = Some(i);
                    changed = true;
                }
            }
        }

        frame.pop_ctrl();

        frame.end_overlay();
    }

    if changed {
        let state = cast_state_mut(frame.ctrl_state_mut());
        state.open = 0;

        if settings.enable_search {
            let search_buffer = frame.ctrl_string_mut();
            search_buffer.truncate(0);
        }
    }

    frame.pop_ctrl();
    frame.pop_ctrl();

    changed
}

#[repr(C)]
#[derive(Clone, Copy)]
#[derive(bytemuck::AnyBitPattern, bytemuck::NoUninit)]
struct State {
    // TODO(yan): @Cleanup Should these be platform sized? Padding calculations
    // change based on platform this way. We could do u16 or u32, but then we'd
    // have to truncate manually.
    search_text_cursor: usize,
    search_text_selection_start: usize,
    search_text_selection_end: usize,
    kb_selected_option_idx: usize,
    open: u8,
    _pad0: u8,
    _pad1: u16,
    _pad2: u32,
}

fn cast_state_mut(state: &mut CtrlState) -> &mut State {
    bytemuck::from_bytes_mut(&mut state.0[..size_of::<State>()])
}
