use alloc::string::String;
use core::mem;

use wfinlinevec::InlineVec;
use wfmath::Box2;
use wfmath::Vec2;
use wfmath::vec2;

use crate::core::Align;
use crate::core::Frame;
use crate::core::Inputs;
use crate::core::Modifiers;
use crate::core::TextStorage;

pub fn update_text(
    // This is dyn so that we don't force our users to be generic if they can't
    // afford to be, e.g. because they'd have to complicate their API to get rid
    // of inference failures. This shouldn't be perf sensitive, so it's fine?
    text: &mut dyn TextStorage,
    text_cursor: &mut usize,
    text_selection_start: &mut usize,
    text_selection_end: &mut usize,
    inputs: Inputs,
    modifiers: Modifiers,
    received_characters: &str,
    get_clipboard: fn() -> String,
    set_clipboard: fn(&str),
) -> bool {
    let mut cursor = usize::min(*text_cursor, text.len());
    let mut selection_start = usize::min(*text_selection_start, text.len());
    let mut selection_end = usize::min(*text_selection_end, text.len());

    let mut modified = false;

    // TODO(yan): @Correctness This structure of handling inputs is lossy. For
    // instance, if we get a Ctrl+V AND additional received characters (either
    // preceeding or succeeding it), we drop those characters. The correct way
    // would be to interpret received characters in a loop, identifying control
    // sequences within.
    if received_characters.len() > 0 || inputs != Inputs::NONE {
        let mut handled = false;

        match inputs {
            Inputs::KB_BACKSPACE => {
                if text.len() > 0 {
                    let start = usize::min(selection_start, selection_end);
                    let end = usize::max(selection_start, selection_end);

                    if start != end {
                        // Ok to unwrap, because we are only removing.
                        text.try_splice(start, end - start, "").unwrap();

                        cursor = start;
                        selection_start = start;
                        selection_end = start;
                    } else if cursor == text.len() {
                        let cursor_after_trunc = seek_prev(cursor, text);

                        text.truncate(cursor_after_trunc);

                        cursor = cursor_after_trunc;
                        selection_start = cursor;
                        selection_end = cursor;
                    } else if cursor > 0 {
                        let cursor_after = seek_prev(cursor, text);
                        let delete_count = cursor - cursor_after;

                        // Ok to unwrap, because we are only removing.
                        text.try_splice(cursor_after, delete_count, "").unwrap();

                        cursor = cursor_after;
                        selection_start = cursor;
                        selection_end = cursor;
                    }

                    modified = true;
                }

                // Always consider backspace handled, because we don't want to
                // insert the weird character it corresponds to.
                handled = true;
            }

            Inputs::KB_DELETE => {
                if text.len() > 0 {
                    let last_char_index = seek_prev(text.len(), text);

                    if selection_start != selection_end {
                        let start = usize::min(selection_start, selection_end);
                        let end = usize::max(selection_start, selection_end);

                        // Ok to unwrap, because we are only removing.
                        text.try_splice(start, end - start, "").unwrap();

                        cursor = start;
                        selection_start = cursor;
                        selection_end = cursor;
                    } else if cursor == last_char_index {
                        text.truncate(last_char_index);

                        selection_start = cursor;
                        selection_end = cursor;
                    } else if cursor < last_char_index {
                        let delete_count = seek_next(cursor, text) - cursor;

                        // Ok to unwrap, because we are only removing.
                        text.try_splice(cursor, delete_count, "").unwrap();

                        selection_start = cursor;
                        selection_end = cursor;
                    }

                    modified = true;
                }

                // Always consider delete handled, because we don't want to
                // insert the weird character it corresponds to.
                handled = true;
            }

            Inputs::KB_A => {
                if modifiers == Modifiers::CTRL {
                    cursor = 0;
                    selection_start = 0;
                    selection_end = text.len();

                    handled = true;
                }
            }

            Inputs::KB_LEFT_ARROW => {
                cursor = seek_prev(cursor, text);
                selection_end = cursor;
                if !modifiers.intersects(Modifiers::SHIFT) {
                    selection_start = cursor;
                }

                handled = true;
            }

            Inputs::KB_B => {
                if modifiers == Modifiers::CTRL {
                    cursor = seek_prev(cursor, text);
                    selection_start = cursor;
                    selection_end = cursor;

                    handled = true;
                } else if modifiers == Modifiers::CTRL | Modifiers::SHIFT {
                    cursor = seek_prev(cursor, text);
                    selection_end = cursor;

                    handled = true;
                }
            }

            Inputs::KB_RIGHT_ARROW => {
                cursor = seek_next(cursor, text);
                selection_end = cursor;
                if !modifiers.intersects(Modifiers::SHIFT) {
                    selection_start = cursor;
                }

                handled = true;
            }

            Inputs::KB_F => {
                if modifiers == Modifiers::CTRL {
                    cursor = seek_next(cursor, text);
                    selection_start = cursor;
                    selection_end = cursor;

                    handled = true;
                } else if modifiers == Modifiers::CTRL | Modifiers::SHIFT {
                    cursor = seek_next(cursor, text);
                    selection_end = cursor;

                    handled = true;
                }
            }

            Inputs::KB_UP_ARROW => {
                // TODO(yan): Multiline text inputs
                handled = true;
            }

            Inputs::KB_P => {
                if modifiers == Modifiers::CTRL {
                    // TODO(yan): Multiline text inputs
                    handled = true;
                }
            }

            Inputs::KB_DOWN_ARROW => {
                // TODO(yan): Multiline text inputs
                handled = true;
            }

            Inputs::KB_N => {
                if modifiers == Modifiers::CTRL {
                    // TODO(yan): Multiline text inputs
                    handled = true;
                }
            }

            Inputs::KB_X => {
                if modifiers == Modifiers::CTRL {
                    if selection_start != selection_end {
                        let start = usize::min(selection_start, selection_end);
                        let end = usize::max(selection_start, selection_end);

                        let s = &text[start..end];
                        set_clipboard(s);

                        text.try_splice(start, end - start, "").unwrap();

                        cursor = start;
                        selection_start = cursor;
                        selection_end = cursor;

                        modified = true;
                    }

                    handled = true;
                }
            }

            Inputs::KB_C => {
                if modifiers == Modifiers::CTRL {
                    if selection_start != selection_end {
                        let start = usize::min(selection_start, selection_end);
                        let end = usize::max(selection_start, selection_end);

                        let s = &text[start..end];
                        set_clipboard(s);
                    }

                    handled = true;
                }
            }

            Inputs::KB_V => {
                if modifiers == Modifiers::CTRL {
                    // start and end can be the same index here, in which
                    // case the splice will not remove anything, only insert
                    // stuff from the clipboard. If they are not the same,
                    // the selected text gets replaced.
                    let start = usize::min(selection_start, selection_end);
                    let end = usize::max(selection_start, selection_end);

                    let s = get_clipboard();
                    let _ = text.try_splice(start, end - start, &s);

                    cursor += s.len();
                    selection_start = cursor;
                    selection_end = cursor;

                    handled = true;
                    modified = true;
                }
            }

            Inputs::KB_ENTER => {
                // This is considered handled and we don't enter any text, but
                // the component that calls us has to also catch this KB_ENTER,
                // if it wants to do something with it, e.g. text input
                // confirmation.
                //
                // TODO(yan): Implement multiline mode, where we don't set the
                // handled flag (unless we receive SHIFT+ENTER or CTRL+ENTER, or
                // something).
                handled = true;
            }

            Inputs::KB_ESCAPE => {
                handled = true;
            }

            _ => (),
        }

        if !handled {
            if selection_start != selection_end {
                let start = usize::min(selection_start, selection_end);
                let end = usize::max(selection_start, selection_end);

                let _ = text.try_splice(start, end - start, received_characters);

                cursor = start + received_characters.len();
                selection_start = cursor;
                selection_end = cursor;
            } else if cursor == text.len() {
                let _ = text.try_extend(received_characters);

                cursor = text.len();
                selection_start = cursor;
                selection_end = cursor;
            } else {
                let _ = text.try_splice(cursor, 0, received_characters);

                cursor += received_characters.len();
                selection_start = cursor;
                selection_end = cursor;
            }

            modified = true;
        }
    }

    *text_cursor = cursor;
    *text_selection_start = selection_start;
    *text_selection_end = selection_end;

    modified
}

// This is a modified text drawing routine from ui.rs. It doesn't handle
// word-wrapping and trimming, but can instead draw the cursor, text selection,
// handle horizontal and vertical scrolling within the text input, etc.
//
// TODO(yan): Handle word-wrapping and anything we need to make this able to
// handle multiline text inputs on demand.
pub fn draw_text(
    frame: &mut Frame,
    text: &str,
    text_cursor: usize,
    mut text_selection_start: usize,
    mut text_selection_end: usize,
    halign: Align,
    valign: Align,
    color: u32,
) {
    if text_selection_start > text_selection_end {
        mem::swap(&mut text_selection_start, &mut text_selection_end);
    }

    let available_size = frame.ctrl_inner_size();
    let available_width = available_size.x;
    let available_height = available_size.y;

    let font_atlas = frame.font_atlas();
    let font_atlas_texture_id = frame.font_atlas_texture_id();
    let font_size = font_atlas.font_size();

    #[derive(Clone, Copy)]
    struct Line {
        idx_start: usize,
        idx_end: usize,
        width: f32,
    }

    // TODO(jt): @Correctness This should take a scratch Arena instead of being stack-allocated, but
    // we currently can't easily borrow it from Frame, because we need to call it mutably.
    let mut lines: InlineVec<Line, 1024> = InlineVec::new();
    let mut line_idx_start = 0;
    let mut line_idx_end = 0;
    let mut line_width = 0.0;

    for (i, c) in text.char_indices() {
        if c == '\n' && line_idx_end > line_idx_start {
            // Note that this could be an empty line, but that's fine.
            lines.push(Line {
                idx_start: line_idx_start,
                idx_end: line_idx_end,
                width: line_width,
            });

            // 1 is the byte width of the '\n', so i + 1 is ok.
            line_idx_start = i + 1;
            line_idx_end = i + 1;
            line_width = 0.0;

            continue;
        }

        let glyph_info = font_atlas.glyph_info(c);
        let glyph_advance_width = glyph_info.advance_width;

        line_idx_end += c.len_utf8();
        line_width += glyph_advance_width;
    }

    lines.push(Line {
        idx_start: line_idx_start,
        idx_end: line_idx_end,
        width: line_width,
    });

    //
    // Emit rects based on generated line data.
    //
    let line_metrics = font_atlas.font_horizontal_line_metrics();

    let mut position_x = 0.0;
    let mut position_y = if lines.len() as f32 * line_metrics.new_line_size < available_height {
        match valign {
            Align::Start => line_metrics.line_gap,
            Align::Center => {
                let line_gap = line_metrics.line_gap;
                let new_line_size = line_metrics.new_line_size;
                let text_block_size = new_line_size * lines.len() as f32 - line_gap;

                line_gap + (available_height - text_block_size) / 2.0
            }
            Align::End => {
                let line_gap = line_metrics.line_gap;
                let new_line_size = line_metrics.new_line_size;
                let text_block_size = new_line_size * lines.len() as f32 - line_gap;

                line_gap + available_height - text_block_size
            }
        }
    } else {
        line_metrics.line_gap
    };

    let mut cursor_drawn = false;
    let mut selection_rect = Box2::ZERO;

    for line in &lines {
        let line_slice = &text[line.idx_start..line.idx_end];

        position_x = match halign {
            Align::Start => 0.0,
            Align::Center => (available_width - line.width) / 2.0,
            Align::End => available_width - line.width,
        };

        for (i, c) in line_slice.chars().enumerate() {
            // Reborrow font_atlas, so that the globally borrowed one is
            // released and we can call Ctrl::draw_rect.
            let font_atlas = frame.font_atlas();
            let glyph_info = font_atlas.glyph_info(c);

            let position = Vec2::new(position_x, position_y);
            let rect = glyph_info.rect + position + vec2(0.0, line_metrics.ascent);

            let text_position = i + line_idx_start;
            if text_position == text_cursor {
                frame.ctrl_draw_rect(
                    Box2::new(position_x, position_y, 1.0, line_metrics.ascent - line_metrics.descent),
                    Box2::ZERO,
                    0x40ffa0c0,
                    font_atlas_texture_id,
                );
                cursor_drawn = true;
            }

            if text_position >= text_selection_start && text_position <= text_selection_end {
                let r = Box2::new(position.x, position_y, 0.0, line_metrics.ascent - line_metrics.descent);

                if selection_rect == Box2::ZERO {
                    selection_rect = r;
                } else {
                    selection_rect = selection_rect.extend_by_box(r);
                }
            }

            // TODO(yan): @Speed @Memory Does early software scissor make
            // sense here? We also do it later, when translating to the
            // low-level draw list, but we could have less things to
            // translate.
            frame.ctrl_draw_rect(rect, glyph_info.atlas_rect, color, font_atlas_texture_id);

            position_x += glyph_info.advance_width;
        }

        position_y += line_metrics.new_line_size;
    }

    if selection_rect != Box2::ZERO {
        if text_selection_end == text.len() {
            selection_rect = selection_rect.extend_by_point(Vec2::new(position_x, position_y));
        }

        frame.ctrl_draw_rect(selection_rect, Box2::ZERO, 0x40ffa040, font_atlas_texture_id)
    }

    if !cursor_drawn {
        let rect = Box2::new(
            position_x,
            position_y - line_metrics.ascent + line_metrics.descent,
            font_size / 2.0,
            line_metrics.ascent - line_metrics.descent,
        );

        frame.ctrl_draw_rect(rect, Box2::ZERO, 0x40ffa0c0, font_atlas_texture_id);
    }
}

fn seek_prev(index: usize, text: &str) -> usize {
    debug_assert!(index <= text.len());
    floor_char_boundary(text, index.saturating_sub(1))
}

fn seek_next(index: usize, text: &str) -> usize {
    debug_assert!(index <= text.len());

    // Cursor can point at one past last index.
    if index < text.len() {
        ceil_char_boundary(text, index + 1)
    } else {
        index
    }
}

// TODO(jt): @Cleanup Use str::floor_char_boundary once it stabilizes.
fn floor_char_boundary(text: &str, mut index: usize) -> usize {
    while index > 0 && !text.is_char_boundary(index) {
        index -= 1;
    }

    index
}

// TODO(jt): @Cleanup Use str::ceil_char_boundary once it stabilizes.
fn ceil_char_boundary(text: &str, mut index: usize) -> usize {
    while index < text.len() && !text.is_char_boundary(index) {
        index += 1;
    }

    index
}
