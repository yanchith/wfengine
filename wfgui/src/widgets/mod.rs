mod button;
mod checkbox;
mod dropdown;
mod float_slider;
mod image_button;
mod int_slider;
mod panel;
mod separator;
mod size;
mod text;
mod text_input;
mod theme;
mod tooltip;
mod window;

pub use button::*;
pub use checkbox::*;
pub use dropdown::*;
pub use float_slider::*;
pub use image_button::*;
pub use int_slider::*;
pub use panel::*;
pub use separator::*;
pub use size::*;
pub use text::*;
pub use text_input::*;
pub use theme::*;
pub use tooltip::*;
pub use window::*;

// TODO(jt): Widget API!
//
// Most basic are functions. These are always available, if you don't want the advanced versions.
//
// wfgui::text_input(frame, id!(0), &text_buffer, "Thingy");
// wfgui::text_input_ex(frame, id!(0), &text_buffer, "Thingy", wfgui::ButtonSettings::default());
//
// However, for those that do want it, there are macros:
//
// wfgui::text_input!(frame, &text_buffer, "Thingy");
//
// Apart from filling in id!(0) for you, the macro spawns default settings, and splices on top the "named parameters".
//
// wfgui::text_input!(frame, &text_buffer, "Thingy", readonly = false);
//
// If, for some reason you don't want it to fill in id!(0), you can pass the the id directly as a named parameter:
//
// wfgui::text_input!(frame, &text_buffer, "Thingy", id = explitit_id, readonly = false);
