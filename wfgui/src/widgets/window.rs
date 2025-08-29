use core::fmt::Debug;

use wfmath::Box2;
use wfmath::Vec2;

use crate::Theme;
use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::CtrlState;
use crate::core::Frame;
use crate::core::Inputs;
use crate::core::Layout;
use crate::widgets::size::Size;

const FLAGS: CtrlFlags = CtrlFlags::CAPTURE_SCROLL
    .const_bitor(CtrlFlags::CAPTURE_HOVER)
    .const_bitor(CtrlFlags::CAPTURE_ACTIVE);

const ACTIVITY_NONE: u8 = 0;
const ACTIVITY_MOVE: u8 = 1;
const ACTIVITY_RESIZE: u8 = 2;

pub struct Window(bool);

impl Window {
    // TODO(jt): @Cleanup Can we restructure the API such that Drop calls pop_ctrl, so we don't have
    // to? We might run into some borrowchecker issues, but it would be worth it.
    pub fn end(mut self, frame: &mut Frame) {
        assert!(!self.0);

        frame.pop_ctrl();
        self.0 = true;
    }
}

impl Drop for Window {
    fn drop(&mut self) {
        debug_assert!(self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct WindowTheme {
    pub border_color: u32,
    pub border_color_hovered: u32,
    pub background_color: u32,
    pub background_color_hovered: u32,
    pub border: f32,
    pub padding: f32,
}

impl WindowTheme {
    pub const DEFAULT: WindowTheme = Theme::DEFAULT.window;
    pub const TRANSPARENT: WindowTheme = WindowTheme {
        border_color: 0,
        border_color_hovered: 0,
        background_color: 0,
        background_color_hovered: 0,
        border: 0.0,
        padding: 0.0,
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct WindowSettings<'a> {
    pub movable: bool,
    pub resizable: bool,
    pub open_on_top: bool,
    pub layout: Layout,
    pub min_width: Option<f32>,
    pub min_height: Option<f32>,
    pub max_width: Option<f32>,
    pub max_width_align: Align,
    pub max_height: Option<f32>,
    pub max_height_align: Align,
    pub theme: &'a WindowTheme,
}

impl Default for WindowSettings<'_> {
    fn default() -> Self {
        Self {
            movable: true,
            resizable: true,
            open_on_top: true,
            layout: Layout::Vertical,
            min_width: None,
            min_height: None,
            max_width: None,
            max_width_align: Align::Center,
            max_height: None,
            max_height_align: Align::Center,
            theme: &Theme::DEFAULT.window,
        }
    }
}

#[macro_export]
macro_rules! begin_window {
    ($frame:expr, $x:expr, $y:expr, $width:expr, $height:expr
     $(,movable = $movable:expr)?
     $(,resizable = $resizable:expr)?
     $(,open_on_top = $open_on_top:expr)?
     $(,layout = $layout:expr)?
     $(,min_width = $min_width:expr)?
     $(,min_height = $min_height:expr)?
     $(,max_width = $max_width:expr)?
     $(,max_width_align = $max_width_align:expr)?
     $(,max_height = $max_height:expr)?
     $(,max_height_align = $max_height_align:expr)?
     $(,theme = $theme:expr)?
     $(,)?
    ) => {
        $crate::begin_window_ex($frame, $crate::id!(0), $x, $y, $width, $height, $crate::WindowSettings {
            $(movable: $movable,)?
            $(resizable: $resizable,)?
            $(open_on_top: $open_on_top,)?
            $(layout: $layout,)?
            $(min_width: $min_width,)?
            $(min_height: $min_height,)?
            $(max_width: $max_width,)?
            $(max_width_align: $max_width_align,)?
            $(max_height: $max_height,)?
            $(max_height_align: $max_height_align,)?
            $(theme: $theme,)?
            ..$crate::WindowSettings::default()
        })
    };
}

// TODO(yan): Make this actually return None when the window is collapsed,
// minimized, or something.
#[inline]
pub fn begin_window<X, Y, W, H>(frame: &mut Frame, id: CtrlId, x: X, y: Y, width: W, height: H) -> Option<Window>
where
    X: TryInto<Size>,
    Y: TryInto<Size>,
    W: TryInto<Size>,
    H: TryInto<Size>,
    <X as TryInto<Size>>::Error: Debug,
    <Y as TryInto<Size>>::Error: Debug,
    <W as TryInto<Size>>::Error: Debug,
    <H as TryInto<Size>>::Error: Debug,
{
    begin_window_ex(frame, id, x, y, width, height, WindowSettings::default())
}

pub fn begin_window_ex<X, Y, W, H>(
    frame: &mut Frame,
    id: CtrlId,
    x: X,
    y: Y,
    width: W,
    height: H,
    settings: WindowSettings,
) -> Option<Window>
where
    X: TryInto<Size>,
    Y: TryInto<Size>,
    W: TryInto<Size>,
    H: TryInto<Size>,
    <X as TryInto<Size>>::Error: Debug,
    <Y as TryInto<Size>>::Error: Debug,
    <W as TryInto<Size>>::Error: Debug,
    <H as TryInto<Size>>::Error: Debug,
{
    let x = x.try_into().unwrap();
    let y = y.try_into().unwrap();
    let width = width.try_into().unwrap();
    let height = height.try_into().unwrap();

    let texture_id = frame.font_atlas_texture_id();
    let parent_size = frame.ctrl_inner_size();
    let cursor_position = frame.cursor_position();
    let lmb_pressed = frame.inputs_pressed() == Inputs::MB_LEFT;
    let lmb_released = frame.inputs_released() == Inputs::MB_LEFT;

    frame.push_ctrl(id);
    let hovered = frame.ctrl_is_hovered();

    let state = cast_state(frame.ctrl_state());
    let (x, y, mut width, mut height, activity, initialized) = if state.initialized == 1 {
        let (orig_x, orig_y) = if settings.movable {
            (state.x, state.y)
        } else {
            (x.resolve(parent_size.x), y.resolve(parent_size.y))
        };

        let (orig_width, orig_height) = if settings.resizable {
            (state.width, state.height)
        } else {
            (width.resolve(parent_size.x), height.resolve(parent_size.y))
        };

        let mut x = orig_x;
        let mut y = orig_y;
        let mut width = orig_width;
        let mut height = orig_height;

        if let Some(min_width) = settings.min_width {
            width = f32::max(width, min_width);
        }

        if let Some(max_width) = settings.max_width {
            width = f32::min(width, max_width);
        }

        if let Some(min_height) = settings.min_height {
            height = f32::max(height, min_height);
        }

        if let Some(max_height) = settings.max_height {
            height = f32::min(height, max_height);
        }

        if width < orig_width {
            match settings.max_width_align {
                Align::Start => (),
                Align::Center => x += 0.5 * (orig_width - width),
                Align::End => x += orig_width - width,
            }
        }

        if height < orig_height {
            match settings.max_height_align {
                Align::Start => (),
                Align::Center => y += 0.5 * (orig_height - height),
                Align::End => y += orig_height - height,
            }
        }

        let activity = match (state.activity, settings.movable, settings.resizable) {
            (ACTIVITY_MOVE, false, _) => ACTIVITY_NONE,
            (ACTIVITY_RESIZE, _, false) => ACTIVITY_NONE,
            (activity, _, _) => activity,
        };

        (x, y, width, height, activity, true)
    } else {
        (
            x.resolve(parent_size.x),
            y.resolve(parent_size.y),
            width.resolve(parent_size.x),
            height.resolve(parent_size.y),
            ACTIVITY_NONE,
            false,
        )
    };

    frame.ctrl_set_flags(FLAGS);
    frame.ctrl_set_layout(settings.layout);
    frame.ctrl_set_rect(Box2::new(x, y, f32::max(width, 0.0), f32::max(height, 0.0)));
    frame.ctrl_set_padding(settings.theme.padding);
    frame.ctrl_set_border(settings.theme.border);
    frame.ctrl_set_margin(0.0);

    let resize_handle_dimension = settings.theme.padding + settings.theme.border;
    let resize_handle_hovered = {
        let position = frame.ctrl_absolute_position();
        let rect = Box2::new(
            position.x + width - resize_handle_dimension,
            position.y + height - resize_handle_dimension,
            resize_handle_dimension,
            resize_handle_dimension,
        );
        rect.contains_point(cursor_position)
    };

    let state = cast_state_mut(frame.ctrl_state_mut());
    state.x = x;
    state.y = y;
    state.width = width;
    state.height = height;
    state.initialized = 1;

    if !settings.movable && activity == ACTIVITY_MOVE {
        state.activity = ACTIVITY_NONE;
    }
    if !settings.resizable && activity == ACTIVITY_RESIZE {
        state.activity = ACTIVITY_NONE;
    }

    if activity == ACTIVITY_RESIZE {
        if lmb_released {
            state.activity = ACTIVITY_NONE;
        } else {
            let activity_start_x = state.activity_start_x;
            let activity_start_y = state.activity_start_y;
            let activity_start_size = Vec2::new(activity_start_x, activity_start_y);

            let activity_start_cursor_x = state.activity_start_cursor_x;
            let activity_start_cursor_y = state.activity_start_cursor_y;
            let activity_start_cursor_position = Vec2::new(activity_start_cursor_x, activity_start_cursor_y);

            let size = activity_start_size + cursor_position - activity_start_cursor_position;
            let size_clamped = size.max(Vec2::ZERO);

            width = size_clamped.x;
            height = size_clamped.y;

            state.width = width;
            state.height = height;

            // Set rect again with updated data to reduce latency
            frame.ctrl_set_rect(Box2::new(x, y, width, height));
        }
    } else if settings.resizable && hovered && resize_handle_hovered && lmb_pressed {
        state.activity = ACTIVITY_RESIZE;
        state.activity_start_x = width;
        state.activity_start_y = height;
        state.activity_start_cursor_x = cursor_position.x;
        state.activity_start_cursor_y = cursor_position.y;
    } else if activity == ACTIVITY_MOVE {
        if lmb_released {
            state.activity = ACTIVITY_NONE;
        } else {
            let activity_start_x = state.activity_start_x;
            let activity_start_y = state.activity_start_y;
            let activity_start_position = Vec2::new(activity_start_x, activity_start_y);

            let activity_start_cursor_x = state.activity_start_cursor_x;
            let activity_start_cursor_y = state.activity_start_cursor_y;
            let activity_start_cursor_position = Vec2::new(activity_start_cursor_x, activity_start_cursor_y);

            let position = activity_start_position + cursor_position - activity_start_cursor_position;

            state.x = position.x;
            state.y = position.y;

            // Set rect again with updated data to reduce latency
            frame.ctrl_set_rect(Box2::new(position.x, position.y, width, height));
        }
    } else if settings.movable && hovered && lmb_pressed {
        state.activity = ACTIVITY_MOVE;
        state.activity_start_x = x;
        state.activity_start_y = y;
        state.activity_start_cursor_x = cursor_position.x;
        state.activity_start_cursor_y = cursor_position.y;
    }

    if hovered && lmb_pressed || settings.open_on_top && !initialized {
        frame.ctrl_set_active(true);
    }

    let (background_color, border_color, resize_handle_color) =
        match (hovered, resize_handle_hovered || activity == ACTIVITY_RESIZE) {
            (false, _) => (
                settings.theme.background_color,
                settings.theme.border_color,
                settings.theme.border_color,
            ),
            (true, false) => (
                settings.theme.background_color_hovered,
                settings.theme.border_color_hovered,
                settings.theme.border_color_hovered,
            ),
            (true, true) => (
                settings.theme.background_color_hovered,
                settings.theme.border_color_hovered,
                0xffffffff,
            ),
        };

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(border_color);
    frame.ctrl_set_draw_self_background_color(background_color);

    if settings.resizable {
        let offset_x = frame.ctrl_scroll_offset_x();
        let offset_y = frame.ctrl_scroll_offset_y();

        frame.ctrl_draw_rect(
            Box2::new(
                width - resize_handle_dimension + offset_x,
                height - resize_handle_dimension + offset_y,
                resize_handle_dimension,
                resize_handle_dimension,
            ),
            Box2::ZERO,
            resize_handle_color,
            texture_id,
        );
    }

    Some(Window(false))
}

#[repr(C)]
#[derive(Clone, Copy)]
#[derive(bytemuck::AnyBitPattern, bytemuck::NoUninit)]
struct State {
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    activity_start_cursor_x: f32,
    activity_start_cursor_y: f32,
    activity_start_x: f32,
    activity_start_y: f32,
    activity: u8,
    initialized: u8,
    _pad0: u8,
    _pad1: u8,
}

fn cast_state(state: &CtrlState) -> &State {
    bytemuck::from_bytes(&state.0[..size_of::<State>()])
}

fn cast_state_mut(state: &mut CtrlState) -> &mut State {
    bytemuck::from_bytes_mut(&mut state.0[..size_of::<State>()])
}
