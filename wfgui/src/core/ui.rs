use alloc::string::String;
use alloc::vec::Vec;
use core::mem;
use core::ops::Range;

use arrayvec::ArrayString;
use wfarena::Arena;
use wfcommon::static_assert;
use wfflags::flags;
use wfmath::Box2;
use wfmath::Vec2;
use wfmath::Vec4;
use wfmath::srgb_to_linear;
use wfmath::vec2;
use wfslab::SlabArray;
use wfslab::SlabLoc;
use wftime::Nanos;

use crate::core::draw_list::Command;
use crate::core::draw_list::DrawList;
use crate::core::draw_list::Vertex;
use crate::core::font_atlas::FontAtlas;
use crate::core::font_atlas::UnicodeRangeFlags;

const ROOT_IDX: usize = 0;
const OVERLAY_ROOT_IDX: usize = 1;

static DEFAULT_STRING: &ArrayString<256> = &ArrayString::new_const();

#[flags(u32)]
pub enum Inputs {
    MB_LEFT = 1 << 0,
    MB_RIGHT = 1 << 1,
    MB_MIDDLE = 1 << 2,
    MB4 = 1 << 3,
    MB5 = 1 << 4,
    MB6 = 1 << 5,
    MB7 = 1 << 6,

    KB_TAB = 1 << 7,
    KB_LEFT_ARROW = 1 << 8,
    KB_RIGHT_ARROW = 1 << 9,
    KB_UP_ARROW = 1 << 10,
    KB_DOWN_ARROW = 1 << 11,
    KB_PAGE_UP = 1 << 12,
    KB_PAGE_DOWN = 1 << 13,
    KB_HOME = 1 << 14,
    KB_END = 1 << 15,
    KB_INSERT = 1 << 16,
    KB_DELETE = 1 << 17,
    KB_BACKSPACE = 1 << 18,
    KB_ENTER = 1 << 19,
    KB_ESCAPE = 1 << 20,

    // Selection:
    KB_A = 1 << 21,

    // Emacs navigation keys:
    KB_F = 1 << 22,
    KB_B = 1 << 23,
    KB_N = 1 << 24,
    KB_P = 1 << 25,

    // Copy & Paste:
    KB_X = 1 << 26,
    KB_C = 1 << 27,
    KB_V = 1 << 28,
}

#[flags(u32)]
pub enum Modifiers {
    CTRL = 0x01,
    ALT = 0x02,
    SHIFT = 0x04,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Layout {
    Free,
    Horizontal,
    Vertical,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Align {
    Start,
    Center,
    End,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Wrap {
    Word,
    Letter,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum DrawPrimitive {
    Rect {
        rect: Box2,
        texture_rect: Box2,
        texture_id: u64,
        color: Vec4,
    },
    // TODO(yan): Circles, Rounded arcs, whatever..
}

#[flags(u32)]
pub enum CtrlFlags {
    /// Whether the control should be affected by user interaction generated
    /// scrolling (and therefore capture scroll events when there is someplace
    /// to scroll). Regardless of this flag, controls can be scrolled
    /// programmatically.
    CAPTURE_SCROLL = 0x01,

    /// Whether the control should report being hovered (and therefore capture
    /// hover events). Hovering is tracked internally regardless of this flag,
    /// but not setting the flag let's the hover event flow to parent controls.
    CAPTURE_HOVER = 0x02,

    /// Whether the control should become active if one of its children becomes
    /// inactive. Controls can become active programmatically regardless of this
    /// flag.
    CAPTURE_ACTIVE = 0x04,

    /// Whether to resize the control's rect width to the width of its contents,
    /// child or inline.
    ///
    /// One usecase is auto-sizing tooltips based on content.
    ///
    /// This has no downsides for non-interactive controls, because the layout
    /// pass computes the size of all of control's contents before they are used
    /// for rendering. Any interactivity may experience a one frame lag,
    /// however, because building the UI happens before layout is computed, and
    /// only has layout data from last frame, if any.
    RESIZE_TO_FIT_HORIZONTAL = 0x08,

    /// Whether to resize the control's rect height to the height of its contents,
    /// child or inline.
    ///
    /// One usecase is auto-sizing tooltips based on content.
    ///
    /// This has no downsides for non-interactive controls, because the layout
    /// pass computes the size of all of control's contents before they are used
    /// for rendering. Any interactivity may experience a one frame lag,
    /// however, because building the UI happens before layout is computed, and
    /// only has layout data from last frame, if any.
    RESIZE_TO_FIT_VERTICAL = 0x10,
}

#[macro_export]
macro_rules! id {
    ($loop_prefix:expr) => {
        $crate::CtrlId {
            line: line!(),
            loop_prefix: $loop_prefix,
            file: file!(),
        }
    };
}

// TODO(jt): @Speed @Memory Consider compressing this to 64 or 128 bits (currently is 192) by
// hashing the source file.
//
// TODO(jt): @Correctness This ID still isn't as unique as we'd like, because someone can still
// smuggle multiple of id!() calls onto the same line in the same file. Ideally, we want a
// compile-time counter (like C has in unity builds, or like JAI has (if it doesn't reset across
// modules)). A good thing about that would also be that we'd cut down the size of this struct,
// storing just u32+u32 instead of u32+u32+pointer+usize. We could also try exploiting TypeId::of
// inside macro, but that's u64+u64+u32 AND it is not guaranteed to be unique between crates.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CtrlId {
    pub line: u32,
    pub loop_prefix: u32,
    pub file: &'static str,
}

static_assert!(size_of::<CtrlId>() == 24);
static_assert!(align_of::<CtrlId>() == 8);

#[repr(C, align(64))]
#[derive(Debug, Clone, PartialEq)]
pub struct CtrlState(pub [u8; 64]);

#[derive(Debug, Clone, PartialEq)]
struct CtrlNode {
    // Has to be unique across siblings, but no further.
    id: CtrlId,

    // TODO(yan): @Speed @Memory Make indices more compact. Option<usize> is 16
    // bytes, but we could carve out a niche.
    parent_idx: Option<usize>,
    child_idx: Option<usize>,
    sibling_idx: Option<usize>,

    strings_idx: Option<SlabLoc>,

    first_frame: u32,
    // Deallocate if not current.
    last_frame: u32,
    // Used to sort free layout controls for detecting hover and rendering.
    last_frame_in_active_path: u32,

    // Layout things
    flags: CtrlFlags,
    layout: Layout,
    rect: Box2,
    padding: f32,
    border: f32,
    margin: f32,

    inline_content_rect: Option<Box2>,

    scroll_offset: Vec2,

    // TODO(yan): @Memory For some controls this is too much memory, and for
    // some others this is not enough. We should eventually make this as small
    // as possible, e.g. just enough to satisfy the larger-but-still-small
    // controls, like Window, and create optional extra state storage for
    // controls that require a lot more.
    //
    // Alternatively, we can do memory size-classed slabs with freelists outside
    // of CtrlNode, and make memory node contain either no state memory at all,
    // or just the smallest size-class.
    state: CtrlState,

    draw_self: bool,
    draw_self_border_color: u32,
    draw_self_background_color: u32,
    draw_range: Range<usize>,

    layout_cache_absolute_position: Vec2,
    layout_cache_content_size: Vec2,
}

pub struct Ui {
    perm_arena: &'static Arena,
    // TODO(jt): It would be nice if we didn't require a temp arena, and could somehow split the
    // scratch space from the permanent arena each frame, but at the moment it seems difficult.
    temp_arena: Arena,

    draw_primitives: SlabArray<DrawPrimitive, &'static Arena, 1024>,
    // This isn't stored in slabs, so that it is contiguous and easier to consume. It is pre-sized a
    // little to mitigate failed grows.
    draw_list: DrawList<&'static Arena>,

    // TODO(jt): Write our own font atlas generator (ttf parser+rasterizer).
    //
    // https://github.com/nothings/stb/blob/master/stb_truetype.h
    //
    // TODO(jt): Additionally, maybe we can do SDF fonts? This would require a separate shader for
    // text, but maybe that's worth it?
    //
    // https://steamcdn-a.akamaihd.net/apps/valve/2007/SIGGRAPH2007_AlphaTestedMagnification.pdf
    // https://www.redblobgames.com/x/2403-distance-field-fonts/
    font_atlas: FontAtlas<&'static Arena>,
    font_atlas_texture_id: u64,

    ctrl_tree: SlabArray<CtrlNode, &'static Arena, 1024>,
    // TODO(jt): Turn this into a "large memory" API instead of storing just strings?
    ctrl_strings: SlabArray<ArrayString<256>, &'static Arena, 64>,

    building_overlay: bool,
    build_parent_idx: Option<usize>,
    build_sibling_idx: Option<usize>,
    overlay_build_parent_idx: Option<usize>,
    overlay_build_sibling_idx: Option<usize>,

    current_frame: u32,
    current_frame_time: Nanos,

    window_size: Vec2,
    window_scale_factor: f32,
    scroll_delta: Vec2,
    cursor_position: Vec2,
    inputs_pressed: Inputs,
    inputs_released: Inputs,
    modifiers: Modifiers,
    received_characters: ArrayString<32>,
    // TODO(jt): @Memory Would we great if we didn't allocate the String here somehow.
    clipboard_getter: fn() -> String,
    clipboard_setter: fn(&str),

    active_ctrl_idx: Option<usize>,
    hovered_ctrl_idx: Option<usize>,
    hovered_capturing_ctrl_idx: Option<usize>,

    // TODO(yan): When exactly should we be capturing keyboard and mouse
    // automatically, when no control requests it? Currently we capture mouse
    // when something is hovered (ImGui does the same). ImGui also automatically
    // captures keyboard not just when a text field is active, but e.g. when
    // windows are being dragged around.
    want_capture_keyboard: bool,
    want_capture_mouse: bool,
}

impl Ui {
    // TODO(jt): Add a with_capacity_in, which pre-sizes structures for N controls.
    pub fn new_in(
        window_width: f32,
        window_height: f32,
        window_scale_factor: f32,
        font_bytes: &[u8],
        font_unicode_range_flags: UnicodeRangeFlags,
        font_size: f32,
        // Maximum window scale factor the rasterizer is prepared for. If
        // displaying on a single monitor, that display's scale factor should be
        // used. If there are multiple monitors with different scales, pick
        // highest for sharpest looking fonts, or lower, if memory or speed is
        // an issue.
        font_rasterization_scale_factor: f32,
        perm_arena: Arena,
        temp_arena: Arena,
    ) -> Self {
        let perm_arena = perm_arena.leak().unwrap(); // TODO(jt): Don't unwrap?

        let window_size = Vec2::new(window_width, window_height);
        let font_atlas = FontAtlas::new_in(
            font_bytes,
            font_unicode_range_flags,
            font_size,
            font_rasterization_scale_factor,
            perm_arena,
        );

        let root_ctrl = CtrlNode {
            id: id!(0),

            parent_idx: None,
            child_idx: None,
            sibling_idx: None,

            strings_idx: None,

            first_frame: 0,
            last_frame: 0,
            last_frame_in_active_path: 0,

            flags: CtrlFlags::NONE,
            layout: Layout::Free,
            rect: Box2::from_points(Vec2::ZERO, window_size),
            padding: 0.0,
            border: 0.0,
            margin: 0.0,

            inline_content_rect: None,

            scroll_offset: Vec2::ZERO,

            state: CtrlState([0; 64]),

            draw_self: false,
            draw_self_border_color: 0,
            draw_self_background_color: 0,
            draw_range: 0..0,

            layout_cache_absolute_position: Vec2::ZERO,
            layout_cache_content_size: Vec2::ZERO,
        };

        let mut ctrl_tree = SlabArray::new_in(perm_arena);
        ctrl_tree.push(root_ctrl.clone());
        ctrl_tree.push(root_ctrl);

        Self {
            perm_arena,
            temp_arena,

            draw_primitives: SlabArray::new_in(perm_arena),
            draw_list: DrawList::with_capacity_in(1000, perm_arena),

            font_atlas,
            font_atlas_texture_id: 0,

            ctrl_tree,
            ctrl_strings: SlabArray::new_in(perm_arena),

            building_overlay: false,
            build_parent_idx: None,
            build_sibling_idx: None,
            overlay_build_parent_idx: None,
            overlay_build_sibling_idx: None,

            current_frame: 0,
            current_frame_time: Nanos::ZERO,

            window_size,
            window_scale_factor,
            scroll_delta: Vec2::ZERO,
            cursor_position: Vec2::ZERO,
            inputs_pressed: Inputs::NONE,
            inputs_released: Inputs::NONE,
            modifiers: Modifiers::NONE,
            received_characters: ArrayString::new(),
            clipboard_getter: empty_clipboard_getter,
            clipboard_setter: empty_clipboard_setter,

            active_ctrl_idx: None,
            hovered_ctrl_idx: None,
            hovered_capturing_ctrl_idx: None,

            want_capture_keyboard: false,
            want_capture_mouse: false,
        }
    }

    pub fn set_font_atlas_texture_id(&mut self, font_atlas_texture_id: u64) {
        self.font_atlas_texture_id = font_atlas_texture_id;
    }

    pub fn set_window_size(&mut self, window_width: f32, window_height: f32) {
        self.window_size = Vec2::new(window_width, window_height);
    }

    pub fn set_window_scale_factor(&mut self, window_scale_factor: f32) {
        self.window_scale_factor = window_scale_factor;
    }

    pub fn scroll(&mut self, delta_x: f32, delta_y: f32) {
        self.scroll_delta += Vec2::new(delta_x, delta_y);
    }

    pub fn set_cursor_position(&mut self, cursor_x: f32, cursor_y: f32) {
        self.cursor_position = Vec2::new(cursor_x, cursor_y);
    }

    pub fn press_inputs(&mut self, inputs: Inputs) {
        self.inputs_pressed |= inputs;
    }

    pub fn release_inputs(&mut self, inputs: Inputs) {
        self.inputs_released |= inputs;
    }

    pub fn set_modifiers(&mut self, modifiers: Modifiers) {
        self.modifiers = modifiers;
    }

    pub fn press_modifiers(&mut self, modifiers: Modifiers) {
        self.modifiers |= modifiers;
    }

    pub fn release_modifiers(&mut self, modifiers: Modifiers) {
        self.modifiers &= !modifiers;
    }

    pub fn send_character(&mut self, character: char) {
        let _ = self.received_characters.try_push(character);
    }

    pub fn set_clipboard_getter(&mut self, getter: fn() -> String) {
        self.clipboard_getter = getter;
    }

    pub fn set_clipboard_setter(&mut self, setter: fn(&str)) {
        self.clipboard_setter = setter;
    }

    pub fn font_atlas(&self) -> &FontAtlas<&'static Arena> {
        &self.font_atlas
    }

    pub fn font_atlas_texture_id(&self) -> u64 {
        self.font_atlas_texture_id
    }

    pub fn font_atlas_image_size(&self) -> (u16, u16) {
        self.font_atlas.image_size()
    }

    pub fn font_atlas_image_rgba8_unorm(&self) -> &[u8] {
        self.font_atlas.image_rgba8_unorm()
    }

    pub fn ctrl_count(&self) -> usize {
        self.ctrl_tree.len()
    }

    pub fn want_capture_keyboard(&self) -> bool {
        self.want_capture_keyboard
    }

    pub fn want_capture_mouse(&self) -> bool {
        self.want_capture_mouse
    }

    pub fn commands(&self) -> &[Command] {
        self.draw_list.commands()
    }

    pub fn vertices(&self) -> &[Vertex] {
        self.draw_list.vertices()
    }

    pub fn indices(&self) -> &[u32] {
        self.draw_list.indices()
    }

    pub fn perm_arena_reserved_size(&self) -> usize {
        self.perm_arena.reserved_size()
    }

    pub fn perm_arena_allocated_size(&self) -> usize {
        self.perm_arena.allocated_size()
    }

    pub fn temp_arena_reserved_size(&self) -> usize {
        self.temp_arena.reserved_size()
    }

    pub fn temp_arena_allocated_size(&self) -> usize {
        self.temp_arena.allocated_size()
    }

    pub fn begin_frame(&mut self, time: Nanos) -> Frame<'_> {
        self.temp_arena.reset();

        self.draw_primitives.clear();
        self.draw_list.clear();
        self.want_capture_keyboard = false;
        self.want_capture_mouse = false;

        self.current_frame = self.current_frame.wrapping_add(1);
        self.current_frame_time = time;

        let root_ctrl = &mut self.ctrl_tree[ROOT_IDX];
        root_ctrl.last_frame = self.current_frame;
        root_ctrl.last_frame_in_active_path = self.current_frame;
        root_ctrl.rect = Box2::from_points(Vec2::ZERO, self.window_size);

        let overlay_root_ctrl = &mut self.ctrl_tree[OVERLAY_ROOT_IDX];
        overlay_root_ctrl.last_frame = self.current_frame;
        overlay_root_ctrl.last_frame_in_active_path = self.current_frame;
        overlay_root_ctrl.rect = Box2::from_points(Vec2::ZERO, self.window_size);

        //
        // Find hovered control.
        //
        // Look at the tree starting from the root and follow branches where the
        // child control's rect contains the cursor. First look at the overlay
        // tree, only then look at the base layer, if we didn't find a
        // hover-capturing ctrl.
        //
        self.hovered_capturing_ctrl_idx = None;
        self.hovered_ctrl_idx = find_hovered_ctrl(
            &self.temp_arena,
            &self.ctrl_tree,
            OVERLAY_ROOT_IDX,
            self.cursor_position,
        );

        if let Some(hovered_ctrl_idx) = self.hovered_ctrl_idx {
            let mut ctrl_idx = hovered_ctrl_idx;
            let mut ctrl = &self.ctrl_tree[hovered_ctrl_idx];

            while !ctrl.flags.intersects(CtrlFlags::CAPTURE_HOVER) && ctrl.parent_idx.is_some() {
                let parent_idx = ctrl.parent_idx.unwrap();

                ctrl_idx = parent_idx;
                ctrl = &self.ctrl_tree[parent_idx];
            }

            if ctrl.flags.intersects(CtrlFlags::CAPTURE_HOVER) {
                self.hovered_capturing_ctrl_idx = Some(ctrl_idx);
                self.want_capture_mouse = true;
            }
        }

        if self.hovered_capturing_ctrl_idx == None {
            self.hovered_ctrl_idx =
                find_hovered_ctrl(&self.temp_arena, &self.ctrl_tree, ROOT_IDX, self.cursor_position);
        }

        if let Some(hovered_ctrl_idx) = self.hovered_ctrl_idx {
            let mut ctrl_idx = hovered_ctrl_idx;
            let mut ctrl = &self.ctrl_tree[hovered_ctrl_idx];

            while !ctrl.flags.intersects(CtrlFlags::CAPTURE_HOVER) && ctrl.parent_idx.is_some() {
                let parent_idx = ctrl.parent_idx.unwrap();

                ctrl_idx = parent_idx;
                ctrl = &self.ctrl_tree[parent_idx];
            }

            if ctrl.flags.intersects(CtrlFlags::CAPTURE_HOVER) {
                self.hovered_capturing_ctrl_idx = Some(ctrl_idx);
                self.want_capture_mouse = true;
            }
        }

        fn find_hovered_ctrl(
            temp: &Arena,
            tree: &SlabArray<CtrlNode, &'static Arena, 1024>,
            ctrl_idx: usize,
            cursor_position: Vec2,
        ) -> Option<usize> {
            let ctrl = &tree[ctrl_idx];
            let ctrl_rect_absolute = Box2::new(
                ctrl.layout_cache_absolute_position.x,
                ctrl.layout_cache_absolute_position.y,
                ctrl.rect.width,
                ctrl.rect.height,
            );

            if ctrl_rect_absolute.contains_point(cursor_position) {
                if ctrl.layout == Layout::Free {
                    // For free layout, we'd like to preserve the render order
                    // of controls when determining hover. The most recently
                    // active control (on top) has priority when determining
                    // hover, followed by the next most recently active control,
                    // all the way up to the least recently active control.

                    let mut siblings: Vec<(usize, u32), _> = Vec::new_in(temp);
                    if let Some(child_idx) = ctrl.child_idx {
                        let mut child = &tree[child_idx];
                        siblings.push((child_idx, child.last_frame_in_active_path));

                        while let Some(sibling_idx) = child.sibling_idx {
                            child = &tree[sibling_idx];
                            siblings.push((sibling_idx, child.last_frame_in_active_path));
                        }
                    }

                    siblings.sort_unstable_by_key(|&(_, frame)| frame);

                    for (sibling_idx, _) in siblings.into_iter().rev() {
                        if let Some(hovered_ctrl) = find_hovered_ctrl(temp, tree, sibling_idx, cursor_position) {
                            // This control is hovered, but also one of its
                            // children is.
                            return Some(hovered_ctrl);
                        }
                    }

                    // This control is hovered, but none of its children are.
                    Some(ctrl_idx)
                } else if let Some(child_idx) = ctrl.child_idx {
                    if let Some(hovered_ctrl) = find_hovered_ctrl(temp, tree, child_idx, cursor_position) {
                        // This control is hovered, but also one of its
                        // children is.
                        return Some(hovered_ctrl);
                    }

                    let mut child = &tree[child_idx];
                    while let Some(sibling_idx) = child.sibling_idx {
                        child = &tree[sibling_idx];

                        if let Some(hovered_ctrl) = find_hovered_ctrl(temp, tree, sibling_idx, cursor_position) {
                            // This control is hovered, but also one of its
                            // children is.
                            return Some(hovered_ctrl);
                        }
                    }

                    // This control is hovered, but none of its children are.
                    Some(ctrl_idx)
                } else {
                    // This control is hovered and has no children to explore.
                    Some(ctrl_idx)
                }
            } else {
                // This control is not hovered.
                None
            }
        }

        //
        // Scroll a control.
        //
        // If the hovered control doesn't want scrolling or doesn't have
        // overflow it could scroll, walk the tree up to the first eligible
        // control and scroll that!
        //
        if self.scroll_delta != Vec2::ZERO {
            if let Some(idx) = self.hovered_ctrl_idx {
                let mut ctrl = &mut self.ctrl_tree[idx];
                let mut ctrl_scroll_size = Vec2::ZERO
                    .max(ctrl.layout_cache_content_size - ctrl.rect.size() + 2.0 * ctrl.padding + 2.0 * ctrl.border);
                let mut ctrl_scroll_offset_new =
                    (ctrl.scroll_offset - self.scroll_delta).clamp(Vec2::ZERO, ctrl_scroll_size);
                let mut ctrl_can_scroll =
                    ctrl.flags.intersects(CtrlFlags::CAPTURE_SCROLL) && ctrl_scroll_offset_new != ctrl.scroll_offset;

                while !ctrl_can_scroll && ctrl.parent_idx.is_some() {
                    let parent_idx = ctrl.parent_idx.unwrap();

                    ctrl = &mut self.ctrl_tree[parent_idx];
                    ctrl_scroll_size = Vec2::ZERO.max(
                        ctrl.layout_cache_content_size - ctrl.rect.size() + 2.0 * ctrl.padding + 2.0 * ctrl.border,
                    );
                    ctrl_scroll_offset_new =
                        (ctrl.scroll_offset - self.scroll_delta).clamp(Vec2::ZERO, ctrl_scroll_size);
                    ctrl_can_scroll = ctrl.flags.intersects(CtrlFlags::CAPTURE_SCROLL)
                        && ctrl_scroll_offset_new != ctrl.scroll_offset;
                }

                if ctrl_can_scroll {
                    ctrl.scroll_offset = ctrl_scroll_offset_new;
                }
            }
        }

        self.build_parent_idx = Some(ROOT_IDX);
        self.build_sibling_idx = None;
        self.overlay_build_parent_idx = Some(OVERLAY_ROOT_IDX);
        self.overlay_build_sibling_idx = None;

        Frame { ui: self }
    }

    pub fn end_frame(&mut self) {
        assert!(
            self.build_parent_idx == Some(ROOT_IDX),
            "Is there a pop_ctrl for every push_ctrl?",
        );
        assert!(
            self.overlay_build_parent_idx == Some(OVERLAY_ROOT_IDX),
            "Is there a pop_ctrl for every push_ctrl?",
        );

        // Perform cleanup on the roots analogous to the cleanup that happens in
        // pop_ctrl for other (not root) controls.
        {
            // build_parent_idx and overlay_build_parent_idx assertions
            // already happen above.
            debug_assert!(self.ctrl_tree[ROOT_IDX].sibling_idx == None);
            debug_assert!(self.ctrl_tree[OVERLAY_ROOT_IDX].sibling_idx == None);

            if let Some(build_sibling_idx) = self.build_sibling_idx {
                self.ctrl_tree[build_sibling_idx].sibling_idx = None;
            } else {
                self.ctrl_tree[self.build_parent_idx.unwrap()].child_idx = None;
            }

            if let Some(overlay_build_sibling_idx) = self.overlay_build_sibling_idx {
                self.ctrl_tree[overlay_build_sibling_idx].sibling_idx = None;
            } else {
                self.ctrl_tree[self.overlay_build_parent_idx.unwrap()].child_idx = None;
            }
        }

        // Discover reachachable dead controls in the tree. If there are any, we
        // did something wrong. There can be dead nodes, but they must not be
        // reachable.
        #[cfg(debug_assertions)]
        {
            dead_discovery(&self.ctrl_tree, ROOT_IDX, self.current_frame);
            dead_discovery(&self.ctrl_tree, OVERLAY_ROOT_IDX, self.current_frame);

            fn dead_discovery(tree: &SlabArray<CtrlNode, &'static Arena, 1024>, ctrl_idx: usize, current_frame: u32) {
                let mut ctrl = &tree[ctrl_idx];

                if ctrl.last_frame != current_frame {
                    let id = ctrl.id;
                    panic!("Reachable dead control found at {ctrl_idx}, id: {id:?}");
                }

                if let Some(child_idx) = ctrl.child_idx {
                    dead_discovery(tree, child_idx, current_frame);

                    while let Some(sibling_idx) = ctrl.sibling_idx {
                        dead_discovery(tree, sibling_idx, current_frame);
                        ctrl = &tree[sibling_idx];
                    }
                }
            }
        }

        //
        // Collect dead controls.
        //

        self.ctrl_tree.retain(|_, ctrl| {
            let retain = ctrl.last_frame == self.current_frame;
            if !retain {
                if let Some(strings_idx) = ctrl.strings_idx {
                    self.ctrl_strings.remove(strings_idx);
                }
            }

            retain
        });

        //
        // Update layout.
        //
        // Because the build phase is now done, we can incorporate all the
        // layout changes. They will be used for this frame's render phase, and
        // next frame's build phase. We update both the base layer and the
        // overlay.
        //
        layout(&mut self.ctrl_tree, ROOT_IDX, Vec2::ZERO);
        layout(&mut self.ctrl_tree, OVERLAY_ROOT_IDX, Vec2::ZERO);

        fn layout(
            tree: &mut SlabArray<CtrlNode, &'static Arena, 1024>,
            ctrl_idx: usize,
            ctrl_absolute_position_base: Vec2,
        ) {
            // TODO(yan): For horizontal and vertical layouts we advance the
            // position by the width and height of the rect of the current
            // control, but what if that control has its position offset by the
            // X or Y of the rect? (e.g. if X=100, should we advance the
            // horizontal cursor by an additional 100 pixels?)

            let ctrl = &tree[ctrl_idx];
            let ctrl_flags = ctrl.flags;
            let ctrl_layout = ctrl.layout;
            let ctrl_inline_content_rect = ctrl.inline_content_rect;
            let ctrl_absolute_position = ctrl_absolute_position_base + ctrl.rect.min_point() + ctrl.margin;

            if let Some(child_idx) = ctrl.child_idx {
                let child_absolute_position_base =
                    ctrl_absolute_position + ctrl.border + ctrl.padding - ctrl.scroll_offset;

                layout(tree, child_idx, child_absolute_position_base);

                let mut child = &tree[child_idx];
                let mut child_margin_rect = child.rect.offset(child.margin);
                let mut child_absolute_position_offset = match ctrl_layout {
                    Layout::Free => Vec2::ZERO,
                    Layout::Horizontal => Vec2::new(child_margin_rect.width, 0.0),
                    Layout::Vertical => Vec2::new(0.0, child_margin_rect.height),
                };

                let mut max_point = child_margin_rect.max_point();

                while let Some(sibling_idx) = child.sibling_idx {
                    layout(
                        tree,
                        sibling_idx,
                        child_absolute_position_base + child_absolute_position_offset,
                    );

                    child = &tree[sibling_idx];
                    child_margin_rect = child.rect.offset(child.margin);

                    match ctrl_layout {
                        Layout::Free => {
                            max_point = max_point.max(child_margin_rect.max_point());
                        }
                        Layout::Horizontal => {
                            child_absolute_position_offset += Vec2::X * child_margin_rect.width;
                            max_point.x += child_margin_rect.width;
                            max_point.y = max_point.y.max(child_margin_rect.max_y());
                        }
                        Layout::Vertical => {
                            child_absolute_position_offset += Vec2::Y * child_margin_rect.height;
                            max_point.x = max_point.x.max(child_margin_rect.max_x());
                            max_point.y += child_margin_rect.height;
                        }
                    }
                }

                if let Some(inline_content_rect) = ctrl_inline_content_rect {
                    max_point = max_point.max(inline_content_rect.max_point());
                }

                let ctrl_mut = &mut tree[ctrl_idx];
                ctrl_mut.layout_cache_absolute_position = ctrl_absolute_position;
                ctrl_mut.layout_cache_content_size = max_point;
            } else {
                let ctrl_mut = &mut tree[ctrl_idx];

                ctrl_mut.layout_cache_absolute_position = ctrl_absolute_position;
                if let Some(inline_content_rect) = ctrl_inline_content_rect {
                    ctrl_mut.layout_cache_content_size = inline_content_rect.max_point();
                } else {
                    ctrl_mut.layout_cache_content_size = Vec2::ZERO;
                }
            }

            if ctrl_flags.intersects(CtrlFlags::RESIZE_TO_FIT_HORIZONTAL | CtrlFlags::RESIZE_TO_FIT_VERTICAL) {
                let ctrl_mut = &mut tree[ctrl_idx];

                let offset = 2.0 * ctrl_mut.border + 2.0 * ctrl_mut.padding;
                let x = ctrl_mut.rect.x;
                let y = ctrl_mut.rect.y;

                let width = if ctrl_flags.intersects(CtrlFlags::RESIZE_TO_FIT_HORIZONTAL) {
                    ctrl_mut.layout_cache_content_size.x + offset
                } else {
                    ctrl_mut.rect.width
                };

                let height = if ctrl_flags.intersects(CtrlFlags::RESIZE_TO_FIT_VERTICAL) {
                    ctrl_mut.layout_cache_content_size.y + offset
                } else {
                    ctrl_mut.rect.height
                };

                ctrl_mut.rect = Box2::new(x, y, width, height);
            }
        }

        //
        // Render into the draw lists. First the base, then the overlay.
        //
        render(
            &self.temp_arena,
            &self.ctrl_tree,
            ROOT_IDX,
            Box2::from_points(Vec2::ZERO, self.window_size),
            &self.draw_primitives,
            self.font_atlas_texture_id,
            &mut self.draw_list,
        );
        render(
            &self.temp_arena,
            &self.ctrl_tree,
            OVERLAY_ROOT_IDX,
            Box2::from_points(Vec2::ZERO, self.window_size),
            &self.draw_primitives,
            self.font_atlas_texture_id,
            &mut self.draw_list,
        );

        // TODO(yan): @Memory If the allocator is a bump allocator, we
        // potentially prevent it from reclaiming memory if draw_list grows.
        fn render(
            temp: &Arena,
            tree: &SlabArray<CtrlNode, &'static Arena, 1024>,
            ctrl_idx: usize,
            parent_ctrl_scissor_rect: Box2,
            draw_primitives: &SlabArray<DrawPrimitive, &'static Arena, 1024>,
            font_atlas_texture_id: u64,
            draw_list: &mut DrawList<&'static Arena>,
        ) {
            let ctrl = &tree[ctrl_idx];
            let ctrl_rect_absolute = Box2::new(
                ctrl.layout_cache_absolute_position.x,
                ctrl.layout_cache_absolute_position.y,
                ctrl.rect.width,
                ctrl.rect.height,
            );

            let ctrl_scissor_rect = parent_ctrl_scissor_rect
                .clamp_box(ctrl_rect_absolute)
                .inset(ctrl.border);

            // Some renderer backends dislike scissor rect with zero or negative
            // dimensions, as well as dimensions greater than the surface
            // dimensions. If we get dangerously close, let's not render
            // anything.
            if ctrl_scissor_rect.width < 1.0 || ctrl_scissor_rect.height < 1.0 {
                return;
            }

            if ctrl.draw_self {
                let border_color = srgb_to_linear(ctrl.draw_self_border_color);
                let background_color = srgb_to_linear(ctrl.draw_self_background_color);

                let ctrl_padding_rect_absolute = ctrl_rect_absolute.inset(ctrl.border);

                if !ctrl_rect_absolute.is_empty() && !ctrl_padding_rect_absolute.is_empty() {
                    // Dimensions are clamped in subtractions here, because fp
                    // precision commonly caused the result to be below 0, which
                    // is a big no-no for Box2::new.

                    let outer = ctrl_rect_absolute;
                    let inner = ctrl_padding_rect_absolute;

                    let lx = outer.x;
                    let ly = outer.y;
                    let lwidth = f32::max(0.0, inner.x - outer.x);
                    let lheight = outer.height;
                    let left = Box2::new(lx, ly, lwidth, lheight);

                    let tx = inner.x;
                    let ty = outer.y;
                    let twidth = inner.width;
                    let theight = f32::max(0.0, inner.y - outer.y);
                    let top = Box2::new(tx, ty, twidth, theight);

                    let rx = inner.x + inner.width;
                    let ry = outer.y;
                    let rwidth = f32::max(0.0, outer.width - inner.width - lwidth);
                    let rheight = outer.height;
                    let right = Box2::new(rx, ry, rwidth, rheight);

                    let bx = inner.x;
                    let by = inner.y + inner.height;
                    let bwidth = inner.width;
                    let bheight = f32::max(0.0, outer.height - inner.height - theight);
                    let bottom = Box2::new(bx, by, bwidth, bheight);

                    if !left.is_empty() {
                        draw_list.draw_rect(
                            left,
                            Box2::ZERO,
                            border_color,
                            parent_ctrl_scissor_rect,
                            font_atlas_texture_id,
                        );
                    }

                    if !top.is_empty() {
                        draw_list.draw_rect(
                            top,
                            Box2::ZERO,
                            border_color,
                            parent_ctrl_scissor_rect,
                            font_atlas_texture_id,
                        );
                    }

                    if !right.is_empty() {
                        draw_list.draw_rect(
                            right,
                            Box2::ZERO,
                            border_color,
                            parent_ctrl_scissor_rect,
                            font_atlas_texture_id,
                        );
                    }

                    if !bottom.is_empty() {
                        draw_list.draw_rect(
                            bottom,
                            Box2::ZERO,
                            border_color,
                            parent_ctrl_scissor_rect,
                            font_atlas_texture_id,
                        );
                    }
                }

                draw_list.draw_rect(
                    ctrl_padding_rect_absolute,
                    Box2::ZERO,
                    background_color,
                    parent_ctrl_scissor_rect,
                    font_atlas_texture_id,
                );
            }

            for draw_primitive_idx in ctrl.draw_range.clone() {
                let draw_primitive = &draw_primitives[draw_primitive_idx];
                match draw_primitive {
                    DrawPrimitive::Rect {
                        rect,
                        texture_rect,
                        texture_id,
                        color,
                    } => {
                        draw_list.draw_rect(
                            *rect + ctrl_rect_absolute.min_point() - ctrl.scroll_offset,
                            *texture_rect,
                            *color,
                            ctrl_scissor_rect,
                            *texture_id,
                        );
                    }
                }
            }

            if ctrl.layout == Layout::Free {
                // For free layout, we'd like to preserve render order of
                // controls, e.g. we render least recently active control first,
                // then a more recently active control, all the way up to the
                // currently active control. To that end, we sort the the
                // siblings by last frame in active path.
                let mut siblings: Vec<(usize, u32), _> = Vec::new_in(temp);
                if let Some(child_idx) = ctrl.child_idx {
                    let mut ctrl = &tree[child_idx];

                    siblings.push((child_idx, ctrl.last_frame_in_active_path));

                    while let Some(sibling_idx) = ctrl.sibling_idx {
                        ctrl = &tree[sibling_idx];
                        siblings.push((sibling_idx, ctrl.last_frame_in_active_path));
                    }
                }

                siblings.sort_unstable_by_key(|&(_, frame)| frame);

                for (sibling_idx, _) in siblings {
                    render(
                        temp,
                        tree,
                        sibling_idx,
                        ctrl_scissor_rect,
                        draw_primitives,
                        font_atlas_texture_id,
                        draw_list,
                    );
                }
            } else {
                // For horizontal and vertical layouts, we don't need any
                // sorting and just iterate over the controls in definition
                // order.
                if let Some(child_idx) = ctrl.child_idx {
                    render(
                        temp,
                        tree,
                        child_idx,
                        ctrl_scissor_rect,
                        draw_primitives,
                        font_atlas_texture_id,
                        draw_list,
                    );

                    let mut child = &tree[child_idx];
                    while let Some(sibling_idx) = child.sibling_idx {
                        child = &tree[sibling_idx];

                        render(
                            temp,
                            tree,
                            sibling_idx,
                            ctrl_scissor_rect,
                            draw_primitives,
                            font_atlas_texture_id,
                            draw_list,
                        );
                    }
                }
            }
        }

        self.build_parent_idx = None;
        self.build_sibling_idx = None;

        // Clear inputs from platform to GUI.
        self.scroll_delta = Vec2::ZERO;
        self.inputs_pressed = Inputs::NONE;
        self.inputs_released = Inputs::NONE;
        self.received_characters.clear();
    }
}

pub struct Frame<'a> {
    ui: &'a mut Ui,
}

impl Frame<'_> {
    pub fn want_capture_keyboard(&self) -> bool {
        self.ui.want_capture_keyboard
    }

    pub fn want_capture_mouse(&self) -> bool {
        self.ui.want_capture_mouse
    }

    pub fn request_capture_keyboard(&mut self) {
        self.ui.want_capture_keyboard = true;
    }

    pub fn request_capture_mouse(&mut self) {
        self.ui.want_capture_mouse = true;
    }

    // TODO(jt): Return a mutable reference to the control, so that the caller can configure it?
    pub fn push_ctrl(&mut self, ctrl_id: CtrlId) {
        // Push a control onto the tree. The control can either be completely
        // new, or already present in the tree from previous frame. Controls are
        // identified by their ID, which has to be unique among children of a
        // single control.
        //
        // Whether inserting a new control or updating an existing one, we must
        // never unlink a control already present in the tree, because it may
        // yet be updated later this frame and we would have lost its state.
        //
        // Updated controls are first temporarily unlinked from the tree, and
        // then re-inserted at the current position. This means that dead
        // controls (if any) will be located after the live controls once the UI
        // is built.
        //
        // Current position in the tree is tracked by two indices:
        // build_parent_idx and build_sibling_idx, pointing to the current
        // parent and last inserted sibling, respectively.
        //
        // Note: Changing the control's layout options invalidates the layout
        // from last frame for this control and all its sibling successors, and
        // their children, but so does re-ordering, or not updating a control
        // from earlier frame. The layout will become valid on the next frame
        // once again.

        let build_parent_idx = self.ui.build_parent_idx.unwrap();
        let draw_range = {
            let next_idx = self.ui.draw_primitives.len();
            next_idx..next_idx
        };

        // TODO(yan): @Speed only search from build_sibling.sibling_idx, if
        // build_sibling already exists.
        let found_idx_and_prev_idx = {
            let parent = &self.ui.ctrl_tree[build_parent_idx];

            // TODO(yan): @Speed This is quadratic. Not great.
            if let Some(child_idx) = parent.child_idx {
                let mut ctrl = &mut self.ui.ctrl_tree[child_idx];

                if ctrl.id == ctrl_id {
                    Some((child_idx, None))
                } else {
                    let mut result = None;

                    let mut ctrl_idx = child_idx;
                    while let Some(sibling_idx) = ctrl.sibling_idx {
                        let prev_ctrl_idx = ctrl_idx;
                        ctrl_idx = sibling_idx;
                        ctrl = &mut self.ui.ctrl_tree[sibling_idx];

                        if ctrl.id == ctrl_id {
                            result = Some((ctrl_idx, Some(prev_ctrl_idx)));
                            break;
                        }
                    }

                    result
                }
            } else {
                None
            }
        };

        let current_idx = if let Some((found_idx, found_prev_idx)) = found_idx_and_prev_idx {
            let ctrl = &mut self.ui.ctrl_tree[found_idx];

            // We do not support re-entrancy. Controls can only be updated
            // once. This simplifies things:
            //
            // - We know that found_idx != build_sibling_idx, because the build
            //   sibling would have to be pushed and popped before,
            //
            // - We know that found_idx hasn't been pushed yet.
            //
            // TODO(yan): @Correctness This assert goes off if we render the
            // component only on some frames (discoverd by drawing a conditional
            // window in the game). We most definitely were not updating the
            // same component multiple times per frame, so this is an issue with
            // unlinking dead controls and/or GC?
            assert!(
                ctrl.last_frame != self.ui.current_frame,
                "Attempt to update the same control ({ctrl_id:?}) twice in one frame",
            );

            ctrl.last_frame = self.ui.current_frame;
            ctrl.inline_content_rect = None;
            ctrl.draw_range = draw_range;

            // After updating the control's data, we unlink the control from its
            // original place and re-link as either the next sibling of the
            // build sibling (if build sibling already exists) or first child of
            // the build parent (if the build sibling doesn't exist yet).
            if let Some(found_prev_idx) = found_prev_idx {
                self.ui.ctrl_tree[found_prev_idx].sibling_idx = ctrl.sibling_idx;
            }

            // Re-link the control as next sibling of the build sibling or
            // as first child of build parent (in case there is no build
            // sibling yet).
            if let Some(build_sibling_idx) = self.ui.build_sibling_idx {
                let build_sibling = &mut self.ui.ctrl_tree[build_sibling_idx];
                let build_sibling_next_sibling_idx = build_sibling.sibling_idx;

                // If we are already positioned correctly, relinking would
                // create a cycle.
                if build_sibling_next_sibling_idx != Some(found_idx) {
                    build_sibling.sibling_idx = Some(found_idx);
                    self.ui.ctrl_tree[found_idx].sibling_idx = build_sibling_next_sibling_idx;
                }
            } else {
                let build_parent = &mut self.ui.ctrl_tree[build_parent_idx];
                let build_parent_child_idx = build_parent.child_idx;

                // If we are already positioned correctly, relinking would
                // create a cycle.
                if build_parent_child_idx != Some(found_idx) {
                    build_parent.child_idx = Some(found_idx);
                    self.ui.ctrl_tree[found_idx].sibling_idx = build_parent_child_idx;
                }
            }

            found_idx
        } else {
            let idx = self.ui.ctrl_tree.len();

            // Preserve links to controls from previous frame so that they can be
            // found by future calls to push_ctrl in this subtree and depth.
            let sibling_idx = if let Some(build_sibling_idx) = self.ui.build_sibling_idx {
                let build_sibling = &mut self.ui.ctrl_tree[build_sibling_idx];
                let build_sibling_next_sibling_idx = build_sibling.sibling_idx;

                build_sibling.sibling_idx = Some(idx);
                build_sibling_next_sibling_idx
            } else {
                let build_parent = &mut self.ui.ctrl_tree[build_parent_idx];
                let build_parent_child_idx = build_parent.child_idx;

                build_parent.child_idx = Some(idx);
                build_parent_child_idx
            };

            self.ui.ctrl_tree.push(CtrlNode {
                id: ctrl_id,

                parent_idx: Some(build_parent_idx),
                child_idx: None,
                sibling_idx,

                strings_idx: None,

                first_frame: self.ui.current_frame,
                last_frame: self.ui.current_frame,
                last_frame_in_active_path: 0,

                flags: CtrlFlags::NONE,
                layout: Layout::Free,
                rect: Box2::ZERO,
                padding: 0.0,
                border: 0.0,
                margin: 0.0,

                inline_content_rect: None,

                scroll_offset: Vec2::ZERO,

                state: CtrlState([0; 64]),

                draw_self: false,
                draw_self_border_color: 0,
                draw_self_background_color: 0,
                draw_range,

                layout_cache_absolute_position: Vec2::ZERO,
                layout_cache_content_size: Vec2::ZERO,
            });

            idx
        };

        self.ui.build_parent_idx = Some(current_idx);
        self.ui.build_sibling_idx = None;
    }

    pub fn pop_ctrl(&mut self) {
        let build_parent_idx = self.ui.build_parent_idx.unwrap();

        // Finalize the parent and last inserted sibling controls and clean
        // their indices so that they only reference live controls. If no child
        // controls were inserted, clear the parent's child references (which
        // could contain dead controls from previous frame). Also cut off the
        // dead sibling controls of the last sibling here, so that they are not
        // reachable.

        // TODO(yan): @Correctness Assert that push_ctrl and pop_ctrl are
        // parenthesized correctly! Count current tree depth and assert
        // something in both pop_ctrl and end_frame?

        let build_parent = &mut self.ui.ctrl_tree[build_parent_idx];
        let build_parent_parent_idx = build_parent.parent_idx;

        if let Some(build_sibling_idx) = self.ui.build_sibling_idx {
            self.ui.ctrl_tree[build_sibling_idx].sibling_idx = None;
        } else {
            build_parent.child_idx = None;
        }

        self.ui.build_parent_idx = build_parent_parent_idx;
        self.ui.build_sibling_idx = Some(build_parent_idx);
    }

    pub fn begin_overlay(&mut self) {
        assert!(!self.ui.building_overlay);

        mem::swap(&mut self.ui.build_parent_idx, &mut self.ui.overlay_build_parent_idx);
        mem::swap(&mut self.ui.build_sibling_idx, &mut self.ui.overlay_build_sibling_idx);

        self.ui.building_overlay = true;
    }

    pub fn end_overlay(&mut self) {
        assert!(self.ui.building_overlay);

        mem::swap(&mut self.ui.build_parent_idx, &mut self.ui.overlay_build_parent_idx);
        mem::swap(&mut self.ui.build_sibling_idx, &mut self.ui.overlay_build_sibling_idx);

        self.ui.building_overlay = false;
    }

    pub fn font_atlas(&self) -> &FontAtlas<&'static Arena> {
        &self.ui.font_atlas
    }

    pub fn font_atlas_texture_id(&self) -> u64 {
        self.ui.font_atlas_texture_id
    }

    pub fn window_size(&self) -> Vec2 {
        self.ui.window_size
    }

    pub fn cursor_position(&self) -> Vec2 {
        self.ui.cursor_position
    }

    pub fn inputs_pressed(&self) -> Inputs {
        self.ui.inputs_pressed
    }

    pub fn inputs_released(&self) -> Inputs {
        self.ui.inputs_released
    }

    pub fn modifiers(&self) -> Modifiers {
        self.ui.modifiers
    }

    pub fn received_characters(&self) -> &str {
        &self.ui.received_characters
    }

    pub fn current_frame(&self) -> u32 {
        self.ui.current_frame
    }

    pub fn current_frame_time(&self) -> Nanos {
        self.ui.current_frame_time
    }

    pub fn ctrl_is_new(&self) -> bool {
        if let Some(build_parent_idx) = self.ui.build_parent_idx {
            self.ui.ctrl_tree[build_parent_idx].first_frame == self.ui.current_frame
        } else {
            false
        }
    }

    pub fn ctrl_is_hovered(&self) -> bool {
        self.ui.build_parent_idx == self.ui.hovered_capturing_ctrl_idx
    }

    pub fn ctrl_is_active(&self) -> bool {
        self.ui.active_ctrl_idx == self.ui.build_parent_idx
    }

    pub fn ctrl_scroll_offset_x(&self) -> f32 {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].scroll_offset.x
    }

    pub fn ctrl_scroll_offset_y(&self) -> f32 {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].scroll_offset.y
    }

    pub fn ctrl_set_active(&mut self, active: bool) {
        let self_idx = self.ui.build_parent_idx.unwrap();

        if active {
            self.ui.active_ctrl_idx = Some(self_idx);

            let mut ctrl = &mut self.ui.ctrl_tree[self_idx];
            ctrl.last_frame_in_active_path = self.ui.current_frame;

            while let Some(ctrl_idx) = ctrl.parent_idx {
                ctrl = &mut self.ui.ctrl_tree[ctrl_idx];
                ctrl.last_frame_in_active_path = self.ui.current_frame;
            }
        } else if let Some(active_ctrl_idx) = self.ui.active_ctrl_idx {
            if active_ctrl_idx == self_idx {
                // If this was the active control, it relinquishes the active
                // status the the first control up the tree that wants to
                // capture it. When that happens, the capturing control and all
                // its parents get their last_frame_in_active_path updated.

                let current_ctrl = &self.ui.ctrl_tree[self_idx];

                if let Some(parent_idx) = current_ctrl.parent_idx {
                    let mut ctrl_idx = parent_idx;
                    let mut ctrl = &mut self.ui.ctrl_tree[parent_idx];

                    while !ctrl.flags.intersects(CtrlFlags::CAPTURE_ACTIVE) && ctrl.parent_idx.is_some() {
                        ctrl_idx = ctrl.parent_idx.unwrap();
                        ctrl = &mut self.ui.ctrl_tree[ctrl_idx];
                    }

                    if ctrl.flags.intersects(CtrlFlags::CAPTURE_ACTIVE) {
                        self.ui.active_ctrl_idx = Some(ctrl_idx);

                        ctrl.last_frame_in_active_path = self.ui.current_frame;

                        while let Some(ctrl_idx) = ctrl.parent_idx {
                            ctrl = &mut self.ui.ctrl_tree[ctrl_idx];
                            ctrl.last_frame_in_active_path = self.ui.current_frame
                        }
                    } else {
                        self.ui.active_ctrl_idx = None;
                    }
                }
            }
        }
    }

    pub fn ctrl_set_flags(&mut self, flags: CtrlFlags) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].flags = flags;
    }

    pub fn ctrl_set_layout(&mut self, layout: Layout) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].layout = layout;
    }

    pub fn ctrl_set_rect(&mut self, rect: Box2) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].rect = rect;
    }

    pub fn ctrl_set_padding(&mut self, padding: f32) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].padding = padding;
    }

    pub fn ctrl_set_border(&mut self, border: f32) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].border = border;
    }

    pub fn ctrl_set_margin(&mut self, margin: f32) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].margin = margin;
    }

    pub fn ctrl_set_scroll_offset_x(&mut self, scroll_offset: f32) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].scroll_offset.x = scroll_offset;
    }

    pub fn ctrl_set_scroll_offset_y(&mut self, scroll_offset: f32) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].scroll_offset.y = scroll_offset;
    }

    pub fn ctrl_set_draw_self(&mut self, draw_self: bool) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].draw_self = draw_self;
    }

    pub fn ctrl_set_draw_self_border_color(&mut self, border_color: u32) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].draw_self_border_color = border_color;
    }

    pub fn ctrl_set_draw_self_background_color(&mut self, background_color: u32) {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].draw_self_background_color = background_color;
    }

    pub fn ctrl_state(&self) -> &CtrlState {
        &self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].state
    }

    pub fn ctrl_state_mut(&mut self) -> &mut CtrlState {
        &mut self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].state
    }

    pub fn ctrl_string(&self) -> &ArrayString<256> {
        if let Some(idx) = self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].strings_idx {
            &self.ui.ctrl_strings[idx]
        } else {
            DEFAULT_STRING
        }
    }

    pub fn ctrl_string_mut(&mut self) -> &mut ArrayString<256> {
        let self_idx = self.ui.build_parent_idx.unwrap();

        if let Some(idx) = self.ui.ctrl_tree[self_idx].strings_idx {
            &mut self.ui.ctrl_strings[idx]
        } else {
            let idx = self.ui.ctrl_strings.push(ArrayString::new());
            self.ui.ctrl_tree[self_idx].strings_idx = Some(idx);

            &mut self.ui.ctrl_strings[idx]
        }
    }

    pub fn ctrl_absolute_position(&self) -> Vec2 {
        self.ui.ctrl_tree[self.ui.build_parent_idx.unwrap()].layout_cache_absolute_position
    }

    pub fn ctrl_inner_size(&self) -> Vec2 {
        let build_parent_idx = self.ui.build_parent_idx.unwrap();
        let parent = &self.ui.ctrl_tree[build_parent_idx];
        let rect = parent.rect.inset(parent.border + parent.padding);

        rect.size()
    }

    pub fn ctrl_count(&self) -> usize {
        self.ui.ctrl_count()
    }

    pub fn get_clipboard_getter(&self) -> fn() -> String {
        self.ui.clipboard_getter
    }

    pub fn get_clipboard_setter(&self) -> fn(&str) {
        self.ui.clipboard_setter
    }

    pub fn ctrl_draw_rect(&mut self, rect: Box2, texture_rect: Box2, color: u32, texture_id: u64) {
        let build_parent_idx = self.ui.build_parent_idx.unwrap();
        let next_draw_primitive_idx = self.ui.draw_primitives.len();

        let parent = &mut self.ui.ctrl_tree[build_parent_idx];
        assert!(
            parent.draw_range.end == next_draw_primitive_idx,
            "Drawing must happen before pushing child controls",
        );

        self.ui.draw_primitives.push(DrawPrimitive::Rect {
            rect,
            texture_rect,
            texture_id,
            color: srgb_to_linear(color),
        });

        parent.draw_range.end += 1;
    }

    pub fn ctrl_draw_text(&mut self, text: &str, halign: Align, valign: Align, wrap: Wrap, color: u32) {
        self.draw_text(false, None, 0.0, text, halign, valign, wrap, color);
    }

    pub fn ctrl_draw_text_fitted(
        &mut self,
        text: &str,
        halign: Align,
        valign: Align,
        wrap: Wrap,
        color: u32,
        fitting: Box2,
    ) {
        self.draw_text(true, Some(fitting), 0.0, text, halign, valign, wrap, color);
    }

    pub fn ctrl_draw_text_inset_and_extend_content_rect(
        &mut self,
        text: &str,
        halign: Align,
        valign: Align,
        wrap: Wrap,
        color: u32,
        inset: f32,
    ) {
        self.draw_text(true, None, inset, text, halign, valign, wrap, color);
    }

    fn draw_text(
        &mut self,
        extend_inline_content_rect: bool,
        fitting: Option<Box2>,
        inset: f32,
        text: &str,
        halign: Align,
        valign: Align,
        wrap: Wrap,
        color: u32,
    ) {
        let color = srgb_to_linear(color);

        assert!(inset >= 0.0);

        let build_parent_idx = self.ui.build_parent_idx.unwrap();
        let next_draw_primitive_idx = self.ui.draw_primitives.len();

        let parent = &mut self.ui.ctrl_tree[build_parent_idx];

        assert!(parent.draw_range.end == next_draw_primitive_idx);

        // Vertical align only makes sense, if there is any free space to align
        // in. If we are going to shrink/resize, there is no free space and it
        // simplifies things for us to align to start and not care later.
        //
        // Note that horizontal align still makes sense for shrinking, because
        // the lines will still be jagged and the width difference between
        // longest line and current line will provide the alignment space.
        let valign = if parent.flags.intersects(CtrlFlags::RESIZE_TO_FIT_VERTICAL) {
            Align::Start
        } else {
            valign
        };

        // We zero X and Y of the default parent rect, because emiting draw
        // commands insider a control already uses that control's transform. Not
        // zeroing would apply them twice.
        let fitting = fitting
            .unwrap_or_else(|| Box2::new(0.0, 0.0, parent.rect.width, parent.rect.height))
            .inset(inset);
        let available_width = fitting.width;
        let available_height = fitting.height;

        // If we are expected to wrap text, but there's not enough space to
        // render a missing character, don't attempt anything.
        if wrap != Wrap::None && self.ui.font_atlas.missing_glyph_info().advance_width > available_width {
            return;
        }

        struct Line {
            idx_start: usize,
            idx_end: usize,
            width: f32,
        }

        let mut lines: Vec<Line, _> = Vec::new_in(&self.ui.temp_arena);

        let mut last_char_was_whitespace = false;
        let mut begun_word: bool;
        let mut begun_word_start = 0;

        let mut line_idx_start = 0;
        let mut line_idx_end = 0;
        let mut line_width = 0.0;

        for (i, c) in text.char_indices() {
            begun_word = !c.is_whitespace();
            if last_char_was_whitespace && !c.is_whitespace() {
                begun_word_start = i;
            }
            last_char_was_whitespace = c.is_whitespace();

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

            let glyph_info = self.ui.font_atlas.glyph_info(c);
            let glyph_advance_width = glyph_info.advance_width;

            if line_width + glyph_advance_width > available_width {
                match wrap {
                    Wrap::Word => {
                        let begun_word_width = if begun_word {
                            let slice = &text[begun_word_start..i];

                            let mut width = 0.0;
                            for c in slice.chars() {
                                width += self.ui.font_atlas.glyph_info(c).advance_width;
                            }

                            width
                        } else {
                            0.0
                        };

                        if !begun_word || begun_word_width + glyph_advance_width > available_width {
                            // If we are not inside a word right now, or the
                            // begun word is wide enough to cause wrapping by
                            // itself, fall back to letter wrapping.
                            lines.push(Line {
                                idx_start: line_idx_start,
                                idx_end: line_idx_end,
                                width: line_width,
                            });

                            line_idx_start = i;
                            line_idx_end = i + c.len_utf8();
                            line_width = glyph_advance_width;
                        } else {
                            // Otherwise commit previous line and move the word
                            // to the next.
                            lines.push(Line {
                                idx_start: line_idx_start,
                                idx_end: begun_word_start,
                                width: line_width - begun_word_width,
                            });

                            line_idx_start = begun_word_start;
                            line_idx_end = i + c.len_utf8();
                            line_width = begun_word_width + glyph_advance_width;
                        }

                        continue;
                    }
                    Wrap::Letter => {
                        lines.push(Line {
                            idx_start: line_idx_start,
                            idx_end: line_idx_end,
                            width: line_width,
                        });

                        line_idx_start = i;
                        line_idx_end = i + c.len_utf8();
                        line_width = glyph_advance_width;

                        continue;
                    }
                    Wrap::None => (),
                }
            }

            line_idx_end += c.len_utf8();
            line_width += glyph_advance_width;
        }

        lines.push(Line {
            idx_start: line_idx_start,
            idx_end: line_idx_end,
            width: line_width,
        });

        //
        // Trim whitespace.
        //
        // Shorten ranges and decrease widths. The widths can only be decreased
        // here, because the lines were already split and the whitespace widths
        // already contributed to computing text wrap.
        for line in &mut lines {
            let line_slice = &text[line.idx_start..line.idx_end];

            let mut start = line.idx_start;
            let mut end = line.idx_end;
            let mut trim_width = 0.0;

            for c in line_slice.chars() {
                if !c.is_whitespace() {
                    break;
                }

                start += c.len_utf8();
                trim_width += self.ui.font_atlas.glyph_info(c).advance_width;
            }

            let mut rev_iter = line_slice.chars().rev().peekable();
            while let Some(c) = rev_iter.next() {
                if !c.is_whitespace() {
                    break;
                }

                if rev_iter.peek().is_some() {
                    end -= c.len_utf8();
                    trim_width += self.ui.font_atlas.glyph_info(c).advance_width;
                }
            }

            if start > end {
                start = end;
            }

            line.idx_start = start;
            line.idx_end = end;
            line.width = f32::max(line.width - trim_width, 0.0)
        }

        //
        // Emit rects based on generated line data.
        //
        let line_metrics = self.ui.font_atlas.font_horizontal_line_metrics();

        let mut position_y = if lines.len() as f32 * line_metrics.new_line_size < available_height {
            match valign {
                Align::Start => line_metrics.line_gap + fitting.y,
                Align::Center => {
                    let line_gap = line_metrics.line_gap;
                    let new_line_size = line_metrics.new_line_size;
                    let text_block_size = new_line_size * lines.len() as f32 - line_gap;

                    line_gap + fitting.y + (available_height - text_block_size) / 2.0
                }
                Align::End => {
                    let line_gap = line_metrics.line_gap;
                    let new_line_size = line_metrics.new_line_size;
                    let text_block_size = new_line_size * lines.len() as f32 - line_gap;

                    line_gap + fitting.y + available_height - text_block_size
                }
            }
        } else {
            line_metrics.line_gap + fitting.y
        };

        for line in &lines {
            let line_slice = &text[line.idx_start..line.idx_end];

            let mut position_x = match halign {
                Align::Start => fitting.x,
                Align::Center => fitting.x + (available_width - line.width) / 2.0,
                Align::End => fitting.x + available_width - line.width,
            };

            for c in line_slice.chars() {
                let glyph_info = self.ui.font_atlas.glyph_info(c);

                let position = Vec2::new(position_x, position_y);
                let rect = glyph_info.rect + position + vec2(0.0, line_metrics.ascent);

                // TODO(yan): @Speed @Memory Does early software scissor make
                // sense here? We also do it later, when translating to the
                // low-level draw list, but we could have less things to
                // translate.
                self.ui.draw_primitives.push(DrawPrimitive::Rect {
                    rect,
                    texture_rect: glyph_info.atlas_rect,
                    texture_id: self.ui.font_atlas_texture_id,
                    color,
                });

                parent.draw_range.end += 1;
                if extend_inline_content_rect {
                    if let Some(inline_content_rect) = &mut parent.inline_content_rect {
                        *inline_content_rect = inline_content_rect.extend_by_box(rect);
                    } else {
                        parent.inline_content_rect = Some(rect);
                    }
                }

                position_x += glyph_info.advance_width;
            }

            position_y += line_metrics.new_line_size;
        }

        // Because this isn't real padding/border, we need to ensure that if we
        // used inset, the final content rect reflects that. This happens
        // automatically for top and left, but we need to add the inset to its
        // size.
        if extend_inline_content_rect {
            if let Some(inline_content_rect) = &mut parent.inline_content_rect {
                *inline_content_rect = inline_content_rect.resize(Vec2::splat(inset));
            }
        }
    }

    pub fn perm_arena_allocated_size(&self) -> usize {
        self.ui.perm_arena.allocated_size()
    }

    pub fn perm_arena_reserved_size(&self) -> usize {
        self.ui.perm_arena.reserved_size()
    }

    pub fn temp_arena_allocated_size(&self) -> usize {
        self.ui.temp_arena.allocated_size()
    }

    pub fn temp_arena_reserved_size(&self) -> usize {
        self.ui.temp_arena.reserved_size()
    }
}

fn empty_clipboard_getter() -> String {
    String::new()
}

fn empty_clipboard_setter(_: &str) {}
