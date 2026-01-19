#![no_std]
#![feature(allocator_api)]

extern crate alloc;

use alloc::vec::Vec;
use core::alloc::Allocator;
use core::fmt::Write;

use wfcommon::cast_i8;
use wfcommon::cast_i16;
use wfcommon::cast_i32;
use wfcommon::cast_u8;
use wfcommon::cast_u16;
use wfcommon::cast_u32;
pub use wfedit_derive::Edit;
use wfinlinevec::InlineString;
use wfinlinevec::InlineVec;
use wfmath::IBox2;
use wfmath::IVec2;
use wfmath::IVec3;
use wfmath::IVec4;
use wfmath::Vec2;
use wfmath::Vec3;
use wfmath::Vec4;

pub trait Edit {
    fn edit(&mut self, f: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool;
}

impl Edit for bool {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::checkbox_ex(
            frame,
            id,
            self,
            label,
            wfgui::CheckboxSettings {
                readonly,
                ..wfgui::CheckboxSettings::default()
            },
        )
    }
}

impl Edit for i8 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut value = i32::from(*self);

        let modified = wfgui::int_slider_ex(
            frame,
            id,
            &mut value,
            label,
            wfgui::IntSliderSettings {
                readonly,

                min: i32::from(Self::MIN),
                max: i32::from(Self::MAX),

                ..wfgui::IntSliderSettings::default()
            },
        );

        *self = cast_i8(value);

        modified
    }
}

impl Edit for i16 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut value = i32::from(*self);

        let modified = wfgui::int_slider_ex(
            frame,
            id,
            &mut value,
            label,
            wfgui::IntSliderSettings {
                readonly,

                min: i32::from(Self::MIN),
                max: i32::from(Self::MAX),

                ..wfgui::IntSliderSettings::default()
            },
        );

        *self = cast_i16(value);

        modified
    }
}

impl Edit for i32 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::int_slider_ex(
            frame,
            id,
            self,
            label,
            wfgui::IntSliderSettings {
                readonly,
                ..wfgui::IntSliderSettings::default()
            },
        )
    }
}

impl Edit for u8 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut value = i32::from(*self);

        let modified = wfgui::int_slider_ex(
            frame,
            id,
            &mut value,
            label,
            wfgui::IntSliderSettings {
                readonly,

                min: i32::from(Self::MIN),
                max: i32::from(Self::MAX),

                ..wfgui::IntSliderSettings::default()
            },
        );

        *self = cast_u8(value);

        modified
    }
}

impl Edit for u16 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut value = i32::from(*self);

        let modified = wfgui::int_slider_ex(
            frame,
            id,
            &mut value,
            label,
            wfgui::IntSliderSettings {
                readonly,

                min: i32::from(Self::MIN),
                max: i32::from(Self::MAX),

                ..wfgui::IntSliderSettings::default()
            },
        );

        *self = cast_u16(value);

        modified
    }
}

// TODO(jt): Edit for u32 is not very good to do, because it clamps the value unnecessarily. Make a
// generic scalar slider that can handle any numeric type.
impl Edit for u32 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        if *self > i32::MAX as u32 {
            *self = i32::MAX as u32;
        }
        let mut value = cast_i32(*self);

        let modified = wfgui::int_slider_ex(
            frame,
            id,
            &mut value,
            label,
            wfgui::IntSliderSettings {
                readonly,

                min: 0,
                max: i32::MAX,

                ..wfgui::IntSliderSettings::default()
            },
        );

        *self = cast_u32(value);

        modified
    }
}

impl Edit for f32 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::float_slider_ex(
            frame,
            id,
            self,
            label,
            wfgui::FloatSliderSettings {
                readonly,
                ..wfgui::FloatSliderSettings::default()
            },
        )
    }
}

// We currently don't impl the trait for short array counterparts of Vec/IVec,
// because that impl would conflict with the generic list impl for arbitrary
// arrays [T; N].

impl Edit for IVec2 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::int2_slider_ex(
            frame,
            id,
            self.as_mut(),
            label,
            wfgui::IntSliderSettings {
                readonly,
                ..wfgui::IntSliderSettings::default()
            },
        )
    }
}

impl Edit for IVec3 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::int3_slider_ex(
            frame,
            id,
            self.as_mut(),
            label,
            wfgui::IntSliderSettings {
                readonly,
                ..wfgui::IntSliderSettings::default()
            },
        )
    }
}

impl Edit for IVec4 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::int4_slider_ex(
            frame,
            id,
            self.as_mut(),
            label,
            wfgui::IntSliderSettings {
                readonly,
                ..wfgui::IntSliderSettings::default()
            },
        )
    }
}

impl Edit for IBox2 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut a = [self.x, self.y, self.width, self.height];

        let modified = wfgui::int4_slider_ex(
            frame,
            id,
            &mut a,
            label,
            wfgui::IntSliderSettings {
                readonly,
                ..wfgui::IntSliderSettings::default()
            },
        );

        let x = a[0];
        let y = a[1];
        // Preserve nonnegative dimensions
        let width = i32::max(0, a[2]);
        let height = i32::max(0, a[3]);

        // We don't see IBox2 fields Rect here, so we need to construct a new one.. eh.
        let r = IBox2::new(x, y, width, height);
        *self = r;

        modified
    }
}

impl Edit for Vec2 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::float2_slider_ex(
            frame,
            id,
            self.as_mut(),
            label,
            wfgui::FloatSliderSettings {
                readonly,
                ..wfgui::FloatSliderSettings::default()
            },
        )
    }
}

impl Edit for Vec3 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::float3_slider_ex(
            frame,
            id,
            self.as_mut(),
            label,
            wfgui::FloatSliderSettings {
                readonly,
                ..wfgui::FloatSliderSettings::default()
            },
        )
    }
}

impl Edit for Vec4 {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        wfgui::float4_slider_ex(
            frame,
            id,
            self.as_mut(),
            label,
            wfgui::FloatSliderSettings {
                readonly,
                ..wfgui::FloatSliderSettings::default()
            },
        )
    }
}

impl<const N: usize> Edit for InlineString<N> {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let (changed, _) = wfgui::text_input_ex(
            frame,
            id,
            self,
            label,
            wfgui::TextInputSettings {
                readonly,
                ..wfgui::TextInputSettings::default()
            },
        );
        changed
    }
}

// TODO(jt): @Cleanup This should use const generics, but Default impls for
// arrays don't use const generics yet.
macro_rules! impl_edit_for_array {
    ( $n:expr ) => {
        impl<T: Edit> Edit for [T; $n] {
            fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
                let mut modified = false;

                if let Some(panel) = wfgui::begin_panel_with_fit_height(frame, id, "100%", label) {
                    for (i, value) in self.iter_mut().enumerate() {
                        let mut s: InlineString<16> = InlineString::new();
                        let _ = write!(s, "[{i}]:");

                        modified |= value.edit(frame, wfgui::id!(cast_u32(i)), &s, readonly);
                    }

                    panel.end(frame);
                }

                modified
            }
        }
    };
}

impl_edit_for_array! { 0 }
impl_edit_for_array! { 1 }
impl_edit_for_array! { 2 }
impl_edit_for_array! { 3 }
impl_edit_for_array! { 4 }
impl_edit_for_array! { 5 }
impl_edit_for_array! { 6 }
impl_edit_for_array! { 7 }
impl_edit_for_array! { 8 }
impl_edit_for_array! { 9 }
impl_edit_for_array! { 10 }
impl_edit_for_array! { 11 }
impl_edit_for_array! { 12 }
impl_edit_for_array! { 13 }
impl_edit_for_array! { 14 }
impl_edit_for_array! { 15 }
impl_edit_for_array! { 16 }
impl_edit_for_array! { 17 }
impl_edit_for_array! { 18 }
impl_edit_for_array! { 19 }
impl_edit_for_array! { 20 }
impl_edit_for_array! { 21 }
impl_edit_for_array! { 22 }
impl_edit_for_array! { 23 }
impl_edit_for_array! { 24 }
impl_edit_for_array! { 25 }
impl_edit_for_array! { 26 }
impl_edit_for_array! { 27 }
impl_edit_for_array! { 28 }
impl_edit_for_array! { 29 }
impl_edit_for_array! { 30 }
impl_edit_for_array! { 31 }
impl_edit_for_array! { 32 }

impl<T: Edit + Default, VA: Allocator> Edit for Vec<T, VA> {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut modified = false;
        let mut remove = None;

        if let Some(panel) = wfgui::begin_panel_with_fit_height(frame, id, "100%", label) {
            for (i, value) in self.iter_mut().enumerate() {
                if let Some(panel) = wfgui::begin_panel_with_layout_fit_height_options(
                    frame,
                    wfgui::id!(cast_u32(i)),
                    "100%",
                    label,
                    wfgui::Layout::Vertical,
                    &wfgui::PanelOptions {
                        draw_padding: false,
                        draw_border: false,
                        draw_header: false,
                    },
                ) {
                    let mut s: InlineString<16> = InlineString::new();
                    let _ = write!(s, "[{i}]:");

                    if !readonly {
                        if wfgui::button(frame, wfgui::id!(0), "-") {
                            remove = Some(i);
                        }
                    }

                    modified |= value.edit(frame, wfgui::id!(1), &s, readonly);

                    panel.end(frame)
                }
            }

            if !readonly {
                if wfgui::button(frame, wfgui::id!(cast_u32(self.len())), "+") {
                    self.push(T::default());
                    modified = true;
                }
            }

            panel.end(frame);
        }

        if let Some(i) = remove {
            self.remove(i);
            modified = true;
        }

        modified
    }
}

impl<T: Clone + Copy + Default + Edit, const N: usize> Edit for InlineVec<T, N> {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut modified = false;
        let mut remove = None;

        if let Some(panel) = wfgui::begin_panel_with_fit_height(frame, id, "100%", label) {
            for (i, value) in self.iter_mut().enumerate() {
                if let Some(panel) = wfgui::begin_panel_with_layout_fit_height_options(
                    frame,
                    wfgui::id!(i as u32), // as u32 is okay, because inlinevec length is a u16 internally
                    "100%",
                    label,
                    wfgui::Layout::Vertical,
                    &wfgui::PanelOptions {
                        draw_padding: false,
                        draw_border: false,
                        draw_header: false,
                    },
                ) {
                    let mut s: InlineString<16> = InlineString::new();
                    let _ = write!(s, "[{i}]:");

                    if !readonly {
                        if wfgui::button(frame, wfgui::id!(0), "-") {
                            remove = Some(i);
                        }
                    }

                    modified |= value.edit(frame, wfgui::id!(1), &s, readonly);

                    panel.end(frame)
                }
            }

            if !readonly && self.len() < self.capacity() {
                if wfgui::button(frame, wfgui::id!(cast_u32(self.len())), "+") {
                    self.push(T::default());
                    modified = true;
                }
            }

            panel.end(frame);
        }

        if let Some(i) = remove {
            self.remove(i);
            modified = true;
        }

        modified
    }
}

macro_rules! impl_edit_for_tuple {
    ( $( $ty:ident . $field:tt ),+ ) => {
        impl<$($ty: Edit,)+> Edit for ($($ty,)+) {
            fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
                let mut modified = false;

                if let Some(panel) = wfgui::begin_panel_with_fit_height(frame, id, "100%", label) {
                    let mut i: u32 = 0;

                    $(#[allow(unused_assignments)] {
                        let mut s: InlineString<16> = InlineString::new();
                        let _ = write!(s, "{i}:");

                        modified |= self.$field.edit(frame, wfgui::id!(i), &s, readonly);

                        i += 1;
                    })+

                    panel.end(frame);
                }

                modified
            }
        }
    }
}

impl_edit_for_tuple! { T0.0 }
impl_edit_for_tuple! { T0.0, T1.1 }
impl_edit_for_tuple! { T0.0, T1.1, T2.2 }
impl_edit_for_tuple! { T0.0, T1.1, T2.2, T3.3 }
impl_edit_for_tuple! { T0.0, T1.1, T2.2, T3.3, T4.4 }
impl_edit_for_tuple! { T0.0, T1.1, T2.2, T3.3, T4.4, T5.5 }
impl_edit_for_tuple! { T0.0, T1.1, T2.2, T3.3, T4.4, T5.5, T6.6 }
impl_edit_for_tuple! { T0.0, T1.1, T2.2, T3.3, T4.4, T5.5, T6.6, T7.7 }

impl<T: Edit + Default> Edit for Option<T> {
    fn edit(&mut self, frame: &mut wfgui::Frame, id: wfgui::CtrlId, label: &str, readonly: bool) -> bool {
        let mut modified = false;

        if let Some(panel) = wfgui::begin_panel_with_fit_height(frame, id, "100%", label) {
            let mut selected_name_index = match self {
                None => Some(0),
                Some(_) => Some(1),
            };
            let selected_name_index_orig = selected_name_index;

            modified |= wfgui::dropdown_ex(
                frame,
                wfgui::id!(0),
                "Option:",
                &["None", "Some"],
                &mut selected_name_index,
                wfgui::DropdownSettings {
                    readonly,
                    ..wfgui::DropdownSettings::default()
                },
            );

            if selected_name_index != selected_name_index_orig {
                match selected_name_index {
                    Some(0) => *self = None,
                    Some(1) => *self = Some(T::default()),
                    _ => unreachable!("Not so fast!"),
                }
            }

            match self {
                None => (),
                Some(value) => modified |= value.edit(frame, wfgui::id!(1), "0:", readonly),
            }

            panel.end(frame);
        }

        modified
    }
}
