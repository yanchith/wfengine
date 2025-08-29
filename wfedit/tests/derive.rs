#![feature(allocator_api)]

use core::ptr::NonNull;

use arrayvec::ArrayString;
use wfarena::Arena;
use wfedit::Edit;
use wfmath::Vec3;
use wftime::Nanos;

fn make_ui() -> wfgui::Ui {
    wfgui::Ui::new_in(
        1280.0,
        720.0,
        1.0,
        wfgui::FONT_IBM_PLEX_MONO,
        wfgui::UnicodeRangeFlags::ALL_LATIN,
        14.0,
        1.0,
        make_arena(),
        make_arena(),
    )
}

#[test]
fn test_struct() {
    #[derive(Debug, PartialEq)]
    #[derive(wfedit::Edit)]
    struct IceCream {
        flavor: ArrayString<20>,
        flavor_normal: Vec3,
        scoops: u16,
    }

    let mut ui = make_ui();
    let mut frame = ui.begin_frame(Nanos::ZERO);

    let mut ice_cream = IceCream {
        flavor: ArrayString::from("Peppermint").unwrap(),
        flavor_normal: Vec3::Z,
        scoops: 3,
    };

    ice_cream.edit(&mut frame, wfgui::id!(0), "My sweet sweet ice cream", false);

    ui.end_frame();
}

#[test]
fn test_enum() {
    #[derive(Debug, PartialEq)]
    #[derive(wfedit::Edit)]
    enum Snack {
        RiceCakes { count: u16 },
        IceCream { flavor: IceCreamFlavor, scoops: u16 },
        ChiaPudding(f32, u16, ArrayString<16>),
        Chocolate,
    }

    #[derive(Debug, PartialEq, Default)]
    #[derive(wfedit::Edit)]
    enum IceCreamFlavor {
        #[default]
        Strawberry,
        Mango,
        Cookies,
        Chocolate,
    }

    let mut ui = make_ui();
    let mut frame = ui.begin_frame(Nanos::ZERO);

    let mut snack = Snack::Chocolate;
    snack.edit(&mut frame, wfgui::id!(0), "My sweet sweet chocolate", false);

    ui.end_frame();
}

fn make_arena() -> Arena {
    let memory = vec![0u8; 1024 << 20];

    let memory_slice = memory.leak();
    let memory_ptr = NonNull::new(memory_slice.as_mut_ptr()).unwrap();
    let memory_len = memory_slice.len();

    let arena_result = unsafe { Arena::with_memory_block(memory_ptr, memory_len) };

    let arena = arena_result.unwrap();

    arena
}
