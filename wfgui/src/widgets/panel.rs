use core::fmt::Debug;

use wfmath::Box2;

use crate::core::Align;
use crate::core::CtrlFlags;
use crate::core::CtrlId;
use crate::core::Frame;
use crate::core::Layout;
use crate::core::Wrap;
use crate::id;
use crate::widgets::size::Size;
use crate::widgets::theme::Theme;

const DEFAULT_OPTIONS: PanelOptions = PanelOptions {
    draw_padding: true,
    draw_border: true,
    draw_header: true,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PanelOptions {
    pub draw_padding: bool,
    pub draw_border: bool,
    pub draw_header: bool,
}

impl Default for PanelOptions {
    fn default() -> Self {
        DEFAULT_OPTIONS
    }
}

#[inline]
pub fn begin_panel<W, H>(frame: &mut Frame, id: CtrlId, width: W, height: H, label: &str) -> Option<Panel>
where
    W: TryInto<Size>,
    H: TryInto<Size>,
    <W as TryInto<Size>>::Error: Debug,
    <H as TryInto<Size>>::Error: Debug,
{
    let width = width.try_into().unwrap();
    let height = height.try_into().unwrap();

    do_panel(
        frame,
        id,
        width,
        height,
        label,
        Layout::Vertical,
        false,
        &DEFAULT_OPTIONS,
        &Theme::DEFAULT,
    );

    Some(Panel(false))
}

#[inline]
pub fn begin_panel_with_layout<W, H>(
    frame: &mut Frame,
    id: CtrlId,
    width: W,
    height: H,
    label: &str,
    layout: Layout,
) -> Option<Panel>
where
    W: TryInto<Size>,
    H: TryInto<Size>,
    <W as TryInto<Size>>::Error: Debug,
    <H as TryInto<Size>>::Error: Debug,
{
    let width = width.try_into().unwrap();
    let height = height.try_into().unwrap();

    do_panel(
        frame,
        id,
        width,
        height,
        label,
        layout,
        false,
        &DEFAULT_OPTIONS,
        &Theme::DEFAULT,
    );

    Some(Panel(false))
}

#[inline]
pub fn begin_panel_with_fit_height<W>(frame: &mut Frame, id: CtrlId, width: W, label: &str) -> Option<Panel>
where
    W: TryInto<Size>,
    <W as TryInto<Size>>::Error: Debug,
{
    let width = width.try_into().unwrap();

    do_panel(
        frame,
        id,
        width,
        Size::new_absolute(0.0),
        label,
        Layout::Vertical,
        true,
        &DEFAULT_OPTIONS,
        &Theme::DEFAULT,
    );

    Some(Panel(false))
}

#[inline]
pub fn begin_panel_with_layout_fit_height<W>(
    frame: &mut Frame,
    id: CtrlId,
    width: W,
    label: &str,
    layout: Layout,
) -> Option<Panel>
where
    W: TryInto<Size>,
    <W as TryInto<Size>>::Error: Debug,
{
    let width = width.try_into().unwrap();

    do_panel(
        frame,
        id,
        width,
        Size::new_absolute(0.0),
        label,
        layout,
        true,
        &DEFAULT_OPTIONS,
        &Theme::DEFAULT,
    );

    Some(Panel(false))
}

#[inline]
pub fn begin_panel_with_layout_options<W, H>(
    frame: &mut Frame,
    id: CtrlId,
    width: W,
    height: H,
    label: &str,
    layout: Layout,
    options: &PanelOptions,
) -> Option<Panel>
where
    W: TryInto<Size>,
    H: TryInto<Size>,
    <W as TryInto<Size>>::Error: Debug,
    <H as TryInto<Size>>::Error: Debug,
{
    let width = width.try_into().unwrap();
    let height = height.try_into().unwrap();

    do_panel(frame, id, width, height, label, layout, false, options, &Theme::DEFAULT);

    Some(Panel(false))
}

#[inline]
pub fn begin_panel_with_layout_fit_height_options<W>(
    frame: &mut Frame,
    id: CtrlId,
    width: W,
    label: &str,
    layout: Layout,
    options: &PanelOptions,
) -> Option<Panel>
where
    W: TryInto<Size>,
    <W as TryInto<Size>>::Error: Debug,
{
    let width = width.try_into().unwrap();

    do_panel(
        frame,
        id,
        width,
        Size::new_absolute(0.0),
        label,
        layout,
        true,
        options,
        &Theme::DEFAULT,
    );

    Some(Panel(false))
}

#[inline]
pub fn begin_panel_with_layout_options_theme<W, H>(
    frame: &mut Frame,
    id: CtrlId,
    width: W,
    height: H,
    label: &str,
    layout: Layout,
    options: &PanelOptions,
    theme: &Theme,
) -> Option<Panel>
where
    W: TryInto<Size>,
    H: TryInto<Size>,
    <W as TryInto<Size>>::Error: Debug,
    <H as TryInto<Size>>::Error: Debug,
{
    let width = width.try_into().unwrap();
    let height = height.try_into().unwrap();

    do_panel(frame, id, width, height, label, layout, false, options, theme);

    Some(Panel(false))
}

pub struct Panel(bool);

impl Panel {
    // TODO(jt): @Cleanup Can we restructure the API such that Drop calls pop_ctrl, so we don't have
    // to? We might run into some borrowchecker issues, but it would be worth it.
    pub fn end(mut self, frame: &mut Frame) {
        assert!(!self.0);

        frame.pop_ctrl();
        frame.pop_ctrl();
        self.0 = true;
    }
}

impl Drop for Panel {
    fn drop(&mut self) {
        debug_assert!(self.0)
    }
}

fn do_panel(
    frame: &mut Frame,
    id: CtrlId,
    width: Size,
    height: Size,
    label: &str,
    layout: Layout,
    fit_height: bool,
    options: &PanelOptions,
    theme: &Theme,
) {
    let parent_size = frame.ctrl_inner_size();
    let outer_flags = if fit_height {
        CtrlFlags::RESIZE_TO_FIT_VERTICAL
    } else {
        CtrlFlags::NONE
    };
    let body_flags = if fit_height {
        CtrlFlags::CAPTURE_SCROLL | CtrlFlags::RESIZE_TO_FIT_VERTICAL
    } else {
        CtrlFlags::CAPTURE_SCROLL
    };

    let outer_width = f32::max(0.0, width.resolve(parent_size.x) - 2.0 * theme.panel_margin);
    let outer_height = f32::max(0.0, height.resolve(parent_size.y) - 2.0 * theme.panel_margin);

    frame.push_ctrl(id);
    frame.ctrl_set_flags(outer_flags);
    frame.ctrl_set_layout(Layout::Vertical);
    frame.ctrl_set_rect(Box2::new(0.0, 0.0, outer_width, outer_height));

    frame.ctrl_set_padding(0.0);
    frame.ctrl_set_border(if options.draw_border { theme.panel_border } else { 0.0 });
    frame.ctrl_set_margin(theme.panel_margin);

    if options.draw_border {
        frame.ctrl_set_draw_self(true);
        frame.ctrl_set_draw_self_border_color(theme.panel_border_color);
    }

    if options.draw_header {
        frame.push_ctrl(id!(0));
        frame.ctrl_set_flags(CtrlFlags::NONE);
        frame.ctrl_set_layout(Layout::Free);
        frame.ctrl_set_rect(Box2::new(0.0, 0.0, outer_width, theme.panel_header_height));
        frame.ctrl_set_padding(0.0);
        frame.ctrl_set_border(0.0);
        frame.ctrl_set_margin(0.0);

        frame.ctrl_set_draw_self(true);
        frame.ctrl_set_draw_self_background_color(theme.panel_header_background_color);

        if label.len() > 0 {
            frame.ctrl_draw_text(
                label,
                Align::Center,
                Align::Center,
                Wrap::Word,
                theme.panel_header_text_color,
            );
        }

        frame.pop_ctrl();
    }

    frame.push_ctrl(id!(1));
    frame.ctrl_set_flags(body_flags);
    frame.ctrl_set_layout(layout);
    frame.ctrl_set_rect(Box2::new(
        0.0,
        0.0,
        outer_width,
        if options.draw_header {
            f32::max(0.0, outer_height - theme.panel_header_height)
        } else {
            outer_height
        },
    ));
    frame.ctrl_set_padding(if options.draw_padding { theme.panel_padding } else { 0.0 });
    frame.ctrl_set_border(0.0);
    frame.ctrl_set_margin(0.0);

    frame.ctrl_set_draw_self(true);
    frame.ctrl_set_draw_self_border_color(theme.panel_border_color);
    frame.ctrl_set_draw_self_background_color(theme.panel_background_color);
}
