#![feature(never_type)]
#![feature(unix_socket_ancillary_data)]

/*
 * generate with something like:
 * cat /usr/share/wayland/wayland.xml /usr/share/wayland-protocols/stable/xdg-shell/xdg-shell.xml | cargo run --bin wscan |awk '/GEN BEGIN/,/GEN DONE/' >wayland.rs
 */
mod wayland;
use std::mem::MaybeUninit;

struct Data;

impl wayland::wl_display_v1::WlDisplayListener for &Data {
    fn error(&self, object_id: wayland::WlObject<!>, code: u32, message: wayland::WlString) {}
}
impl wayland::wl_registry_v1::WlRegistryListener for &Data {
    fn global(&self, name: u32, interface: wayland::WlString, version: u32) {}
}

fn main() {
    let mut display = wayland::wl_display_v1::get();
    display.add_listener(Box::new(&Data));
    let mut registry = MaybeUninit::uninit();
    display.get_registry(&mut registry);
    // SAFETY: get_registry better have initialized it...
    let mut registry = unsafe { registry.assume_init() };
    registry.get_mut().add_listener(Box::new(&Data));
    wayland::poll();
}

