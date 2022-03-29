use anyhow::{anyhow, Context, Result};
use std::cmp::max;
use std::io::BufRead;
use wayland_client::protocol::{
    wl_compositor::WlCompositor,
    wl_keyboard, wl_pointer,
    wl_seat::{self, WlSeat},
    wl_shm::{self, WlShm},
    wl_surface::WlSurface,
};
use wayland_client::EventQueue;
use wayland_client::{self, Display, Filter, GlobalManager, Main};
use wayland_protocols::wlr::unstable::layer_shell::v1::client::{
    zwlr_layer_shell_v1::{self as layer_shell, Layer, ZwlrLayerShellV1 as LayerShell},
    zwlr_layer_surface_v1::{self as layer_surface, ZwlrLayerSurfaceV1 as LayerSurface},
};
use wayland_protocols::xdg_shell::client::xdg_wm_base::{self, XdgWmBase};

macro_rules! filter {
    ($self:ident, $data:ident, $($p:pat => $body:expr),*) => {
        $self.assign(
            Filter::new(move |(_, ev), _filter, mut ddata| {
                let $data = ddata.get::<Data>().expect("failed to get data");
                match ev {
                    $($p => $body),+,
                    _ => {},
                }
            }
        ))
    };
}

mod conf {
    use super::Font;
    use anyhow::{anyhow, Result};
    use std::str::FromStr;

    #[derive(Debug, Default)]
    pub struct Config {
        pub font: Font,
        pub options: Vec<String>,
        pub nf: u32,
        pub nb: u32,
        pub sf: u32,
        pub sb: u32,
        pub button_dim: (usize, usize),
        pub border: usize,
        pub should_close: bool,
    }

    impl Config {
        pub fn buttons_bounds(&self) -> (usize, usize) {
            (
                self.border + self.options.len() * (self.button_dim.0 + self.border),
                self.border + self.button_dim.1 + self.border,
            )
        }

        pub fn in_button(&self, x: usize, y: usize) -> Option<usize> {
            let (border, (bw, bh)) = (self.border, self.button_dim);
            if y >= border && y < border + bh && x >= border && (x - border) % (bw + border) < bw {
                Some((x - border) / (bw + border))
            } else {
                None
            }
        }

        pub fn button_bounds(&self, i: usize) -> (i32, i32, i32, i32) {
            let (border, (bw, bh)) = (self.border, self.button_dim);
            let left = border + i * (bw + border);
            let right = left + bw;
            let top = border;
            let bottom = top + bh;
            (left as i32, right as i32, top as i32, bottom as i32)
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Argb(pub u32);

    static ARGB_FORMAT_MSG: &str =
        "Argb must be specified by a '#' followed by exactly 3, 4, 6, or 8 digits";

    impl FromStr for Argb {
        type Err = anyhow::Error;
        fn from_str(s: &str) -> Result<Self> {
            if !s.starts_with('#') || !s[1..].chars().all(|c| c.is_ascii_hexdigit()) {
                return Err(anyhow!(ARGB_FORMAT_MSG));
            }

            let s = &s[1..];
            let dup = |s: &str| {
                s.chars().fold(String::new(), |mut s, c| {
                    s.push(c);
                    s.push(c);
                    s
                })
            };
            match s.len() {
                8 => Ok(Argb(u32::from_str_radix(s, 16)?)),
                6 => Ok(Argb(u32::from_str_radix(s, 16)? | 0xff000000)),
                4 => Ok(Argb(u32::from_str_radix(&dup(s), 16)?)),
                3 => Ok(Argb(u32::from_str_radix(&dup(s), 16)? | 0xff000000)),
                _ => Err(anyhow!(ARGB_FORMAT_MSG)),
            }
        }
    }
}
use conf::{Argb, Config};

use font::Font;
mod font {
    use anyhow::{Context, Result};
    use rusttype::{self, point, Font as rtFont, Point, PositionedGlyph, Scale};

    #[derive(Debug)]
    pub struct Font {
        font: rtFont<'static>,
        scale: Scale,
        offset: Point<f32>,
    }

    #[derive(Debug)]
    pub struct Glyphs<'f> {
        glyphs: Vec<PositionedGlyph<'f>>,
        pub width: f32,
        pub height: f32,
    }

    #[cfg(default_font = "static")]
    impl Default for Font {
        fn default() -> Self {
            let font = rtFont::try_from_bytes(include_bytes!(env!("EMBED_FONT")) as &[u8])
                .expect("Failed to load embedded default font");
            Font::new(font)
        }
    }

    #[cfg(default_font = "dynamic")]
    impl Default for Font {
        fn default() -> Self {
            Font::load(env!("DEFAULT_FONT")).unwrap()
        }
    }

    #[cfg(default_font = "none")]
    impl Default for Font {
        fn default() -> Self {
            unimplemented!("To use a default font, copy an otf or ttf file into the source directory and recompile")
        }
    }

    impl Font {
        fn new(font: rtFont<'static>) -> Self {
            let scale = Scale::uniform(40.0);
            let v_metrics = font.v_metrics(scale);
            let offset = point(0.0, v_metrics.ascent);
            Font {
                font,
                scale,
                offset,
            }
        }

        pub fn load<P: AsRef<std::path::Path>>(name: &P) -> Result<Font> {
            let bytes = std::fs::read(name)?;
            let font = rtFont::try_from_vec(bytes).context("Failed loading the default font")?;
            Ok(Self::new(font))
        }

        pub fn glyphs(&self, s: &str) -> Glyphs {
            let glyphs: Vec<_> = self.font.layout(s, self.scale, self.offset).collect();
            let width = glyphs
                .last()
                .map(|g| g.position().x + g.unpositioned().h_metrics().advance_width)
                .unwrap_or(0.0);

            Glyphs {
                glyphs,
                width,
                height: self.scale.y,
            }
        }
    }

    impl<'f> Glyphs<'f> {
        pub fn render(self, mut d: impl FnMut(usize, usize, u8)) {
            let (width, height) = (self.width.ceil(), self.height.ceil());

            self.glyphs
                .iter()
                .filter_map(|g| g.pixel_bounding_box().map(|bb| (g, bb)))
                .for_each(|(g, bb)| {
                    g.draw(|x, y, v| {
                        let v = (v * 255.0).ceil() as u8;
                        let x = x as i32 + bb.min.x;
                        let y = y as i32 + bb.min.y;
                        if x >= 0 && x < width as i32 && y >= 0 && y < height as i32 {
                            d(x as usize, y as usize, v);
                        }
                    })
                })
        }
    }
}

#[derive(Debug)]
struct Registry {
    compositor: Main<WlCompositor>,
    seat: Main<WlSeat>,
    shm: Main<WlShm>,
    wmbase: Main<XdgWmBase>,
    layer_shell: Main<LayerShell>,
}

#[derive(Debug, Default)]
struct Pointer {
    pos: Option<(f64, f64)>,
    pos_prev: Option<(f64, f64)>,
    btn: Option<wl_pointer::ButtonState>,
    btn_prev: Option<wl_pointer::ButtonState>,
    frame: bool,
}

#[derive(Debug)]
struct Surface {
    wl: Main<WlSurface>,
    layer: Main<LayerSurface>,
    committed: bool,
    configured: bool,
}

#[derive(Debug)]
struct Data {
    cfg: Config,
    registry: Registry,
    ptr: Pointer,
    seat_cap: wl_seat::Capability,
    shm_formats: Vec<wl_shm::Format>,
    buffer: ShmPixelBuffer,
    surface: Surface,
    rendered: bool,
}

impl Data {
    fn new(cfg: Config, mut registry: Registry) -> Data {
        let seat = &mut registry.seat;
        filter!(seat, data,
            wl_seat::Event::Capabilities{capabilities} => data.seat_cap = capabilities
        );
        let pointer = seat.get_pointer();
        filter!(pointer, data,
            wl_pointer::Event::Enter { surface_x, surface_y, .. } => {
                data.ptr.pos.replace((surface_x, surface_y));
            },
            wl_pointer::Event::Leave { .. } => {
                data.ptr.pos.take();
                data.ptr.btn.take();
            },
            wl_pointer::Event::Motion { surface_x, surface_y, .. } => {
                data.ptr.pos.replace((surface_x, surface_y));
            },
            wl_pointer::Event::Button { button: 0x110, state, .. } => {
                // 0x110 is BUTTON1
                data.ptr.btn.replace(state);
            },
            wl_pointer::Event::Frame => {
                data.ptr.frame = true;
            }
        );
        let kbd = seat.get_keyboard();
        filter!(kbd, data,
            wl_keyboard::Event::Key { key: 1, .. } => {
                data.cfg.should_close = true;
            }
        );

        let wmbase = &mut registry.wmbase;
        filter!(wmbase, data,
            xdg_wm_base::Event::Ping { serial } => data.registry.wmbase.detach().pong(serial)
        );

        let shm = &mut registry.shm;
        filter!(shm, data,
            wl_shm::Event::Format { format } => data.shm_formats.push(format)
        );

        let (width, height) = cfg.buttons_bounds();
        let shmbuffer = create_shmbuffer(width, height, shm).expect("failed to create shm");

        let (width, height) = cfg.buttons_bounds();
        let surface =
            Data::create_surface(width, height, &registry.compositor, &registry.layer_shell);

        let mut data = Data {
            cfg,
            registry,
            ptr: Pointer::default(),
            buffer: shmbuffer,
            surface,
            seat_cap: wl_seat::Capability::from_raw(0).unwrap(),
            shm_formats: vec![],
            rendered: false,
        };
        data.render();
        data
    }

    fn create_surface(
        width: usize,
        height: usize,
        compositor: &Main<WlCompositor>,
        layer_shell: &Main<LayerShell>,
    ) -> Surface {
        let wl = compositor.create_surface();
        let (width, height) = (width as i32, height as i32);
        let namespace = String::from("wtmenu");
        let layer = layer_shell.get_layer_surface(&wl.detach(), None, Layer::Overlay, namespace);
        layer.set_size(width as u32, height as u32);
        layer.set_keyboard_interactivity(layer_surface::KeyboardInteractivity::Exclusive);
        filter!(layer, data,
            layer_surface::Event::Configure { serial, .. } => {
                data.surface.layer.detach().ack_configure(serial);
                data.surface.configured = true;
            },
            layer_surface::Event::Closed => {
                data.cfg.should_close = true;
            }
        );
        wl.commit();

        Surface {
            wl,
            layer,
            committed: false,
            configured: false,
        }
    }

    fn render(&mut self) {
        if self.buffer.locked {
            return;
        }
        let shm = &mut self.buffer;
        let (bw, bh) = self.cfg.button_dim;

        let focus = {
            let cfg = &self.cfg;
            (self.ptr.btn)
                .filter(|s| s == &wl_pointer::ButtonState::Pressed)
                .and(self.ptr.pos)
                .and_then(|(x, y)| cfg.in_button(x.ceil() as usize, y.ceil() as usize))
        };

        for i in 0..shm.width {
            for j in 0..shm.height {
                if let Some(opti) = self.cfg.in_button(i, j) {
                    shm[(i, j)] = if Some(opti) == focus {
                        self.cfg.sb
                    } else {
                        self.cfg.nb
                    };
                } else {
                    shm[(i, j)] = (self.cfg.nb & 0xffffff) | 0x22000000;
                }
            }
        }

        let scale = |v: u8, s: u8| ((v as u32 * s as u32) / 255) as u8;
        let (nf, sf) = (self.cfg.nf, self.cfg.sf);
        let rendered = self.rendered;
        for i in 0..self.cfg.options.len() {
            let opt = self.cfg.options.get(i).unwrap();
            let g = self.cfg.font.glyphs(opt);

            let (left, right, top, bottom) = self.cfg.button_bounds(i);

            let trans_x: i32 = max(left, left - (g.width.ceil() as i32 - bw as i32) / 2);
            let trans_y: i32 = max(top, top - (g.height.ceil() as i32 - bh as i32) / 2);

            let (mut warn_btn, mut warn_buf) = (false, false);
            g.render(|x, y, v| {
                let (x, y) = (x as i32 + trans_x, y as i32 + trans_y);
                if x < 0 || x as usize >= shm.width || y < 0 || y as usize >= shm.height {
                    if !rendered && !warn_buf {
                        eprintln!(
                            "glyph for {:?} exceeds buffer boundaries: {:?} {:?}",
                            opt,
                            (x, y),
                            (shm.width, shm.height)
                        );
                        warn_buf = true;
                    }
                    return;
                }
                if x < left || x >= right || y < top || y >= bottom {
                    if !rendered && !warn_btn {
                        eprintln!(
                            "glyph for {:?} exceeds button boundaries: {:?} {:?}",
                            opt,
                            (x, y),
                            (left, right, top, bottom)
                        );
                        warn_btn = true;
                    }
                    return;
                }

                let pixi = (x as usize, y as usize);
                let [a, rb, gb, bb] = shm[pixi].to_be_bytes();
                let [_, rf, gf, bf] = if Some(i) == focus {
                    sf.to_be_bytes()
                } else {
                    nf.to_be_bytes()
                };
                shm[pixi] = u32::from_be_bytes([
                    a,
                    max(rb, scale(v, rf)),
                    max(gb, scale(v, gf)),
                    max(bb, scale(v, bf)),
                ]);
            });
        }

        let (ww, wh) = self.cfg.buttons_bounds();
        self.surface.wl.damage(0, 0, ww as i32, wh as i32);
        self.surface.committed = false;
        self.rendered = true;
    }
}

mod pixbuf {
    use super::Data;
    use anyhow::{Context, Result};
    use wayland_client::protocol::{
        wl_buffer::{self, WlBuffer},
        wl_shm::{self, WlShm},
    };
    use wayland_client::{Filter, Main};

    #[derive(Debug)]
    pub struct ShmPixelBuffer {
        pub wl: Main<WlBuffer>,
        pub locked: bool,
        pub width: usize,
        pub height: usize,
        addr: *mut u32,
    }

    impl std::ops::Index<(usize, usize)> for ShmPixelBuffer {
        type Output = u32;
        fn index(&self, (x, y): (usize, usize)) -> &Self::Output {
            if x >= self.width || y >= self.height {
                panic!(
                    "index ({}, {}) out of bounds (0..{}, 0..{})",
                    x, y, self.width, self.height
                );
            }
            unsafe {
                self.addr
                    .offset((x + y * self.width) as isize)
                    .as_ref()
                    .unwrap()
            }
        }
    }

    impl std::ops::IndexMut<(usize, usize)> for ShmPixelBuffer {
        fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut Self::Output {
            if x >= self.width || y >= self.height {
                panic!(
                    "index ({}, {}) out of bounds (0..{}, 0..{})",
                    x, y, self.width, self.height
                );
            }
            unsafe {
                self.addr
                    .offset((x + y * self.width) as isize)
                    .as_mut()
                    .unwrap()
            }
        }
    }

    pub fn create_shmbuffer(
        width: usize,
        height: usize,
        shm: &Main<WlShm>,
    ) -> Result<ShmPixelBuffer> {
        let fd = nix::unistd::mkstemp("/dev/shm/shmbuf_XXXXXX")
            .and_then(|(fd, path)| nix::unistd::unlink(path.as_path()).and(Ok(fd)))
            .context("Failed to create temp file fd for shm")?;
        let (format, pixel_size) = (wl_shm::Format::Argb8888, 4);
        let stride: i32 = width as i32 * pixel_size;
        let size: usize = stride as usize * height;

        nix::unistd::ftruncate(fd, size as i64).context("Failed calling ftruncate")?;

        let shmdata: *mut u32 = unsafe {
            let data = libc::mmap(
                std::ptr::null_mut(),
                size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED,
                fd,
                0,
            );
            // checking for null is not in the manpage example, can you mmap 0x0?
            if data == libc::MAP_FAILED || data.is_null() {
                libc::close(fd);
                panic!("map failed");
            }
            data as *mut u32
        };

        let pool = shm.create_pool(fd, size as i32);
        let buffer = pool.create_buffer(0, width as i32, height as i32, stride, format);
        pool.destroy();

        filter!(buffer, data,
            wl_buffer::Event::Release => {
                data.buffer.locked = false;
            }
        );

        Ok(ShmPixelBuffer {
            wl: buffer,
            locked: false,
            addr: shmdata,
            width: width,
            height: height,
        })
    }
}
use pixbuf::{create_shmbuffer, ShmPixelBuffer};

fn init_registry(display: &Display, event_queue: &mut EventQueue) -> Result<Registry> {
    let disp_proxy = display.attach(event_queue.token());

    let gm = GlobalManager::new(&disp_proxy);
    event_queue.dispatch(&mut (), |_, _, _| {})?;
    let compositor: Main<WlCompositor> = gm
        .instantiate_exact(4)
        .context("Failed to get compositor handle")?;
    let seat: Main<WlSeat> = gm
        .instantiate_exact(5)
        .context("Failed to get seat handle")?;
    let wmbase: Main<XdgWmBase> = gm
        .instantiate_exact(2)
        .context("Failed to get wmbase handle")?;
    let shm: Main<WlShm> = gm
        .instantiate_exact(1)
        .context("Failed to get shm handle")?;
    let layer_shell: Main<LayerShell> = gm
        .instantiate_range(2, 5)
        .context("Failed to get layer shell handle")?;

    Ok(Registry {
        compositor,
        seat,
        wmbase,
        shm,
        layer_shell,
    })
}

fn parse_config(mut args: std::env::Args, stdin: std::io::StdinLock) -> Result<Config> {
    let border = 1usize;
    let (bw, bh) = (300usize, 0usize);
    let (mut nf, mut nb, mut sf, mut sb) =
        (0xffddddddu32, 0xdd222222u32, 0xffddddddu32, 0xffff9900u32);
    let mut font: Option<Font> = None;

    args.next();
    loop {
        match (args.next(), args.next()) {
            (Some(flag), _) if flag.as_str() == "-h" => {
                println!(
                    "usage: wsel [-h] [-fn font] [-nf color] [-nb color] [-sf color] [-sb color]"
                );
                std::process::exit(0);
            }
            (Some(flag), Some(arg)) => match flag.as_str() {
                "-fn" => {
                    font = font.or(Font::load(&arg)
                        .map_err(|err| eprintln!("failed to load font {}: {}", arg, err))
                        .ok())
                }
                "-nf" => nf = arg.parse::<Argb>()?.0,
                "-nb" => nb = arg.parse::<Argb>()?.0,
                "-sf" => sf = arg.parse::<Argb>()?.0,
                "-sb" => sb = arg.parse::<Argb>()?.0,
                _ => {
                    Err(anyhow!("Unrecognized argument {}", flag))?;
                }
            },
            (Some(arg), None) => Err(anyhow!("Unrecognized argument {}", arg))?,
            (None, _) => break,
        }
    }

    let options = stdin.lines().fold(Ok(vec![]), |acc, x| match (acc, x) {
        (Ok(acc), Ok(s)) if s.len() == 0 => Ok(acc),
        (Ok(mut acc), Ok(s)) => Ok({
            acc.push(s);
            acc
        }),
        (Err(err), _) => Err(err),
        (Ok(_), Err(err)) => Err(err),
    })?;

    Ok(Config {
        options,
        font: font.unwrap_or_else(Font::default),
        button_dim: (bw, if bh != 0 { bh } else { bw }),
        border,
        nf,
        nb,
        sf,
        sb,
        should_close: false,
    })
}

fn main() -> Result<()> {
    let cfg = parse_config(std::env::args(), std::io::stdin().lock())?;
    if cfg.options.len() == 0 {
        return Ok(());
    }

    let display = Display::connect_to_env().context("failed to connect to display")?;
    let mut event_queue = display.create_event_queue();
    let registry = init_registry(&display, &mut event_queue)
        .context("failed to get necessary handles for registry")?;
    let mut data = Data::new(cfg, registry);

    while !data.cfg.should_close {
        event_queue
            .dispatch(&mut data, |_, _, _| {})
            .context("An error occurred during event dispatch")?;

        if data.ptr.frame
            && (data.ptr.pos_prev.is_some() ^ data.ptr.pos.is_some()
                || data.ptr.btn != data.ptr.btn_prev)
        {
            data.ptr.btn_prev = data.ptr.btn;
            data.ptr.pos_prev = data.ptr.pos;
            data.render();

            if let Some(opt) = (data.ptr.btn)
                .filter(|btn| btn == &wl_pointer::ButtonState::Released)
                .and(data.ptr.pos)
                .and_then(|(x, y)| data.cfg.in_button(x.ceil() as usize, y.ceil() as usize))
                .and_then(|i| data.cfg.options.get(i))
            {
                println!("{}", opt);
                data.cfg.should_close = true;
            }
        }

        if data.surface.configured && !data.surface.committed {
            data.surface.wl.attach(Some(&data.buffer.wl), 0, 0);
            data.buffer.locked = true;
            data.surface.wl.commit();
            data.surface.committed = true;
        }
    }

    Ok(())
}
