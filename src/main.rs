use anyhow::{anyhow, Context, Result};
use wayland_client::protocol::{
    wl_compositor::WlCompositor,
    wl_output::WlOutput,
    wl_pointer,
    wl_seat::{self, WlSeat},
    wl_shm::{self, WlShm},
    wl_surface::WlSurface,
};
use wayland_client::EventQueue;
use wayland_client::{self, Display, Filter, GlobalManager, Main};
use wayland_protocols::unstable::xdg_output::v1::client::{
    zxdg_output_manager_v1::ZxdgOutputManagerV1 as XdgOutputManager,
    zxdg_output_v1::{self as xdg_output, ZxdgOutputV1 as XdgOutput},
};
use wayland_protocols::wlr::unstable::layer_shell::v1::client::{
    zwlr_layer_shell_v1::{Layer, ZwlrLayerShellV1 as LayerShell},
    zwlr_layer_surface_v1::{self as layer_surface, ZwlrLayerSurfaceV1 as LayerSurface},
};
use wayland_protocols::xdg_shell::client::xdg_wm_base::{self, XdgWmBase};

macro_rules! filter {
    ($self:expr, $data:ident, $($p:pat => $body:expr),*) => {
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
    use std::io::BufRead;
    use std::str::FromStr;
    use std::sync::mpsc::{self, Receiver};

    #[derive(Clone, Copy, Debug)]
    pub enum StatusMod {
        Esc,
        Bel,
        Enq,
    }
    pub type Status = Vec<Vec<(StatusMod, String)>>;

    pub fn handle_stdin() -> Receiver<Status> {
        fn split_mods(s: &str) -> Vec<(StatusMod, String)> {
            let mut b = 0;
            let mut ret = vec![];
            let mut m = StatusMod::Esc;
            let mut inds = s.char_indices();
            loop {
                match inds.next() {
                    Some((e, c)) => match c {
                        '\x07' /*bell*/ => {
                            if b != e {
                                ret.push((m, String::from(&s[b..e])));
                            }
                            m = StatusMod::Bel;
                            b = e + 1;
                        },
                        '\x05' /*enquiry*/ => {
                            if b != e {
                                ret.push((m, String::from(&s[b..e])));
                            }
                            m = StatusMod::Enq;
                            b = e + 1;
                        },
                        '\x27' /*escape*/ => {
                            if b != e {
                                ret.push((m, String::from(&s[b..e])));
                            }
                            m = StatusMod::Esc;
                            b = e + 1;
                        }
                        _ => {}
                    },
                    None => {
                        if b < s.len() {
                            ret.push((m, String::from(&s[b..])));
                        }
                        break;
                    }
                }
            }
            ret
        }

        let (tx, rx) = mpsc::channel();
        std::thread::spawn(move || {
            let stdin = std::io::stdin();
            let stdin = stdin.lock();
            for line in stdin.lines() {
                if let Ok(line) = line {
                    let status: Status = line.split('\t').map(split_mods).take(3).collect();
                    tx.send(status).unwrap();
                }
            }
        });
        rx
    }

    #[derive(Debug, Default)]
    pub struct Config {
        pub font: Font,
        pub status: Status,
        pub nf: u32,
        pub nb: u32,
        pub sf: u32,
        pub sb: u32,
        pub uf: u32,
        pub ub: u32,
        pub border: usize,
        pub should_close: bool,
    }

    impl Config {}

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
            let scale = Scale::uniform(18.0);
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
    xdg_output: Main<XdgOutput>,
    shm: Main<WlShm>,
    wmbase: Main<XdgWmBase>,
    layer_shell: Main<LayerShell>,
}

#[derive(Debug, Default)]
struct Pointer {
    pos: Option<(f64, f64)>,
    btn: Option<wl_pointer::ButtonState>,
    frame: bool,
}

#[derive(Debug)]
struct Surface {
    wl: Main<WlSurface>,
    layer: Main<LayerSurface>,
    buffer: ShmPixelBuffer,
    committed: bool,
    configured: bool,
    rendered: bool,
}

#[derive(Debug)]
struct Data {
    cfg: Config,
    registry: Registry,
    ptr: Pointer,
    seat_cap: wl_seat::Capability,
    shm_formats: Vec<wl_shm::Format>,
    surface: Option<Surface>,
}

impl Data {
    fn new(cfg: Config, registry: Registry) -> Data {
        filter!(registry.seat, data,
            wl_seat::Event::Capabilities{capabilities} => data.seat_cap = capabilities);
        filter!(registry.seat.get_pointer(), data,
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

        filter!(registry.wmbase, data,
            xdg_wm_base::Event::Ping { serial } => data.registry.wmbase.detach().pong(serial)
        );
        filter!(registry.xdg_output, data,
            xdg_output::Event::LogicalSize { width, height } => data.init_surface(width, height)
        );
        filter!(registry.shm, data,
            wl_shm::Event::Format { format } => data.shm_formats.push(format)
        );

        let data = Data {
            cfg,
            registry,
            ptr: Pointer::default(),
            seat_cap: wl_seat::Capability::from_raw(0).unwrap(),
            shm_formats: vec![],
            surface: None,
        };
        data
    }

    fn init_surface(&mut self, width: i32, _height: i32) {
        let height: i32 = 20;
        let shmbuffer = create_shmbuffer(width as usize, height as usize, &self.registry.shm)
            .expect("failed to create shm");
        let surface = self.registry.compositor.create_surface();
        let namespace = String::from("wsel");
        let layer_surface = self.registry.layer_shell.get_layer_surface(
            &surface.detach(),
            None,
            Layer::Overlay,
            namespace,
        );

        use layer_surface::Anchor;
        layer_surface.set_anchor(Anchor::Top | Anchor::Left | Anchor::Right);
        layer_surface.set_size(width as u32, height as u32);
        layer_surface.set_exclusive_zone(height);

        filter!(layer_surface, data,
            layer_surface::Event::Configure { serial, .. } => if let Some(ref mut surface) = data.surface {
                surface.layer.detach().ack_configure(serial);
                surface.configured = true;
            },
            layer_surface::Event::Closed => {
                data.cfg.should_close = true;
            }
        );
        surface.commit();

        let mut surface = Surface {
            wl: surface,
            layer: layer_surface,
            buffer: shmbuffer,
            committed: false,
            configured: false,
            rendered: false,
        };
        surface.render(&self.cfg);
        self.surface = Some(surface);
    }
}

impl Surface {
    fn render(&mut self, cfg: &Config) {
        if self.buffer.locked {
            return;
        }
        let shm = &mut self.buffer;

        for i in 0..shm.width {
            for j in 0..shm.height {
                shm[(i, j)] = cfg.nb;
            }
        }

        fn scale(v: u8, s: u8) -> u8 {
            ((v as u32 * s as u32) / 255) as u8
        }
        fn lerp(s: u8, a: u8, b: u8) -> u8 {
            scale(s, a) + scale(255 - s, b)
        }

        let rendered = self.rendered;
        let mut alignments = ["left", "center", "right"].into_iter();
        for col in cfg.status.iter() {
            let align = alignments.next().unwrap();
            let gsegs = col
                .iter()
                .map(|(m, seg)| (*m, cfg.font.glyphs(seg)))
                .collect::<Vec<_>>();
            let width = gsegs.iter().fold(0.0, |acc, (_, g)| acc + g.width);
            let mut left = match align {
                "left" => 0,
                "center" => (shm.width - width.ceil() as usize) / 2,
                "right" => shm.width - width.ceil() as usize,
                _ => unreachable!(),
            };

            for (m, g) in gsegs.into_iter() {
                let mut warn_buf = false;
                let right = left + g.width.ceil() as usize;
                let fg = match m {
                    conf::StatusMod::Bel => {
                        for x in left..right {
                            for y in 0..shm.height {
                                shm[(x, y)] = cfg.ub;
                            }
                        }
                        cfg.uf
                    }
                    conf::StatusMod::Enq => {
                        for x in left..right {
                            for y in 0..shm.height {
                                shm[(x, y)] = cfg.sb;
                            }
                        }
                        cfg.sf
                    }
                    conf::StatusMod::Esc => cfg.nf,
                };
                let (trans_x, trans_y) = (left as i32, 0);
                left = right;
                g.render(|x, y, v| {
                    let (x, y) = (x as i32 + trans_x, y as i32 + trans_y);
                    if x < 0 || x as usize >= shm.width || y < 0 || y as usize >= shm.height {
                        if !rendered && !warn_buf {
                            eprintln!(
                                "glyph exceeds buffer boundaries: {:?} {:?}",
                                (x, y),
                                (shm.width, shm.height)
                            );
                            warn_buf = true;
                        }
                        return;
                    }

                    let pixi = (x as usize, y as usize);
                    let [ab, rb, gb, bb] = shm[pixi].to_be_bytes();
                    let [af, rf, gf, bf] = fg.to_be_bytes();
                    let s = scale(af, v);
                    shm[pixi] = u32::from_be_bytes([
                        lerp(s, 0xff, ab),
                        lerp(s, rf, rb),
                        lerp(s, gf, gb),
                        lerp(s, bf, bb),
                    ]);
                });
            }
        }

        self.wl.damage(0, 0, shm.width as i32, shm.height as i32);
        self.committed = false;
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
                if let Some(ref mut surface) = data.surface {
                    surface.buffer.locked = false;
                }
            }
        );

        Ok(ShmPixelBuffer {
            wl: buffer,
            locked: false,
            addr: shmdata,
            width,
            height,
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
    let output: Main<WlOutput> = gm
        .instantiate_exact(4)
        .context("Failed to get output handle")?;
    let output_manager: Main<XdgOutputManager> = gm
        .instantiate_exact(3)
        .context("Failed to get xdg output manager handle")?;
    let xdg_output = output_manager.get_xdg_output(&output);
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
        xdg_output,
        wmbase,
        shm,
        layer_shell,
    })
}

fn parse_config(mut args: std::env::Args) -> Result<Config> {
    let border = 1usize;
    let (mut nf, mut nb, mut sf, mut sb, mut uf, mut ub) = (
        0xffddddddu32,
        0xdd222222u32,
        0xff222222u32,
        0xffff9900u32,
        0xff222222u32,
        0xffddddddu32,
    );
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
                "-uf" => uf = arg.parse::<Argb>()?.0,
                "-ub" => ub = arg.parse::<Argb>()?.0,
                _ => {
                    Err(anyhow!("Unrecognized argument {}", flag))?;
                }
            },
            (Some(arg), None) => Err(anyhow!("Unrecognized argument {}", arg))?,
            (None, _) => break,
        }
    }

    Ok(Config {
        status: vec![],
        font: font.unwrap_or_else(Font::default),
        border,
        nf,
        nb,
        sf,
        sb,
        uf,
        ub,
        should_close: false,
    })
}

fn main() -> Result<()> {
    let cfg = parse_config(std::env::args())?;

    let display = Display::connect_to_env().context("failed to connect to display")?;
    let mut event_queue = display.create_event_queue();
    let registry = init_registry(&display, &mut event_queue)
        .context("failed to get necessary handles for registry")?;
    let mut data = Data::new(cfg, registry);
    let status_ch = conf::handle_stdin();

    let mut dirty = true;
    while !data.cfg.should_close {
        let start = std::time::SystemTime::now();

        while let Ok(status) = status_ch.try_recv() {
            data.cfg.status = status;
            data.surface.as_mut().map(|s| s.rendered = false);
            dirty = true;
        }

        if let Some(ref mut surface) = data.surface {
            if surface.rendered != true {
                surface.render(&data.cfg);
            }

            if surface.configured && !surface.committed {
                surface.wl.attach(Some(&surface.buffer.wl), 0, 0);
                surface.buffer.locked = true;
                surface.wl.commit();
                surface.committed = true;
            }

            if surface.committed {
                dirty = false;
            }
        }

        if let Err(e) = display.flush() {
            if e.kind() != std::io::ErrorKind::WouldBlock {
                return Err(e.into());
            }
        }

        if let Some(guard) = event_queue.prepare_read() {
            if let Err(e) = guard.read_events() {
                if e.kind() != std::io::ErrorKind::WouldBlock {
                    return Err(e.into());
                }
            }
        }

        event_queue
            .dispatch_pending(&mut data, |_, _, _| {})
            .context("An error occurred during event dispatch")?;

        let wait = if dirty {
            std::time::Duration::new(0, 1_000_000)
        } else {
            std::time::Duration::new(0, 50_000_000)
        };
        match start.elapsed() {
            Ok(dur) => {
                if dur < wait {
                    std::thread::sleep(wait - dur);
                }
            }
            Err(e) => {
                eprintln!("sleep: {}", e);
                std::thread::sleep(std::time::Duration::new(1, 0));
            }
        }
    }

    Ok(())
}
