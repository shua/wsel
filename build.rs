use std::env;

fn main() {
    if let Some(font) = env::var("DEFAULT_FONT").ok() {
        println!("cargo:rustc-cfg=default_font=\"dynamic\"");
        println!("cargo:rustc-env=DEFAULT_FONT={}", font);
    } else if let Some(font) = std::fs::read_dir(".").ok().and_then(|rd| {
        rd.map(|i| {
            i.ok()
                .and_then(|f| f.path().to_str().map(str::to_string))
                .unwrap_or("".into())
        })
        .filter(|f| f.ends_with(".otf") || f.ends_with(".ttf"))
        .next()
    }) {
        println!("cargo:rustc-cfg=default_font=\"static\"");
        println!("cargo:rustc-env=EMBED_FONT=../../{}", font);
    } else {
        println!("cargo:rustc-cfg=default_font=\"none\"");
    }
}
