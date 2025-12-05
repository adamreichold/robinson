use std::env::var;

fn main() {
    println!("cargo:rustc-check-cfg=cfg(simd_avx2)");
    println!("cargo:rustc-check-cfg=cfg(simd_ssse3)");
    println!("cargo:rustc-check-cfg=cfg(simd_sse2)");
    println!("cargo:rustc-check-cfg=cfg(simd_neon)");

    let arch = var("CARGO_CFG_TARGET_ARCH").unwrap();
    let endian = var("CARGO_CFG_TARGET_ENDIAN").unwrap();

    let feature = var("CARGO_CFG_TARGET_FEATURE").unwrap_or_default();
    let features = feature.split(',').collect::<Vec<_>>();

    if arch == "x86_64" {
        if features.contains(&"avx2") {
            println!("cargo:rustc-cfg=simd_avx2");
        } else {
            if features.contains(&"ssse3") {
                println!("cargo:rustc-cfg=simd_ssse3");
            }

            println!("cargo:rustc-cfg=simd_sse2");
        }
    } else if arch == "aarch64" && endian == "little" && features.contains(&"neon") {
        println!("cargo:rustc-cfg=simd_neon");
    }
}
