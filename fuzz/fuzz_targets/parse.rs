#![no_main]

use bumpalo::Bump;
use libfuzzer_sys::fuzz_target;
use robinson::Document;

fuzz_target!(|text: &str| {
    let bump = Bump::new();
    let _ = Document::parse(text, &bump);
});
