#![no_main]

use libfuzzer_sys::fuzz_target;
use robinson::Document;

fuzz_target!(|text: &str| {
    let _ = Document::parse(text);
});
