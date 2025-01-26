#![feature(test)]

extern crate test;
use test::{Bencher, black_box};

use std::fs::read_to_string;

use robinson::Document;

macro_rules! bench {
    ($name:ident) => {
        #[bench]
        fn $name(bencher: &mut Bencher) {
            let text = read_to_string(format!("benches/inputs/{}.xml", stringify!($name))).unwrap();

            bencher.iter(|| Document::parse(black_box(&text)).unwrap());
        }
    };
}

bench!(gigantic);
bench!(huge);
bench!(large);
bench!(medium);
bench!(tiny);

bench!(cdata);
bench!(text);
bench!(attributes);
