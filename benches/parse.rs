#![feature(test)]

extern crate test;
use test::{Bencher, black_box};

use std::fs::read_to_string;

macro_rules! bench {
    ($name:ident) => {
        mod $name {
            use super::*;

            #[bench]
            fn robinson(bencher: &mut Bencher) {
                use robinson::Document;

                let text =
                    read_to_string(format!("benches/inputs/{}.xml", stringify!($name))).unwrap();

                bencher.iter(|| Document::parse(black_box(&text)).unwrap());
            }

            #[bench]
            fn roxmltree(bencher: &mut Bencher) {
                use roxmltree::{Document, ParsingOptions};

                let text =
                    read_to_string(format!("benches/inputs/{}.xml", stringify!($name))).unwrap();

                bencher.iter(|| {
                    Document::parse_with_options(
                        black_box(&text),
                        ParsingOptions {
                            allow_dtd: true,
                            ..Default::default()
                        },
                    )
                    .unwrap()
                });
            }

            #[bench]
            fn quickxml(bencher: &mut Bencher) {
                use quick_xml::{events::Event, reader::NsReader};

                let text =
                    read_to_string(format!("benches/inputs/{}.xml", stringify!($name))).unwrap();

                bencher.iter(|| {
                    let mut reader = NsReader::from_str(black_box(&text));

                    let mut elements = 0;
                    let mut attributes = 0;

                    loop {
                        match reader.read_resolved_event().unwrap() {
                            (_, Event::Start(start)) => {
                                elements += 1;
                                attributes += start.attributes().count();
                            }
                            (_, Event::Eof) => break,
                            _ => (),
                        }
                    }

                    (elements, attributes)
                });
            }
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
