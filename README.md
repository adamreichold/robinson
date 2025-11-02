# robinson

[![crates.io](https://img.shields.io/crates/v/robinson.svg)](https://crates.io/crates/robinson)
[![docs.rs](https://docs.rs/robinson/badge.svg)](https://docs.rs/robinson)
[![github.com](https://github.com/adamreichold/robinson/actions/workflows/test.yaml/badge.svg)](https://github.com/adamreichold/robinson/actions/workflows/test.yaml)

For when you go to a lonely island and survival depends on parsing XML.

This is a rewrite of [`roxmltree`](https://github.com/RazrFalcon/roxmltree) which makes a few API changes and drops some verification steps to enable speed-ups between 40 and 90 percent. It also compares favourably to [`quick-xml`](https://github.com/tafia/quick-xml/) even though it does build a [DOM](https://en.wikipedia.org/wiki/Document_Object_Model) and fully normalizes text and attribute values including entity resolution. For example, using `-Ctarget-cpu=native` on a Ryzen 5600U, the XML files contained in this repository yield the following measurements:

```
test gigantic::quickxml    ... bench:     957,260.00 ns/iter (+/- 33,442.06)
test gigantic::robinson    ... bench:     214,256.85 ns/iter (+/- 2,676.48)
test gigantic::roxmltree   ... bench:   1,811,600.92 ns/iter (+/- 59,620.67)

test huge::quickxml        ... bench:   2,703,925.00 ns/iter (+/- 19,549.92)
test huge::robinson        ... bench:   1,508,501.10 ns/iter (+/- 8,950.02)
test huge::roxmltree       ... bench:   4,569,721.60 ns/iter (+/- 29,496.06)

test large::quickxml       ... bench:   1,406,248.90 ns/iter (+/- 18,654.61)
test large::robinson       ... bench:     855,286.70 ns/iter (+/- 7,551.35)
test large::roxmltree      ... bench:   2,312,425.60 ns/iter (+/- 11,228.98)

test medium::quickxml      ... bench:     251,536.60 ns/iter (+/- 9,172.72)
test medium::robinson      ... bench:     181,242.25 ns/iter (+/- 1,933.15)
test medium::roxmltree     ... bench:     489,666.69 ns/iter (+/- 6,123.20)

test tiny::quickxml        ... bench:       2,907.67 ns/iter (+/- 22.95)
test tiny::robinson        ... bench:       1,970.23 ns/iter (+/- 21.82)
test tiny::roxmltree       ... bench:       3,650.34 ns/iter (+/- 91.92)
```

## License

Licensed under

 * [Apache License, Version 2.0](LICENSE-APACHE) or
 * [MIT license](LICENSE-MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
