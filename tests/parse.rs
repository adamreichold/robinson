use std::env::var;
use std::fs::{exists, read_dir, read_to_string, write};

use pretty_assertions::assert_eq;
use robinson::Document;

fn test(name: &str) {
    let input = format!("tests/inputs/{name}.xml");
    let output = format!("tests/outputs/{name}.dbg");

    let text = read_to_string(&input).unwrap();

    let res = Document::parse(&text);
    let ast = format!("{res:#?}\n");

    let expected_ast = read_to_string(&output).unwrap();

    if var("OVERWRITE_OUTPUTS").is_ok() {
        write(&output, &ast).unwrap();
    }

    assert_eq!(ast, expected_ast);
}

macro_rules! test {
    ($name:ident) => {
        #[test]
        fn $name() {
            test(stringify!($name));
        }
    };
}

// <<<<<<< discovered_tests
test!(attribute_values);
test!(attributes);
test!(billion_laughs_attribute_value);
test!(billion_laughs_element);
test!(cdata);
test!(children);
test!(crlf_via_char);
test!(crlf_via_entity);
test!(declaration);
test!(empty);
test!(entity_reference_attribute_value);
test!(entity_reference_element);
test!(invalid_reference_hexcode);
test!(missing_root_element);
test!(namespace_close_element);
test!(public_doctype);
test!(text);
test!(text_entity_interleaved);
test!(unclosed_root_element);
test!(unknown_namespace_empty);
test!(xml_namespace);
// >>>>>>> discovered_tests

#[test]
fn discover_tests() {
    let mut tests = Vec::new();

    for entry in read_dir("tests/inputs").unwrap() {
        let entry = entry.unwrap();

        let file_name = entry.file_name().into_string().unwrap();

        if let Some(test) = file_name.strip_suffix(".xml") {
            tests.push(test.to_owned());
        }
    }

    let source = read_to_string("tests/parse.rs").unwrap();

    let mut discovered_tests = source
        .lines()
        .skip_while(|line| *line != "// <<<<<<< discovered_tests")
        .skip(1)
        .take_while(|line| *line != "// >>>>>>> discovered_tests")
        .map(|line| {
            line.strip_prefix("test!(")
                .unwrap()
                .strip_suffix(");")
                .unwrap()
        })
        .collect::<Vec<_>>();

    tests.sort_unstable();
    discovered_tests.sort_unstable();

    if tests != discovered_tests {
        let mut old_lines = source.lines();
        let mut new_lines = Vec::new();

        for line in &mut old_lines {
            new_lines.push(line.to_owned());
            if line == "// <<<<<<< discovered_tests" {
                break;
            }
        }

        new_lines.extend(tests.iter().map(|test| format!("test!({test});")));

        for line in &mut old_lines {
            if line == "// >>>>>>> discovered_tests" {
                new_lines.push(line.to_owned());
                break;
            }
        }

        new_lines.extend(old_lines.map(|line| line.to_owned()));

        write("tests/parse.rs", new_lines.join("\n")).unwrap();

        for test in &tests {
            let output = format!("tests/outputs/{test}.dbg");

            if !exists(&output).unwrap() {
                write(&output, []).unwrap();
            }
        }
    }

    assert_eq!(tests, discovered_tests);
}
