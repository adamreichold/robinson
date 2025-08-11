use robinson::{Document, Options};

#[test]
fn parsed_entity_can_provide_text() {
    let text = r#"<!DOCTYPE foo [<!ENTITY bar SYSTEM "baz.xml">]> <qux>&bar;</qux>"#;

    let mut entity_resolver = |_pub_id: Option<&str>, _uri: &str| Ok(Some("foobar"));

    let opts = Options {
        entity_resolver: Some(&mut entity_resolver),
    };

    let doc = Document::parse_with_opts(text, opts).unwrap();

    assert_eq!(doc.root_element().child_text().unwrap(), "foobar");
}

#[test]
fn parsed_entity_can_provide_attribute_value() {
    let text = r#"<!DOCTYPE foo [<!ENTITY bar SYSTEM "baz.xml">]> <qux bar="&bar;"/>"#;

    let mut entity_resolver = |_pub_id: Option<&str>, _uri: &str| Ok(Some("foobar"));

    let opts = Options {
        entity_resolver: Some(&mut entity_resolver),
    };

    let doc = Document::parse_with_opts(text, opts).unwrap();

    assert_eq!(doc.root_element().attribute("bar").unwrap(), "foobar");
}

#[test]
fn parsed_entity_can_include_text_declaration() {
    let text = r#"<!DOCTYPE foo [<!ENTITY bar SYSTEM "baz.xml">]> <qux>&bar;</qux>"#;

    let mut entity_resolver =
        |_pub_id: Option<&str>, _uri: &str| Ok(Some(r#"<?xml version="1.0"?><foobar/>"#));

    let opts = Options {
        entity_resolver: Some(&mut entity_resolver),
    };

    let doc = Document::parse_with_opts(text, opts).unwrap();

    assert_eq!(
        doc.root_element()
            .children()
            .next()
            .unwrap()
            .name()
            .unwrap(),
        "foobar"
    );
}
