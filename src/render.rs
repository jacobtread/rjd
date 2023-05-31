use std::fmt::{Display, Write};

use classfile::class::{AccessFlags, ClassFile};

struct JavaClassRenderer<'a> {
    class: ClassFile<'a>,
}

fn field_flags_fmt(flags: &AccessFlags, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut spaced = true;
    if flags.contains(AccessFlags::PUBLIC) {
        f.write_str("public ")?;
    } else if flags.contains(AccessFlags::PRIVATE) {
        f.write_str("private ")?;
    } else if flags.contains(AccessFlags::PROTECTED) {
        f.write_str("protected ")?;
    } else {
        spaced = false;
    }

    if flags.contains(AccessFlags::VOLATILE) {
        f.write_str("volatile ")?;
        spaced = true;
    }

    if flags.contains(AccessFlags::TRANSIENT) {
        f.write_str("transient ")?;
        spaced = true;
    }

    if flags.contains(AccessFlags::STATIC) {
        f.write_str("static ")?;
        spaced = true;
    }

    if flags.contains(AccessFlags::FINAL) {
        f.write_str("final ")?;
        spaced = true;
    }

    if !spaced {
        f.write_char(' ')?;
    }

    Ok(())
}

fn class_flags_fmt(flags: &AccessFlags, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    if flags.contains(AccessFlags::PUBLIC) {
        f.write_str("public ")?;
    } else if flags.contains(AccessFlags::PRIVATE) {
        f.write_str("private ")?;
    } else if flags.contains(AccessFlags::PROTECTED) {
        f.write_str("protected ")?;
    }

    if flags.contains(AccessFlags::FINAL) {
        f.write_str("final ")?;
    } else if flags.contains(AccessFlags::ABSTRACT) {
        f.write_str("abstract ")?;
    }

    f.write_str(if flags.contains(AccessFlags::INTERFACE) {
        "interface"
    } else if flags.contains(AccessFlags::ANNOTATION) {
        "@interface"
    } else if flags.contains(AccessFlags::ENUM) {
        "enum"
    } else {
        "class"
    })
}

fn method_flags_fmt(flags: &AccessFlags, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let mut spaced = true;
    if flags.contains(AccessFlags::PUBLIC) {
        f.write_str("public ")?;
    } else if flags.contains(AccessFlags::PRIVATE) {
        f.write_str("private ")?;
    } else if flags.contains(AccessFlags::PROTECTED) {
        f.write_str("protected ")?;
    } else {
        spaced = false;
    }

    if flags.contains(AccessFlags::SYNCHRONIZED) {
        f.write_str("syncronized ")?;
        spaced = true;
    }

    if flags.contains(AccessFlags::TRANSIENT) {
        f.write_str("transient ")?;
        spaced = true;
    }

    if flags.contains(AccessFlags::STATIC) {
        f.write_str("static ")?;
        spaced = true;
    }

    if flags.contains(AccessFlags::FINAL) {
        f.write_str("final ")?;
        spaced = true;
    }

    if !spaced {
        f.write_char(' ')?;
    }

    Ok(())
}

impl Display for JavaClassRenderer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let class = &self.class;
        let this_class = &class.this_class;
        let package = &this_class.packages;

        if !package.is_empty() {
            writeln!(f, "package {};", package.join("."))?;
        }

        f.write_char('\n')?;

        class_flags_fmt(&class.access_flags, f)?;

        f.write_char(' ')?;
        f.write_str(this_class.class)?;
        f.write_str(" {\n\n")?;

        for field in &class.fields {
            f.write_str("  ")?;
            field_flags_fmt(&field.access_flags, f)?;
            field.descriptor.fmt(f)?;
            f.write_char(' ')?;
            f.write_str(field.name)?;
            f.write_str(";\n")?;
        }
        f.write_char('\n')?;

        if !class.methods.is_empty() {
            for method in &class.methods {
                f.write_str("  ")?;
                method_flags_fmt(&method.access_flags, f)?;
                write!(
                    f,
                    "{} {} () {{}}",
                    &method.descriptor.return_type, &method.name
                )?;
                f.write_char('\n')?;
            }

            f.write_char('\n')?;
        }

        f.write_str("}")?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::fs::read;

    use classfile::class::parse_class_file;

    use crate::render::JavaClassRenderer;

    #[test]
    fn test_render_class() {
        let file = read("tests/ExampleStringLoop.class").unwrap();
        let (_, class_file) = parse_class_file(&file).unwrap();

        let render = JavaClassRenderer { class: class_file };
        println!("{}", render)
    }
}
