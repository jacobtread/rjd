use std::fmt::{Display, Write};

use super::class::ClassFile;

struct JavaClassRenderer<'a> {
    class: ClassFile<'a>,
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

        class.access_flags.fmt(f)?;

        f.write_char(' ')?;
        f.write_str(this_class.class)?;
        f.write_str(" {\n\n")?;

        for field in &class.fields {
            f.write_str("  ")?;
            field.access_flags.fmt(f)?;
            field.descriptor.fmt(f)?;
            f.write_char(' ')?;
            f.write_str(field.name)?;
            f.write_str(";\n")?;
        }
        f.write_char('\n')?;

        f.write_str("}")?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::fs::read;

    use crate::{
        class::{class::ClassFile, render::JavaClassRenderer},
        format::class::RawClassFile,
    };

    #[test]
    fn test_render_class() {
        let file = read("tests/ExampleStringLoop.class").unwrap();
        let class_file = RawClassFile::try_read(&file).unwrap();
        let class_file = ClassFile::try_from(class_file).unwrap();
        dbg!(&class_file.fields);
        dbg!(&class_file.methods);
        dbg!(&class_file.attributes);
        let render = JavaClassRenderer { class: class_file };
        println!("{}", render)
    }
}
