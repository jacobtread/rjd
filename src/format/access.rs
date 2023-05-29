use std::fmt::{Display, Write};

use super::reader::{ByteReadable, ByteReader, ReadResult};
use bitflags::bitflags;

// CLASS ACCESS FLAGS
// --------------------------
// Flag Name	    Value	Interpretation
// ACC_PUBLIC	    0x0001	Declared public; may be accessed from outside its package.
// ACC_FINAL	    0x0010	Declared final; no subclasses allowed.
// ACC_SUPER	    0x0020	Treat superclass methods specially when invoked by the invokespecial instruction.
// ACC_INTERFACE	0x0200	Is an interface, not a class.
// ACC_ABSTRACT	    0x0400	Declared abstract; must not be instantiated.
// ACC_SYNTHETIC	0x1000	Declared synthetic; not present in the source code.
// ACC_ANNOTATION	0x2000	Declared as an annotation type.
// ACC_ENUM	        0x4000  Declared as an enum type.

bitflags! {
    #[derive(Debug)]
    pub struct ClassAccessFlags: u16 {
        const PUBLIC     = 0x0001;
        const FINAL      = 0x0010;
        const SUPER      = 0x0020;
        const INTERFACE  = 0x0200;
        const ABSTRACT   = 0x0400;
        const SYNTHETIC  = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM       = 0x4000;
    }
}

impl Display for ClassAccessFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.contains(ClassAccessFlags::PUBLIC) {
            f.write_str("public ")?;
        }

        if self.contains(ClassAccessFlags::FINAL) {
            f.write_str("final ")?;
        } else if self.contains(ClassAccessFlags::ABSTRACT) {
            f.write_str("abstract ")?;
        }

        f.write_str(if self.contains(ClassAccessFlags::INTERFACE) {
            "interface"
        } else if self.contains(ClassAccessFlags::ANNOTATION) {
            "@interface"
        } else if self.contains(ClassAccessFlags::ENUM) {
            "enum"
        } else {
            "class"
        })
    }
}

// NESTED CLASS ACCESS FLAGS
// --------------------------
// Flag Name	  Value	    Interpretation
// ACC_PUBLIC	  0x0001	Marked or implicitly public in source.
// ACC_PRIVATE	  0x0002	Marked private in source.
// ACC_PROTECTED  0x0004	Marked protected in source.
// ACC_STATIC	  0x0008	Marked or implicitly static in source.
// ACC_FINAL	  0x0010	Marked final in source.
// ACC_INTERFACE  0x0200	Was an interface in source.
// ACC_ABSTRACT	  0x0400	Marked or implicitly abstract in source.
// ACC_SYNTHETIC  0x1000	Declared synthetic; not present in the source code.
// ACC_ANNOTATION 0x2000	Declared as an annotation type.
// ACC_ENUM	      0x4000	Declared as an enum type.
bitflags! {
    #[derive(Debug)]
    pub struct NestedClassAccessFlags: u16 {
        const PUBLIC     = 0x0001;
        const PRIVATE    = 0x0002;
        const PROTECTED  = 0x0004;
        const STATIC     = 0x0008;
        const FINAL      = 0x0010;
        const INTERFACE  = 0x0200;
        const ABSTRACT   = 0x0400;
        const SYNTHETIC  = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM       = 0x4000;
    }
}

impl Display for NestedClassAccessFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.contains(NestedClassAccessFlags::PUBLIC) {
            f.write_str("public ")?;
        } else if self.contains(NestedClassAccessFlags::PRIVATE) {
            f.write_str("private ")?;
        } else if self.contains(NestedClassAccessFlags::PROTECTED) {
            f.write_str("protected ")?;
        }

        if self.contains(NestedClassAccessFlags::FINAL) {
            f.write_str("final ")?;
        } else if self.contains(NestedClassAccessFlags::ABSTRACT) {
            f.write_str("abstract ")?;
        }

        f.write_str(if self.contains(NestedClassAccessFlags::INTERFACE) {
            "interface"
        } else if self.contains(NestedClassAccessFlags::ANNOTATION) {
            "@interface"
        } else if self.contains(NestedClassAccessFlags::ENUM) {
            "enum"
        } else {
            "class"
        })
    }
}

// FIELD ACCESS FLAGS
// --------------------------
// Flag Name	    Value	Interpretation
// ACC_PUBLIC	    0x0001	Declared public; may be accessed from outside its package.
// ACC_PRIVATE	    0x0002	Declared private; usable only within the defining class.
// ACC_PROTECTED	0x0004	Declared protected; may be accessed within subclasses.
// ACC_STATIC	    0x0008	Declared static.
// ACC_FINAL	    0x0010	Declared final; never directly assigned to after object construction (JLS §17.5).
// ACC_VOLATILE	    0x0040	Declared volatile; cannot be cached.
// ACC_TRANSIENT	0x0080	Declared transient; not written or read by a persistent object manager.
// ACC_SYNTHETIC	0x1000	Declared synthetic; not present in the source code.
// ACC_ENUM	        0x4000	Declared as an element of an enum.

bitflags! {
    #[derive(Debug)]
    pub struct FieldAccessFlags: u16 {
        const PUBLIC        = 0x0001;
        const PRIVATE       = 0x0002;
        const PROTECTED     = 0x0004;
        const STATIC        = 0x0008;
        const FINAL         = 0x0010;
        const VOLATILE      = 0x0040;
        const TRANSIENT     = 0x0080;
        const SYNTHETIC     = 0x1000;
        const ENUM          = 0x4000;
    }
}

impl Display for FieldAccessFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut spaced = true;
        if self.contains(FieldAccessFlags::PUBLIC) {
            f.write_str("public ")?;
        } else if self.contains(FieldAccessFlags::PRIVATE) {
            f.write_str("private ")?;
        } else if self.contains(FieldAccessFlags::PROTECTED) {
            f.write_str("protected ")?;
        } else {
            spaced = false;
        }

        if self.contains(FieldAccessFlags::VOLATILE) {
            f.write_str("volatile ")?;
            spaced = true;
        }

        if self.contains(FieldAccessFlags::TRANSIENT) {
            f.write_str("transient ")?;
            spaced = true;
        }

        if self.contains(FieldAccessFlags::STATIC) {
            f.write_str("static ")?;
            spaced = true;
        }

        if self.contains(FieldAccessFlags::FINAL) {
            f.write_str("final ")?;
            spaced = true;
        }

        if !spaced {
            f.write_char(' ')?;
        }

        Ok(())
    }
}

// METHOD ACCESS FLAGS
// --------------------------
// Flag Name	Value	Interpretation
// ACC_PUBLIC	0x0001	Declared public; may be accessed from outside its package.
// ACC_PRIVATE	0x0002	Declared private; accessible only within the defining class.
// ACC_PROTECTED	0x0004	Declared protected; may be accessed within subclasses.
// ACC_STATIC	0x0008	Declared static.
// ACC_FINAL	0x0010	Declared final; must not be overridden (§5.4.5).
// ACC_SYNCHRONIZED	0x0020	Declared synchronized; invocation is wrapped by a monitor use.
// ACC_BRIDGE	0x0040	A bridge method, generated by the compiler.
// ACC_VARARGS	0x0080	Declared with variable number of arguments.
// ACC_NATIVE	0x0100	Declared native; implemented in a language other than Java.
// ACC_ABSTRACT	0x0400	Declared abstract; no implementation is provided.
// ACC_STRICT	0x0800	Declared strictfp; floating-point mode is FP-strict.
// ACC_SYNTHETIC	0x1000	Declared synthetic; not present in the source code.

bitflags! {
    #[derive(Debug)]
    pub struct MethodAccessFlags: u16 {
        const PUBLIC        = 0x0001;
        const PRIVATE       = 0x0002;
        const PROTECTED     = 0x0004;
        const STATIC        = 0x0008;
        const FINAL         = 0x0010;
        const SYNCHRONIZED  = 0x0020;
        const BRIDGE        = 0x0040;
        const VARARGS       = 0x0080;
        const NATIVE        = 0x0100;
        const ABSTRACT      = 0x0400;
        const STRICT        = 0x0800;
        const SYNTHETIC     = 0x1000;
    }
}

impl ByteReadable<'_> for ClassAccessFlags {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let bits = r.u2()?;
        Ok(Self::from_bits_truncate(bits))
    }
}

impl ByteReadable<'_> for FieldAccessFlags {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let bits = r.u2()?;
        Ok(Self::from_bits_truncate(bits))
    }
}

impl ByteReadable<'_> for MethodAccessFlags {
    fn read(r: &mut ByteReader<'_>) -> ReadResult<Self> {
        let bits = r.u2()?;
        Ok(Self::from_bits_truncate(bits))
    }
}
