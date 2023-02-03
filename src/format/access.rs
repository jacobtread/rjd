use super::reader::{ByteReadable, ByteReader, ReadError};
use bitflags::bitflags;

// Flag Name	    Value	Interpretation
// ACC_PUBLIC	    0x0001	Declared public; may be accessed from outside its package.
// ACC_PRIVATE	    0x0002	Declared private; usable only within the defining class.
// ACC_PROTECTED	0x0004	Declared protected; may be accessed within subclasses.
// ACC_STATIC	    0x0008	Declared static.
// ACC_FINAL	    0x0010	Declared final; no subclasses allowed.
// ACC_SUPER	    0x0020	Treat superclass methods specially when invoked by the invokespecial instruction.
// ACC_VOLATILE	    0x0040	Declared volatile; cannot be cached.
// ACC_TRANSIENT	0x0080	Declared transient; not written or read by a persistent object manager.
// ACC_INTERFACE	0x0200	Is an interface, not a class.
// ACC_ABSTRACT	    0x0400	Declared abstract; must not be instantiated.
// ACC_SYNTHETIC	0x1000	Declared synthetic; not present in the source code.
// ACC_ANNOTATION	0x2000	Declared as an annotation type.
// ACC_ENUM	        0x4000  Declared as an enum type.
bitflags! {

    pub struct AccessFlags: u16 {
        const PUBLIC     = 0x0001;
        const PRIVATE    = 0x0002;
        const PROTECTED  = 0x0004;
        const STATIC     = 0x0008;
        const FINAL      = 0x0010;
        const SUPER      = 0x0020;
        const VOLATILE   = 0x0040;
        const TRANSIENT  = 0x0080;
        const INTERFACE  = 0x0200;
        const ABSTRACT   = 0x0400;
        const SYNTHETIC  = 0x1000;
        const ANNOTATION = 0x2000;
        const ENUM       = 0x4000;
    }

}

impl ByteReadable<'_> for AccessFlags {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let bits = r.u2()?;
        Ok(AccessFlags::from_bits_truncate(bits))
    }
}
