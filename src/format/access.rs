use super::reader::{ByteReadable, ByteReader, ReadError};
use bitflags::bitflags;

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

    pub struct AccessFlags: u16 {
        const PUBLIC     = 0b00000001;
        const FINAL      = 0b00010000;
        const SUPER      = 0b00100000;
        const INTERFACE  = 0b001000000000;
        const ABSTRACT   = 0b010000000000;
        const SYNTHETIC  = 0b0001000000000000;
        const ANNOTATION = 0b0010000000000000;
        const ENUM       = 0b0100000000000000;
    }

}

impl ByteReadable<'_> for AccessFlags {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let bits = r.u2()?;
        Ok(AccessFlags::from_bits_truncate(bits))
    }
}
