use std::fmt::Debug;

use thiserror::Error;

/// Structure for reading over a slice of bytes using a cursor
/// of maintaining the current position
pub struct ByteReader<'a> {
    /// The slice of bytes to read from
    buffer: &'a [u8],
    /// The current position in the buffer
    cursor: usize,
}

impl<'a> ByteReader<'a> {
    pub fn new(buffer: &[u8]) -> ByteReader {
        ByteReader { buffer, cursor: 0 }
    }

    /// Borrows a slice of the underlying buffer of the provided
    /// length moving the cursor over the length
    ///
    /// `length` The length of the slice
    pub fn slice(&mut self, length: usize) -> ReadResult<&'a [u8]> {
        self.reserve(length)?;

        let start = self.cursor;
        self.cursor += length;
        let slice = &self.buffer[start..self.cursor];
        Ok(slice)
    }

    /// Consumes a 1 byte unsigned value from the
    /// underlying buffer moving the cursor along
    pub fn u1(&mut self) -> ReadResult<u8> {
        self.reserve(1)?;

        // Get value and move cursor
        let value = self.buffer[self.cursor];
        self.cursor += 1;
        Ok(value)
    }

    pub fn u2(&mut self) -> ReadResult<u16> {
        let slice = self.slice(2)?;
        let mut value: [u8; 2] = [0; 2];
        value.copy_from_slice(slice);
        let value = u16::from_be_bytes(value);
        Ok(value)
    }

    fn slice_4(&mut self) -> ReadResult<[u8; 4]> {
        let slice = self.slice(4)?;
        let mut value: [u8; 4] = [0; 4];
        value.copy_from_slice(slice);
        Ok(value)
    }

    fn slice_8(&mut self) -> ReadResult<[u8; 8]> {
        let slice = self.slice(8)?;
        let mut value: [u8; 8] = [0; 8];
        value.copy_from_slice(slice);
        Ok(value)
    }

    pub fn u4(&mut self) -> ReadResult<u32> {
        let value = self.slice_4()?;
        let value = u32::from_be_bytes(value);
        Ok(value)
    }

    /// Checks if the buffer has atleast the provided `amount`
    /// after the current cusor position
    pub fn reserve(&self, amount: usize) -> ReadResult<()> {
        if self.cursor + amount > self.buffer.len() {
            Err(ReadError::NotEnoughBytes {
                length: self.buffer.len(),
                cursor: self.cursor,
                needed: amount,
                remaining: self.remaining(),
            })
        } else {
            Ok(())
        }
    }

    pub fn u2_list<R, E>(&mut self, sub: bool) -> Result<Vec<R>, E>
    where
        R: ByteReadable<'a, Error = E> + Debug,
        E: From<ReadError>,
    {
        let mut count = self.u2()?;
        if sub {
            count -= 1;
        }
        let mut list = Vec::with_capacity(count as usize);
        for _ in 0..count {
            let value = R::read(self)?;
            list.push(value);
        }
        Ok(list)
    }

    pub fn remaining(&self) -> usize {
        self.buffer.len() - self.cursor
    }
}

impl ByteReadable<'_> for i32 {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let value = r.slice_4()?;
        let value = i32::from_be_bytes(value);
        Ok(value)
    }
}

impl ByteReadable<'_> for f32 {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let value = r.slice_4()?;
        let value = f32::from_be_bytes(value);
        Ok(value)
    }
}

impl ByteReadable<'_> for i64 {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let value = r.slice_8()?;
        let value = i64::from_be_bytes(value);
        Ok(value)
    }
}

impl ByteReadable<'_> for f64 {
    type Error = ReadError;

    fn read(r: &mut ByteReader<'_>) -> Result<Self, Self::Error> {
        let value = r.slice_8()?;
        let value = f64::from_be_bytes(value);
        Ok(value)
    }
}

#[derive(Debug, Error)]
pub enum ReadError {
    /// There wasn't enough bytes present to finish reading
    #[error("Not enough bytes to finish reading")]
    NotEnoughBytes {
        /// The length of the buffer being read from
        length: usize,
        /// The current cursor position
        cursor: usize,
        /// The number of bytes left after the cursor
        remaining: usize,
        /// The number of bytes required
        needed: usize,
    },
}

pub type ReadResult<T> = Result<T, ReadError>;

pub trait ByteReadable<'a>: Sized {
    type Error;

    fn read(r: &mut ByteReader<'a>) -> Result<Self, Self::Error>;
}
