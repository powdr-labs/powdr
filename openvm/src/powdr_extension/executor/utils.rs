use core::mem::{size_of, MaybeUninit};
use core::ptr;

/// Error for (de)serialization cursor operations.
#[derive(Debug, Clone, Copy)]
enum PrecomputeSerError {
    BufferTooSmall,
}
type SerResult<T> = Result<T, PrecomputeSerError>;

pub struct BufWriter<'a> {
    buf: &'a mut [u8],
    pos: usize,
}
impl<'a> BufWriter<'a> {
    #[inline]
    pub fn new(buf: &'a mut [u8]) -> Self {
        Self { buf, pos: 0 }
    }

    #[inline]
    fn remaining(&self) -> usize {
        self.buf.len().saturating_sub(self.pos)
    }

    #[inline]
    pub fn write_usize(&mut self, v: usize) -> SerResult<()> {
        let n = size_of::<usize>();
        if self.remaining() < n {
            return Err(PrecomputeSerError::BufferTooSmall);
        }
        let bytes = v.to_le_bytes();
        // Copy only the first n bytes (usize::to_le_bytes() is always n long).
        self.buf[self.pos..self.pos + n].copy_from_slice(&bytes[..n]);
        self.pos += n;
        Ok(())
    }

    #[inline]
    pub fn write_bytes(&mut self, src: &[u8]) -> SerResult<()> {
        if self.remaining() < src.len() {
            return Err(PrecomputeSerError::BufferTooSmall);
        }
        self.buf[self.pos..self.pos + src.len()].copy_from_slice(src);
        self.pos += src.len();
        Ok(())
    }

    // Returns a &mut [u8] window of length `len` and advances the cursor.
    #[inline]
    pub fn reserve_mut(&mut self, len: usize) -> SerResult<&mut [u8]> {
        if self.remaining() < len {
            return Err(PrecomputeSerError::BufferTooSmall);
        }
        let start = self.pos;
        self.pos += len;
        Ok(&mut self.buf[start..start + len])
    }

    #[inline]
    pub fn write_exec_func<F, Ctx>(
        &mut self,
        f: openvm_circuit::arch::ExecuteFunc<F, Ctx>,
    ) -> SerResult<()>
    where
        F: openvm_stark_backend::p3_field::PrimeField32,
        Ctx: openvm_circuit::arch::ExecutionCtxTrait,
    {
        let n = size_of::<openvm_circuit::arch::ExecuteFunc<F, Ctx>>();
        if self.remaining() < n {
            return Err(PrecomputeSerError::BufferTooSmall);
        }
        unsafe {
            let src_ptr: *const u8 = (&f as *const _ as *const u8);
            let dst_ptr: *mut u8 = self.buf.as_mut_ptr().add(self.pos);
            // Copy over the raw bytes of the function pointer
            ptr::copy_nonoverlapping(src_ptr, dst_ptr, n);
        }
        self.pos += n;
        Ok(())
    }
}

pub struct BufReader<'a> {
    buf: &'a [u8],
    pos: usize,
}
impl<'a> BufReader<'a> {
    #[inline]
    pub fn new(buf: &'a [u8]) -> Self {
        Self { buf, pos: 0 }
    }

    #[inline]
    pub fn remaining(&self) -> usize {
        self.buf.len().saturating_sub(self.pos)
    }

    #[inline]
    pub fn read_usize(&mut self) -> SerResult<usize> {
        let n = size_of::<usize>();
        if self.remaining() < n {
            return Err(PrecomputeSerError::BufferTooSmall);
        }
        let mut arr = [0u8; size_of::<usize>()];
        arr.copy_from_slice(&self.buf[self.pos..self.pos + n]);
        self.pos += n;
        Ok(usize::from_le_bytes(arr))
    }

    #[inline]
    pub fn read_bytes(&mut self, len: usize) -> SerResult<&'a [u8]> {
        if self.remaining() < len {
            return Err(PrecomputeSerError::BufferTooSmall);
        }
        let start = self.pos;
        self.pos += len;
        Ok(&self.buf[start..start + len])
    }

    #[inline]
    pub fn read_exec_func<F, Ctx>(&mut self) -> SerResult<openvm_circuit::arch::ExecuteFunc<F, Ctx>>
    where
        F: openvm_stark_backend::p3_field::PrimeField32,
        Ctx: openvm_circuit::arch::ExecutionCtxTrait,
    {
        let n = size_of::<openvm_circuit::arch::ExecuteFunc<F, Ctx>>();
        if self.remaining() < n {
            return Err(PrecomputeSerError::BufferTooSmall);
        }
        let mut uninit = MaybeUninit::<openvm_circuit::arch::ExecuteFunc<F, Ctx>>::uninit();
        unsafe {
            let src_ptr: *const u8 = self.buf.as_ptr().add(self.pos);
            let dst_ptr: *mut u8 = uninit.as_mut_ptr() as *mut u8;
            ptr::copy_nonoverlapping(src_ptr, dst_ptr, n);
            self.pos += n;
            Ok(uninit.assume_init())
        }
    }
}
