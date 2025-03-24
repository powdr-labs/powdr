use core::arch::asm;
use core::iter::FusedIterator;
use core::slice;

extern crate alloc;

use powdr_syscalls::Syscall;

use alloc::vec;
use alloc::vec::Vec;

/// An iterator over the static prover data.
#[derive(Copy, Clone)]
pub struct ProverDataReader {
    remaining_data: &'static [u32],
}

use spin::Mutex;

static PROVER_DATA_READER: Mutex<Option<ProverDataReader>> = Mutex::new(None);

// Initialize the reader once and get a mutable reference
fn get_prover_data_reader() -> spin::MutexGuard<'static, Option<ProverDataReader>> {
    let mut reader = PROVER_DATA_READER.lock();
    if reader.is_none() {
        *reader = Some(ProverDataReader::new());
    }
    reader
}

impl ProverDataReader {
    /// Creates an iterator over the static prover data.
    ///
    /// A newly created iterator will start at the beginning of the prover data.
    pub fn new() -> Self {
        extern "C" {
            // The prover data start and end symbols. Their addresses are set by the linker.
            static __powdr_prover_data_start: u32;
            static __powdr_prover_data_end: u32;
        }
        const POWDR_PAGE_SIZE: isize = 2048;

        unsafe {
            // We skip the first page of the prover data, as it used as salt to
            // randomize its merkle tree node.
            let region_start: *const u32 = &__powdr_prover_data_start;
            let data_start = region_start.byte_offset(POWDR_PAGE_SIZE);
            let data_end: *const u32 = &__powdr_prover_data_end;

            let prover_data_section =
                slice::from_raw_parts(data_start, data_end.offset_from(data_start) as usize);

            // The first word of the prover data section is the total number of words the user wrote.
            let (&total_words, remaining_data) = prover_data_section.split_first().unwrap();

            let remaining_data = &remaining_data[..total_words as usize];
            Self { remaining_data }
        }
    }
}

impl Iterator for ProverDataReader {
    type Item = &'static [u8];

    /// Returns the next slice of prover data.
    ///
    /// Because it is in static memory, the reference can be stored, passed around and will always be valid.
    ///
    /// The start of the slice is guaranteed to be 4-bytes aligned.
    fn next(&mut self) -> Option<&'static [u8]> {
        if self.remaining_data.is_empty() {
            return None;
        }

        let (&len_bytes, remaining) = self.remaining_data.split_first().unwrap();
        let len_words = (len_bytes + 3) / 4;
        let (data, remaining) = remaining.split_at(len_words as usize);
        self.remaining_data = remaining;

        // SAFETY: It is safe to cast an u32 slice to an u8 slice, which is the most general type.
        unsafe {
            let data_ptr = data.as_ptr() as *const u8;
            Some(slice::from_raw_parts(data_ptr, len_bytes as usize))
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.remaining_data.is_empty() {
            (0, Some(0))
        } else {
            // At the minimum the size is 1 (the next slice can contain all remaining words);
            // At the maximum the size is len (every remaining word define a new slice with 0 elements).
            (1, Some(self.remaining_data.len()))
        }
    }
}

impl FusedIterator for ProverDataReader {}

/// A single u32 from input channel 0.
pub fn read_u32(idx: u32) -> u32 {
    let mut value: u32;
    unsafe {
        ecall!(Syscall::Input, lateout("a0") value, in("a0") 0, in("a1") idx + 1);
    }
    value
}

/// Reads data.len() u32s from the file descriptor fd into the data slice.
pub fn read_slice(fd: u32, data: &mut [u32]) {
    for (i, d) in data.iter_mut().enumerate() {
        unsafe {
            ecall!(Syscall::Input, lateout("a0") *d, in("a0") fd, in("a1") (i+1) as u32);
        };
    }
}

/// Reads the length of the data first at index 0, then the data itself.
pub fn read_data_len(fd: u32) -> usize {
    let mut out: u32;
    unsafe {
        ecall!(Syscall::Input, lateout("a0") out, in("a0") fd, in("a1") 0);
    };
    out as usize
}

/// Writes a single u8 to the file descriptor fd.
pub fn write_u8(fd: u32, byte: u8) {
    unsafe {
        ecall!(Syscall::Output, in("a0") fd, in("a1") byte);
    }
}

/// Writes data.len() u8s from the data slice to the file descriptor fd.
pub fn write_slice(fd: u32, data: &[u8]) {
    for byte in data {
        write_u8(fd, *byte);
    }
}

use serde::de::DeserializeOwned;
use serde::Serialize;

pub fn read<T: DeserializeOwned>() -> T {
    let mut reader = get_prover_data_reader();
    if let Some(ref mut reader) = *reader {
        let slice = reader.next().unwrap();
        serde_cbor::from_slice(slice).unwrap()
    } else {
        panic!("powdr ProverDataReader not available");
    }
}

pub fn read_bytes() -> &'static [u8] {
    let mut reader = get_prover_data_reader();
    if let Some(ref mut reader) = *reader {
        reader.next().unwrap()
    } else {
        panic!("powdr ProverDataReader not available");
    }
}

/// Reads and deserializes a serialized value of type T from the file descriptor fd.
pub fn read_fd<T: DeserializeOwned>(fd: u32) -> T {
    let l = read_data_len(fd);
    let mut data = vec![0; l];
    read_slice(fd, &mut data);

    // TODO this extra conversion can be removed if we change everything to be u8
    let data: Vec<u8> = data.into_iter().map(|x| x as u8).collect();

    serde_cbor::from_slice(data.as_slice()).unwrap()
}

/// Serializes and writes a value of type T to the file descriptor fd.
pub fn write<T: Serialize>(fd: u32, data: T) {
    let data = serde_cbor::to_vec(&data).unwrap();
    write_slice(fd, &data);
}
