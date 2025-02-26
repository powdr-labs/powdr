/// The custom serialization format expected by estark
use std::{
    io::Write,
    io::{self},
    path::Path,
};

use powdr_number::FieldElement;

use super::buffered_write_file;

pub fn write_polys_file<F: FieldElement>(
    path: &Path,
    polys: &[(String, Vec<F>)],
) -> Result<(), io::Error> {
    buffered_write_file(path, |writer| write_polys_stream(writer, polys))??;

    Ok(())
}

fn ceil_div(num: usize, div: usize) -> usize {
    num.div_ceil(div)
}

fn write_polys_stream<T: FieldElement>(
    file: &mut impl Write,
    polys: &[(String, Vec<T>)],
) -> Result<(), io::Error> {
    let ceil_div = ceil_div(T::BITS as usize, 64);
    let width = ceil_div * 8;

    if polys.is_empty() {
        return Ok(());
    }

    // TODO maybe the witness should have a proper type that
    // explicitly has a degree or length?
    let degree = polys[0].1.len();
    for (_, values) in polys {
        assert_eq!(values.len(), degree);
    }

    for i in 0..degree {
        for (_, values) in polys {
            let bytes = values[i].to_bytes_le();
            assert_eq!(bytes.len(), width);
            file.write_all(&bytes)?;
        }
    }

    Ok(())
}
