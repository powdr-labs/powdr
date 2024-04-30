use std::{
    fs::File,
    io::{self, BufWriter, Read, Write},
    path::Path,
};

use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Compress, Validate};
use csv::{Reader, Writer};
use serde_with::{DeserializeAs, SerializeAs};

use crate::{DegreeType, FieldElement};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CsvRenderMode {
    SignedBase10,
    UnsignedBase10,
    Hex,
}

impl Default for CsvRenderMode {
    fn default() -> Self {
        Self::Hex
    }
}

const ROW_NAME: &str = "Row";

pub fn write_polys_csv_file<T: FieldElement>(
    file: impl Write,
    render_mode: CsvRenderMode,
    polys: &[&(String, Vec<T>)],
) {
    let mut writer = Writer::from_writer(file);

    // Write headers, adding a "Row" column
    let mut headers = vec![ROW_NAME];
    headers.extend(polys.iter().map(|(name, _)| {
        assert!(name != ROW_NAME);
        name.as_str()
    }));
    writer.write_record(&headers).unwrap();

    let len = polys[0].1.len();
    for row_index in 0..len {
        let mut row = Vec::new();
        row.push(format!("{}", row_index));
        for (_, values) in polys {
            assert!(values.len() == len);
            let value = match render_mode {
                CsvRenderMode::SignedBase10 => format!("{}", values[row_index]),
                CsvRenderMode::UnsignedBase10 => format!("{}", values[row_index].to_integer()),
                CsvRenderMode::Hex => format!("0x{:x}", values[row_index].to_integer()),
            };
            row.push(value);
        }
        writer.write_record(&row).unwrap();
    }

    writer.flush().unwrap();
}

pub fn read_polys_csv_file<T: FieldElement>(file: impl Read) -> Vec<(String, Vec<T>)> {
    let mut reader = Reader::from_reader(file);
    let headers = reader.headers().unwrap();

    let mut polys = headers
        .iter()
        .map(|name| (name.to_string(), Vec::new()))
        .collect::<Vec<_>>();

    for result in reader.records() {
        let record = result.unwrap();
        for (idx, value) in record.iter().enumerate() {
            let value = if let Some(value) = value.strip_prefix("0x") {
                T::from_str_radix(value, 16).unwrap()
            } else if let Some(value) = value.strip_prefix('-') {
                -T::from_str(value).unwrap()
            } else {
                T::from_str(value).unwrap()
            };
            polys[idx].1.push(value);
        }
    }

    // Remove "Row" column, which was added by write_polys_csv_file()
    polys
        .into_iter()
        .filter(|(name, _)| name != ROW_NAME)
        .collect()
}

fn ceil_div(num: usize, div: usize) -> usize {
    (num + div - 1) / div
}

pub fn buffered_write_file<R>(
    path: &Path,
    do_write: impl FnOnce(&mut BufWriter<File>) -> R,
) -> Result<R, io::Error> {
    let mut writer = BufWriter::new(File::create(path)?);
    let result = do_write(&mut writer);
    writer.flush()?;

    Ok(result)
}

pub fn write_polys_file<F: FieldElement>(
    path: &Path,
    polys: &[(String, Vec<F>)],
) -> Result<(), io::Error> {
    buffered_write_file(path, |writer| write_polys_stream(writer, polys))??;

    Ok(())
}

fn write_polys_stream<T: FieldElement>(
    file: &mut impl Write,
    polys: &[(String, Vec<T>)],
) -> Result<(), io::Error> {
    let width = ceil_div(T::BITS as usize, 64) * 8;

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
        for (_name, constant) in polys {
            let bytes = constant[i].to_bytes_le();
            assert_eq!(bytes.len(), width);
            file.write_all(&bytes)?;
        }
    }

    Ok(())
}

pub fn read_polys_file<T: FieldElement>(
    file: &mut impl Read,
    columns: &[String],
) -> (Vec<(String, Vec<T>)>, DegreeType) {
    assert!(!columns.is_empty());
    let width = ceil_div(T::BITS as usize, 64) * 8;

    let bytes_to_read = width * columns.len();

    let mut result: Vec<(_, Vec<T>)> = columns
        .iter()
        .map(|name| (name.to_string(), vec![]))
        .collect();
    let mut degree = 0;

    loop {
        let mut buf = vec![0u8; bytes_to_read];
        match file.read_exact(&mut buf) {
            Ok(()) => {}
            Err(_) => return (result, degree),
        }
        degree += 1;
        result
            .iter_mut()
            .zip(buf.chunks(width))
            .for_each(|((_, values), bytes)| {
                values.push(T::from_bytes_le(bytes));
            });
    }
}

// Serde wrappers for serialize/deserialize

pub fn ark_se<S, A: CanonicalSerialize>(a: &A, s: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut bytes = vec![];
    a.serialize_with_mode(&mut bytes, Compress::Yes)
        .map_err(serde::ser::Error::custom)?;
    serde_with::Bytes::serialize_as(&bytes, s)
}

pub fn ark_de<'de, D, A: CanonicalDeserialize>(data: D) -> Result<A, D::Error>
where
    D: serde::de::Deserializer<'de>,
{
    let s: Vec<u8> = serde_with::Bytes::deserialize_as(data)?;
    let a = A::deserialize_with_mode(s.as_slice(), Compress::Yes, Validate::Yes);
    a.map_err(serde::de::Error::custom)
}

#[cfg(test)]
mod tests {
    use crate::Bn254Field;
    use std::io::Cursor;

    use super::*;
    use test_log::test;

    fn test_polys() -> (Vec<(String, Vec<Bn254Field>)>, u64) {
        (
            vec![
                ("a".to_string(), (0..16).map(Bn254Field::from).collect()),
                ("b".to_string(), (-16..0).map(Bn254Field::from).collect()),
            ],
            16,
        )
    }

    #[test]
    fn write_read() {
        let mut buf: Vec<u8> = vec![];

        let (polys, degree) = test_polys();

        write_polys_stream(&mut buf, &polys).unwrap();
        let (read_polys, read_degree) = read_polys_file::<Bn254Field>(
            &mut Cursor::new(buf),
            &["a".to_string(), "b".to_string()],
        );

        assert_eq!(read_polys, polys);
        assert_eq!(read_degree, degree);
    }

    #[test]
    fn write_read_csv() {
        let polys = test_polys()
            .0
            .into_iter()
            .map(|(name, values)| (name.to_string(), values))
            .collect::<Vec<_>>();
        let polys_ref = polys.iter().collect::<Vec<_>>();

        for render_mode in &[
            CsvRenderMode::SignedBase10,
            CsvRenderMode::UnsignedBase10,
            CsvRenderMode::Hex,
        ] {
            let mut buf: Vec<u8> = vec![];
            write_polys_csv_file(&mut buf, *render_mode, &polys_ref);
            let read_polys = read_polys_csv_file::<Bn254Field>(&mut Cursor::new(buf));

            assert_eq!(read_polys, polys);
        }
    }
}
