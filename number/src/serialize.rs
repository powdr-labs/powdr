use std::{
    fs::File,
    io::{self, BufWriter, Read, Write},
    path::Path,
};

use ark_serialize::{CanonicalDeserialize, CanonicalSerialize, Compress, Validate};
use csv::{Reader, Writer};
use serde::{de::DeserializeOwned, Serialize};
use serde_with::{DeserializeAs, SerializeAs};

use crate::FieldElement;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub enum CsvRenderMode {
    SignedBase10,
    UnsignedBase10,
    #[default]
    Hex,
}

const ROW_NAME: &str = "Row";

pub fn write_polys_csv_file<T: FieldElement>(
    file: impl Write,
    render_mode: CsvRenderMode,
    polys: &[(&String, &[T])],
) {
    let mut writer = Writer::from_writer(file);

    // Write headers, adding a "Row" column
    let mut headers = vec![ROW_NAME];
    headers.extend(polys.iter().map(|(name, _)| {
        assert!(*name != ROW_NAME);
        name.as_str()
    }));
    writer.write_record(&headers).unwrap();

    let max_len = polys.iter().map(|p| p.1.len()).max().unwrap();
    for row_index in 0..max_len {
        let mut row = Vec::new();
        row.push(format!("{row_index}"));
        for (_, values) in polys {
            let value = values
                .get(row_index)
                .map(|v| match render_mode {
                    CsvRenderMode::SignedBase10 => format!("{v}"),
                    CsvRenderMode::UnsignedBase10 => format!("{}", v.to_integer()),
                    CsvRenderMode::Hex => format!("0x{:x}", v.to_integer()),
                })
                .unwrap_or_default();
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
            // shorter polys/columns end in empty cells
            if value.trim().is_empty() {
                continue;
            }
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

pub fn buffered_write_file<R>(
    path: &Path,
    do_write: impl FnOnce(&mut BufWriter<File>) -> R,
) -> Result<R, io::Error> {
    let mut writer = BufWriter::new(File::create(path)?);
    let result = do_write(&mut writer);
    writer.flush()?;

    Ok(result)
}

pub trait ReadWrite {
    fn read(file: &mut impl Read) -> Self;
    fn write(&self, path: &Path) -> Result<(), serde_cbor::Error>;
}

impl<T: DeserializeOwned + Serialize> ReadWrite for T {
    fn read(file: &mut impl Read) -> Self {
        serde_cbor::from_reader(file).unwrap()
    }
    fn write(&self, path: &Path) -> Result<(), serde_cbor::Error> {
        buffered_write_file(path, |writer| serde_cbor::to_writer(writer, &self))??;
        Ok(())
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

    fn test_polys() -> Vec<(String, Vec<Bn254Field>)> {
        vec![
            ("a".to_string(), (0..16).map(Bn254Field::from).collect()),
            ("b".to_string(), (-16..0).map(Bn254Field::from).collect()),
        ]
    }

    #[test]
    fn write_read() {
        let mut buf: Vec<u8> = vec![];

        let polys = test_polys();

        serde_cbor::to_writer(&mut buf, &polys).unwrap();
        let read_polys: Vec<(String, Vec<Bn254Field>)> = ReadWrite::read(&mut Cursor::new(buf));

        assert_eq!(read_polys, polys);
    }

    #[test]
    fn write_read_csv() {
        let polys = test_polys()
            .into_iter()
            .map(|(name, values)| (name.to_string(), values))
            .collect::<Vec<_>>();
        let polys_ref = polys
            .iter()
            .map(|(name, values)| (name, values.as_slice()))
            .collect::<Vec<_>>();

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
