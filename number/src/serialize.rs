use std::io::{Read, Write};

use crate::{BigInt, DegreeType, FieldElement};

fn ceil_div(num: usize, div: usize) -> usize {
    (num + div - 1) / div
}

pub fn write_polys_file<T: FieldElement>(
    file: &mut impl Write,
    degree: DegreeType,
    polys: &Vec<(&str, Vec<T>)>,
) {
    let width = ceil_div(T::modulus().to_arbitrary_integer().bits() as usize, 64) * 8;

    for i in 0..degree as usize {
        for (_name, constant) in polys {
            let bytes = constant[i].to_bytes_le();
            assert_eq!(bytes.len(), width);
            file.write_all(&bytes).unwrap();
        }
    }
}

pub fn read_polys_file<'a, T: FieldElement>(
    file: &mut impl Read,
    columns: &[&'a str],
) -> (Vec<(&'a str, Vec<T>)>, DegreeType) {
    let width = ceil_div(T::modulus().to_arbitrary_integer().bits() as usize, 64) * 8;

    let bytes_to_read = width * columns.len();

    let mut result: Vec<(_, Vec<T>)> = columns.iter().map(|name| (*name, vec![])).collect();
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

#[cfg(test)]
mod tests {

    use crate::Bn254Field;
    use std::io::Cursor;

    use super::*;

    #[test]
    fn write_read() {
        let mut buf: Vec<u8> = vec![];

        let degree = 4;
        let polys = vec![
            ("a", vec![Bn254Field::from(0); degree]),
            ("b", vec![Bn254Field::from(1); degree]),
        ];

        write_polys_file(&mut buf, degree as u64, &polys);
        let (read_polys, read_degree) =
            read_polys_file::<Bn254Field>(&mut Cursor::new(buf), &["a", "b"]);

        assert_eq!(read_polys, polys);
        assert_eq!(read_degree, degree as u64);
    }
}
