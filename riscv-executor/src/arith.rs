use powdr_number::FieldElement;

// TODO: implement
pub fn ec_double<F: FieldElement>(x: &[F], y: &[F]) -> ([F; 8], [F; 8]) {
    let x1: [F; 8] = [
        0x60297556u32.into(),
        0x2f057a14u32.into(),
        0x8568a18bu32.into(),
        0x82f6472fu32.into(),
        0x355235d3u32.into(),
        0x20453a14u32.into(),
        0x755eeea4u32.into(),
        0xfff97bd5u32.into(),
    ];
    let y1: [F; 8] = [
        0xb075f297u32.into(),
        0x3c870c36u32.into(),
        0x518fe4a0u32.into(),
        0xde80f0f6u32.into(),
        0x7f45c560u32.into(),
        0xf3be9601u32.into(),
        0xacfbb620u32.into(),
        0xae12777au32.into(),
    ];
    let x2: [F; 8] = [
        0x70afe85au32.into(),
        0xc5b0f470u32.into(),
        0x9620095bu32.into(),
        0x687cf441u32.into(),
        0x4d734633u32.into(),
        0x15c38f00u32.into(),
        0x48e7561bu32.into(),
        0xd01115d5u32.into(),
    ];
    let y2: [F; 8] = [
        0xf4062327u32.into(),
        0x6b051b13u32.into(),
        0xd9a86d52u32.into(),
        0x79238c5du32.into(),
        0xe17bd815u32.into(),
        0xa8b64537u32.into(),
        0xc815e0d7u32.into(),
        0xa9f34ffdu32.into(),
    ];

    assert_eq!(x.len(), 8);
    assert_eq!(y.len(), 8);

    if (x1, y1) == (x.try_into().unwrap(), y.try_into().unwrap()) {
        return (x2, y2);
    }

    unimplemented!()
}
