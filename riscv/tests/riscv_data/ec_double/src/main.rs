#![no_main]
#![no_std]

use hex_literal::hex;

use powdr_riscv_runtime::ec::double_u32_le as ec_double;
use powdr_riscv_runtime::ec::double_u8_be;

#[no_mangle]
pub fn main() {
    let x1 = [
        0x60297556, 0x2f057a14, 0x8568a18b, 0x82f6472f, 0x355235d3, 0x20453a14, 0x755eeea4,
        0xfff97bd5,
    ];
    let y1 = [
        0xb075f297, 0x3c870c36, 0x518fe4a0, 0xde80f0f6, 0x7f45c560, 0xf3be9601, 0xacfbb620,
        0xae12777a,
    ];
    let x2 = [
        0x70afe85a, 0xc5b0f470, 0x9620095b, 0x687cf441, 0x4d734633, 0x15c38f00, 0x48e7561b,
        0xd01115d5,
    ];
    let y2 = [
        0xf4062327, 0x6b051b13, 0xd9a86d52, 0x79238c5d, 0xe17bd815, 0xa8b64537, 0xc815e0d7,
        0xa9f34ffd,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    // same as above but using the big endian api
    let x1 = hex!("fff97bd5755eeea420453a14355235d382f6472f8568a18b2f057a1460297556");
    let y1 = hex!("ae12777aacfbb620f3be96017f45c560de80f0f6518fe4a03c870c36b075f297");
    let x2 = hex!("d01115d548e7561b15c38f004d734633687cf4419620095bc5b0f47070afe85a");
    let y2 = hex!("a9f34ffdc815e0d7a8b64537e17bd81579238c5dd9a86d526b051b13f4062327");
    assert_eq!(double_u8_be(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x70afe85a, 0xc5b0f470, 0x9620095b, 0x687cf441, 0x4d734633, 0x15c38f00, 0x48e7561b,
        0xd01115d5,
    ];
    let y1 = [
        0xf4062327, 0x6b051b13, 0xd9a86d52, 0x79238c5d, 0xe17bd815, 0xa8b64537, 0xc815e0d7,
        0xa9f34ffd,
    ];
    let x2 = [
        0xb202e6ce, 0x502bda8, 0x9d62b794, 0x68321543, 0x61ba8b09, 0x8ac09c91, 0x413d33d4,
        0xfe72c435,
    ];
    let y2 = [
        0xcf58c5bf, 0x978ed2fb, 0x6b4a9d22, 0x1dc88e3, 0x9d729981, 0xd3ab47e0, 0x7ff24a68,
        0x6851de06,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xb202e6ce, 0x502bda8, 0x9d62b794, 0x68321543, 0x61ba8b09, 0x8ac09c91, 0x413d33d4,
        0xfe72c435,
    ];
    let y1 = [
        0xcf58c5bf, 0x978ed2fb, 0x6b4a9d22, 0x1dc88e3, 0x9d729981, 0xd3ab47e0, 0x7ff24a68,
        0x6851de06,
    ];
    let x2 = [
        0x1118e5c3, 0x9bd870aa, 0x452bebc1, 0xfc579b27, 0xf4e65b4b, 0xb441656e, 0x9645307d,
        0x6eca335d,
    ];
    let y2 = [
        0x5a08668, 0x498a2f78, 0x3bf8ec34, 0x3a496a3a, 0x74b875a0, 0x592f5790, 0x7a7a0710,
        0xd50123b5,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x1118e5c3, 0x9bd870aa, 0x452bebc1, 0xfc579b27, 0xf4e65b4b, 0xb441656e, 0x9645307d,
        0x6eca335d,
    ];
    let y1 = [
        0x5a08668, 0x498a2f78, 0x3bf8ec34, 0x3a496a3a, 0x74b875a0, 0x592f5790, 0x7a7a0710,
        0xd50123b5,
    ];
    let x2 = [
        0x7f8cb0e3, 0x43933aca, 0xe1efe3a4, 0xa22eb53f, 0x4b2eb72e, 0x8fa64e04, 0x74456d8f,
        0x3f0e80e5,
    ];
    let y2 = [
        0xea5f404f, 0xcb0289e2, 0xa65b53a4, 0x9501253a, 0x485d01b3, 0xe90b9c08, 0x296cbc91,
        0xcb66d7d7,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x7f8cb0e3, 0x43933aca, 0xe1efe3a4, 0xa22eb53f, 0x4b2eb72e, 0x8fa64e04, 0x74456d8f,
        0x3f0e80e5,
    ];
    let y1 = [
        0xea5f404f, 0xcb0289e2, 0xa65b53a4, 0x9501253a, 0x485d01b3, 0xe90b9c08, 0x296cbc91,
        0xcb66d7d7,
    ];
    let x2 = [
        0x33ce1752, 0xc7b750f7, 0xd7cd204e, 0xe783c797, 0xd99c9aea, 0x812ddf64, 0xd01dc635,
        0xd7a0da58,
    ];
    let y2 = [
        0x762cef4, 0xbbc02738, 0xc062b742, 0xbe040a8, 0x40e28465, 0xf6f29283, 0x68008032,
        0x912770e0,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x33ce1752, 0xc7b750f7, 0xd7cd204e, 0xe783c797, 0xd99c9aea, 0x812ddf64, 0xd01dc635,
        0xd7a0da58,
    ];
    let y1 = [
        0x762cef4, 0xbbc02738, 0xc062b742, 0xbe040a8, 0x40e28465, 0xf6f29283, 0x68008032,
        0x912770e0,
    ];
    let x2 = [
        0xb5476085, 0xa908b701, 0x96eb9f84, 0xb5714e77, 0xa78ed1af, 0x10d3aad6, 0x7a08cd3e,
        0x3443a706,
    ];
    let y2 = [
        0x8b8f52d8, 0x6d3484bd, 0xd0c2b67f, 0x18a4b27, 0x8c7e1da9, 0x4f6e8c4b, 0x829b6f85,
        0x661a7a5f,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xb5476085, 0xa908b701, 0x96eb9f84, 0xb5714e77, 0xa78ed1af, 0x10d3aad6, 0x7a08cd3e,
        0x3443a706,
    ];
    let y1 = [
        0x8b8f52d8, 0x6d3484bd, 0xd0c2b67f, 0x18a4b27, 0x8c7e1da9, 0x4f6e8c4b, 0x829b6f85,
        0x661a7a5f,
    ];
    let x2 = [
        0xe57e8dfa, 0xfcfc0cb9, 0xa3c7e184, 0x9809191, 0xaca98ca0, 0xd9a30f8, 0xf0799c4c,
        0x8262cf2f,
    ];
    let y2 = [
        0xfbac376a, 0x35cff8d8, 0x2b14c478, 0x57b6ed33, 0xc5b34f34, 0x66fee22e, 0x9109e4e,
        0x83fd95e2,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xe57e8dfa, 0xfcfc0cb9, 0xa3c7e184, 0x9809191, 0xaca98ca0, 0xd9a30f8, 0xf0799c4c,
        0x8262cf2f,
    ];
    let y1 = [
        0xfbac376a, 0x35cff8d8, 0x2b14c478, 0x57b6ed33, 0xc5b34f34, 0x66fee22e, 0x9109e4e,
        0x83fd95e2,
    ];
    let x2 = [
        0x7c70620c, 0xd17cc1f2, 0xabc288d9, 0x4998c4be, 0x2b671780, 0xc60dd31a, 0x8d2c236d,
        0x1653a8a4,
    ];
    let y2 = [
        0x315b32cd, 0x6ca2e81d, 0xdfd3dc52, 0x12af748, 0x4efa701c, 0xeafa9947, 0x35af7f7a,
        0x3382909,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x7c70620c, 0xd17cc1f2, 0xabc288d9, 0x4998c4be, 0x2b671780, 0xc60dd31a, 0x8d2c236d,
        0x1653a8a4,
    ];
    let y1 = [
        0x315b32cd, 0x6ca2e81d, 0xdfd3dc52, 0x12af748, 0x4efa701c, 0xeafa9947, 0x35af7f7a,
        0x3382909,
    ];
    let x2 = [
        0xe71dabcd, 0x47d42ba6, 0x89e5cb4f, 0x54d3fe49, 0x60b5373f, 0x6098ae32, 0x6b63f43c,
        0xd49ee4fb,
    ];
    let y2 = [
        0x16603c2, 0xe66a90cf, 0x12ff7031, 0x129c5093, 0xa61bf356, 0xd7c87ea7, 0x9a5490d, 0x531e392,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xe71dabcd, 0x47d42ba6, 0x89e5cb4f, 0x54d3fe49, 0x60b5373f, 0x6098ae32, 0x6b63f43c,
        0xd49ee4fb,
    ];
    let y1 = [
        0x16603c2, 0xe66a90cf, 0x12ff7031, 0x129c5093, 0xa61bf356, 0xd7c87ea7, 0x9a5490d, 0x531e392,
    ];
    let x2 = [
        0xc8c828a, 0x53f30ab9, 0xc96ae41f, 0x132eb242, 0x17e81c75, 0xe44a0d8, 0xa4149e75,
        0x5f94851c,
    ];
    let y2 = [
        0x37344d80, 0xbfeb0a3f, 0x4fc68b04, 0x8c66df75, 0x8882f35e, 0xe5f0797d, 0xafa1fee8,
        0x26b8c3b8,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xc8c828a, 0x53f30ab9, 0xc96ae41f, 0x132eb242, 0x17e81c75, 0xe44a0d8, 0xa4149e75,
        0x5f94851c,
    ];
    let y1 = [
        0x37344d80, 0xbfeb0a3f, 0x4fc68b04, 0x8c66df75, 0x8882f35e, 0xe5f0797d, 0xafa1fee8,
        0x26b8c3b8,
    ];
    let x2 = [
        0xc5041216, 0x65b7f8f1, 0x842b836a, 0x3f7335f6, 0xdc2fed52, 0x128b59ef, 0x21f7acf4,
        0xda75317b,
    ];
    let y2 = [
        0x6e708572, 0xdaed3298, 0xe77aceda, 0xe9aac07a, 0x342d7fc6, 0xdf19e21b, 0xbf72d5f0,
        0x73f8a046,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xc5041216, 0x65b7f8f1, 0x842b836a, 0x3f7335f6, 0xdc2fed52, 0x128b59ef, 0x21f7acf4,
        0xda75317b,
    ];
    let y1 = [
        0x6e708572, 0xdaed3298, 0xe77aceda, 0xe9aac07a, 0x342d7fc6, 0xdf19e21b, 0xbf72d5f0,
        0x73f8a046,
    ];
    let x2 = [
        0x3c62bac0, 0x9505324f, 0x51f0ab06, 0x19150ddf, 0xc3e8b70e, 0x1364b7d2, 0x23f469c,
        0x9530f0f9,
    ];
    let y2 = [
        0x7618e309, 0x478abda9, 0x2f1fdc68, 0xe25b3285, 0x59b333e0, 0x34dd2f7f, 0x8f9f21e2,
        0x8f3c305a,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x3c62bac0, 0x9505324f, 0x51f0ab06, 0x19150ddf, 0xc3e8b70e, 0x1364b7d2, 0x23f469c,
        0x9530f0f9,
    ];
    let y1 = [
        0x7618e309, 0x478abda9, 0x2f1fdc68, 0xe25b3285, 0x59b333e0, 0x34dd2f7f, 0x8f9f21e2,
        0x8f3c305a,
    ];
    let x2 = [
        0xdc3c9c8f, 0x6704385, 0x3e4367b2, 0xf2816fee, 0xaaa332b0, 0x6f09ff43, 0xbe4298fd,
        0x67be02dc,
    ];
    let y2 = [
        0x593652d9, 0x55384998, 0xb88c2be, 0xcd993bf6, 0x8291693, 0xa2c945b6, 0x3e4def84,
        0x7a9b55a7,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xdc3c9c8f, 0x6704385, 0x3e4367b2, 0xf2816fee, 0xaaa332b0, 0x6f09ff43, 0xbe4298fd,
        0x67be02dc,
    ];
    let y1 = [
        0x593652d9, 0x55384998, 0xb88c2be, 0xcd993bf6, 0x8291693, 0xa2c945b6, 0x3e4def84,
        0x7a9b55a7,
    ];
    let x2 = [
        0x10aaa33a, 0x11f9bcbe, 0xc17b9ca5, 0x8c92dd29, 0xbc571836, 0xdf569013, 0xf4ef876a,
        0x893b2492,
    ];
    let y2 = [
        0xd1af3445, 0x67b80b8a, 0x13ceeb42, 0xa439e8a2, 0x66507f32, 0xf413a007, 0x72d1c89e,
        0xcdb152b6,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x10aaa33a, 0x11f9bcbe, 0xc17b9ca5, 0x8c92dd29, 0xbc571836, 0xdf569013, 0xf4ef876a,
        0x893b2492,
    ];
    let y1 = [
        0xd1af3445, 0x67b80b8a, 0x13ceeb42, 0xa439e8a2, 0x66507f32, 0xf413a007, 0x72d1c89e,
        0xcdb152b6,
    ];
    let x2 = [
        0xf6e55dc8, 0x4b891216, 0xeaca0439, 0x6ff95ab6, 0xc0509442, 0xba84a440, 0x90c5ffb2,
        0x44314047,
    ];
    let y2 = [
        0xdbe323b3, 0x31d944ae, 0x9eaa2e50, 0xa66a29b7, 0x5642fed7, 0xfe99837f, 0xe65366f8,
        0x96b0c142,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xf6e55dc8, 0x4b891216, 0xeaca0439, 0x6ff95ab6, 0xc0509442, 0xba84a440, 0x90c5ffb2,
        0x44314047,
    ];
    let y1 = [
        0xdbe323b3, 0x31d944ae, 0x9eaa2e50, 0xa66a29b7, 0x5642fed7, 0xfe99837f, 0xe65366f8,
        0x96b0c142,
    ];
    let x2 = [
        0x33f0e9aa, 0x3eb5e196, 0xb11bd34b, 0x68112776, 0xd58138d2, 0xb7924ae0, 0x575f26ad,
        0xe5380fe8,
    ];
    let y2 = [
        0x4082720f, 0xc4ba4136, 0xf468318e, 0x6fb94e5d, 0x924c8e01, 0x5b691363, 0x9087b41d,
        0xb97fd873,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x33f0e9aa, 0x3eb5e196, 0xb11bd34b, 0x68112776, 0xd58138d2, 0xb7924ae0, 0x575f26ad,
        0xe5380fe8,
    ];
    let y1 = [
        0x4082720f, 0xc4ba4136, 0xf468318e, 0x6fb94e5d, 0x924c8e01, 0x5b691363, 0x9087b41d,
        0xb97fd873,
    ];
    let x2 = [
        0xeebc61d6, 0x1aed361b, 0xd9ff42de, 0x8a8fd3a7, 0x5d6b1f51, 0xc395f0d1, 0xa3ed9af0,
        0x939ff3e4,
    ];
    let y2 = [
        0xa3f5cb70, 0xe75ea466, 0xb78c7f82, 0x980bf26e, 0xef016c04, 0x9d46fc4e, 0x8b7a90e,
        0xdeab3bcf,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xeebc61d6, 0x1aed361b, 0xd9ff42de, 0x8a8fd3a7, 0x5d6b1f51, 0xc395f0d1, 0xa3ed9af0,
        0x939ff3e4,
    ];
    let y1 = [
        0xa3f5cb70, 0xe75ea466, 0xb78c7f82, 0x980bf26e, 0xef016c04, 0x9d46fc4e, 0x8b7a90e,
        0xdeab3bcf,
    ];
    let x2 = [
        0xc497e0df, 0x16e134d, 0xecf76f53, 0x4c3bb436, 0xfe6029a0, 0x7858785, 0xae383293,
        0xfdc63e52,
    ];
    let y2 = [
        0xdb9eb19f, 0xf0604449, 0xbf35d9d5, 0x7bbeb22f, 0x8ae2e8b8, 0xe3df7142, 0xacebbb52,
        0x292dad67,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xc497e0df, 0x16e134d, 0xecf76f53, 0x4c3bb436, 0xfe6029a0, 0x7858785, 0xae383293,
        0xfdc63e52,
    ];
    let y1 = [
        0xdb9eb19f, 0xf0604449, 0xbf35d9d5, 0x7bbeb22f, 0x8ae2e8b8, 0xe3df7142, 0xacebbb52,
        0x292dad67,
    ];
    let x2 = [
        0xf55812dd, 0xa0a2a582, 0x552d30e2, 0x3d446723, 0xc058f78e, 0xb6abed6, 0x92ff352f,
        0x7029bd7a,
    ];
    let y2 = [
        0x1a2d2927, 0x721cc66b, 0x43b2c73c, 0x47dae842, 0xe30683ac, 0x7dd6544a, 0xfde8b3d2,
        0xb0eefada,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xf55812dd, 0xa0a2a582, 0x552d30e2, 0x3d446723, 0xc058f78e, 0xb6abed6, 0x92ff352f,
        0x7029bd7a,
    ];
    let y1 = [
        0x1a2d2927, 0x721cc66b, 0x43b2c73c, 0x47dae842, 0xe30683ac, 0x7dd6544a, 0xfde8b3d2,
        0xb0eefada,
    ];
    let x2 = [
        0xb181fdc2, 0xdcdabff9, 0x5cc62364, 0xdd2f62bb, 0x18a34e7e, 0x4aa264b8, 0xf47e6e47,
        0xf42c102a,
    ];
    let y2 = [
        0xa485d7fd, 0x81f00093, 0x9a2acf26, 0x4c15502d, 0xb86fe22a, 0x78fad05c, 0x6cfe806c,
        0x57503ab4,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xb181fdc2, 0xdcdabff9, 0x5cc62364, 0xdd2f62bb, 0x18a34e7e, 0x4aa264b8, 0xf47e6e47,
        0xf42c102a,
    ];
    let y1 = [
        0xa485d7fd, 0x81f00093, 0x9a2acf26, 0x4c15502d, 0xb86fe22a, 0x78fad05c, 0x6cfe806c,
        0x57503ab4,
    ];
    let x2 = [
        0xeedd7dd6, 0x3866d47d, 0x65e1968c, 0x49376fe2, 0xee7cfdec, 0xca5a7840, 0x24c7524b,
        0x32cfcf6a,
    ];
    let y2 = [
        0xfe08e330, 0x25fd44ae, 0x349a08b, 0x7a0d8cd2, 0x409f561e, 0x6208096a, 0x976a7748,
        0x21846a34,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xeedd7dd6, 0x3866d47d, 0x65e1968c, 0x49376fe2, 0xee7cfdec, 0xca5a7840, 0x24c7524b,
        0x32cfcf6a,
    ];
    let y1 = [
        0xfe08e330, 0x25fd44ae, 0x349a08b, 0x7a0d8cd2, 0x409f561e, 0x6208096a, 0x976a7748,
        0x21846a34,
    ];
    let x2 = [
        0x21231d11, 0xce674831, 0x3c2aaad7, 0x22ab36c6, 0xc777c398, 0x33d1155c, 0x8b9388e4,
        0x3514d41e,
    ];
    let y2 = [
        0xe3855df5, 0x53d6fb40, 0xaf79ebe, 0x9384f31d, 0x56839eff, 0xef44d11e, 0x16017eb8,
        0x89a83250,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x21231d11, 0xce674831, 0x3c2aaad7, 0x22ab36c6, 0xc777c398, 0x33d1155c, 0x8b9388e4,
        0x3514d41e,
    ];
    let y1 = [
        0xe3855df5, 0x53d6fb40, 0xaf79ebe, 0x9384f31d, 0x56839eff, 0xef44d11e, 0x16017eb8,
        0x89a83250,
    ];
    let x2 = [
        0x80633cb1, 0x2567e09e, 0x69d02113, 0x575a224b, 0x12181fcb, 0xc62732, 0x17aacad4,
        0x6dde9cf3,
    ];
    let y2 = [
        0x67ce6b34, 0x57dd49aa, 0xcf859ef3, 0x80b27fda, 0xa1ba66a8, 0x5c99ef86, 0xa707e41d,
        0x9188fbe7,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x80633cb1, 0x2567e09e, 0x69d02113, 0x575a224b, 0x12181fcb, 0xc62732, 0x17aacad4,
        0x6dde9cf3,
    ];
    let y1 = [
        0x67ce6b34, 0x57dd49aa, 0xcf859ef3, 0x80b27fda, 0xa1ba66a8, 0x5c99ef86, 0xa707e41d,
        0x9188fbe7,
    ];
    let x2 = [
        0x44e5467d, 0x4d0bd76a, 0x19bbface, 0x40908ab8, 0xec970e9, 0x2c21f62e, 0xfc69a122,
        0x97d064f0,
    ];
    let y2 = [
        0x1e9cb3fa, 0x797300fd, 0x54f17ccd, 0xda5fb3b8, 0xa850861f, 0x3f7c66f, 0xd33402cc,
        0x89974f2e,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x44e5467d, 0x4d0bd76a, 0x19bbface, 0x40908ab8, 0xec970e9, 0x2c21f62e, 0xfc69a122,
        0x97d064f0,
    ];
    let y1 = [
        0x1e9cb3fa, 0x797300fd, 0x54f17ccd, 0xda5fb3b8, 0xa850861f, 0x3f7c66f, 0xd33402cc,
        0x89974f2e,
    ];
    let x2 = [
        0x13613bec, 0xcca81cb9, 0x101cfe67, 0x8bb5fc9d, 0xc74f972a, 0xedf1b33d, 0xc93937bd,
        0x2dcfcab8,
    ];
    let y2 = [
        0x9a039215, 0x3e730924, 0xd33f5f38, 0x3732cfba, 0xd6f6c6f4, 0x65f088b7, 0x9474a412,
        0x46dbc4dd,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x13613bec, 0xcca81cb9, 0x101cfe67, 0x8bb5fc9d, 0xc74f972a, 0xedf1b33d, 0xc93937bd,
        0x2dcfcab8,
    ];
    let y1 = [
        0x9a039215, 0x3e730924, 0xd33f5f38, 0x3732cfba, 0xd6f6c6f4, 0x65f088b7, 0x9474a412,
        0x46dbc4dd,
    ];
    let x2 = [
        0x47fb9e1a, 0x17cd1708, 0xde2a3296, 0x7fe74b74, 0xbbab0e76, 0xf1a02bc9, 0xa48ec5a8,
        0x1bec414a,
    ];
    let y2 = [
        0x749c0443, 0x57f6e117, 0xe8c9796e, 0x681385da, 0x30c54b0f, 0x8a79bc57, 0x70126667,
        0xe3586704,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x47fb9e1a, 0x17cd1708, 0xde2a3296, 0x7fe74b74, 0xbbab0e76, 0xf1a02bc9, 0xa48ec5a8,
        0x1bec414a,
    ];
    let y1 = [
        0x749c0443, 0x57f6e117, 0xe8c9796e, 0x681385da, 0x30c54b0f, 0x8a79bc57, 0x70126667,
        0xe3586704,
    ];
    let x2 = [
        0xbb7ceceb, 0xf3f678ff, 0x8897faf0, 0x73a59f93, 0x6f6e6814, 0x36ffb812, 0x4276d450,
        0x437a8620,
    ];
    let y2 = [
        0x56c181e1, 0x7363bcc3, 0xdc8f9782, 0x87220fcf, 0x99d297ff, 0x69b8feb6, 0x3eeac32f,
        0xb916ba1,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xbb7ceceb, 0xf3f678ff, 0x8897faf0, 0x73a59f93, 0x6f6e6814, 0x36ffb812, 0x4276d450,
        0x437a8620,
    ];
    let y1 = [
        0x56c181e1, 0x7363bcc3, 0xdc8f9782, 0x87220fcf, 0x99d297ff, 0x69b8feb6, 0x3eeac32f,
        0xb916ba1,
    ];
    let x2 = [
        0xdcbf00eb, 0x4c9d9d87, 0xc18d0227, 0x41b4e98b, 0xa1a30bc2, 0x49be16f6, 0x96ead4dc,
        0xb89070ae,
    ];
    let y2 = [
        0x1b0e664e, 0x1b7f1bcd, 0xb6b96a67, 0xcb0d8b06, 0xc1c4a766, 0x472294e4, 0xc8a2d88f,
        0x6f24c8c2,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xdcbf00eb, 0x4c9d9d87, 0xc18d0227, 0x41b4e98b, 0xa1a30bc2, 0x49be16f6, 0x96ead4dc,
        0xb89070ae,
    ];
    let y1 = [
        0x1b0e664e, 0x1b7f1bcd, 0xb6b96a67, 0xcb0d8b06, 0xc1c4a766, 0x472294e4, 0xc8a2d88f,
        0x6f24c8c2,
    ];
    let x2 = [
        0xb6fbe7b2, 0xb9d6ff9a, 0x458d65a3, 0x5eadedc1, 0xb2a88460, 0xf336bbb1, 0x9cb441f8,
        0x26488766,
    ];
    let y2 = [
        0x21bc2a34, 0x932a78bc, 0x6a0eb603, 0x5638d981, 0xd02ddf18, 0x8f2f2dca, 0xb2014498,
        0x9e15dab4,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xb6fbe7b2, 0xb9d6ff9a, 0x458d65a3, 0x5eadedc1, 0xb2a88460, 0xf336bbb1, 0x9cb441f8,
        0x26488766,
    ];
    let y1 = [
        0x21bc2a34, 0x932a78bc, 0x6a0eb603, 0x5638d981, 0xd02ddf18, 0x8f2f2dca, 0xb2014498,
        0x9e15dab4,
    ];
    let x2 = [
        0x2b038315, 0x9690d306, 0x69310e6f, 0x9cacc433, 0x9794b862, 0x1e4680e3, 0x56771222,
        0xaba55687,
    ];
    let y2 = [
        0xae25fc0a, 0xf9a003f9, 0xd8b63338, 0x3fbfb532, 0x25130d6f, 0x63d570f6, 0xaa365edb,
        0xa0e75d87,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x2b038315, 0x9690d306, 0x69310e6f, 0x9cacc433, 0x9794b862, 0x1e4680e3, 0x56771222,
        0xaba55687,
    ];
    let y1 = [
        0xae25fc0a, 0xf9a003f9, 0xd8b63338, 0x3fbfb532, 0x25130d6f, 0x63d570f6, 0xaa365edb,
        0xa0e75d87,
    ];
    let x2 = [
        0x95bc15b4, 0x9cb9a134, 0x465a2ee6, 0x9275028e, 0xced7ca8d, 0xed858ee9, 0x51eeadc9,
        0x10e90e2e,
    ];
    let y2 = [
        0x58aa258d, 0x34ebe609, 0x2bb6a88, 0x4ca58963, 0x16ad1f75, 0x4d57a8c6, 0x80d5e042,
        0xc68a3703,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x95bc15b4, 0x9cb9a134, 0x465a2ee6, 0x9275028e, 0xced7ca8d, 0xed858ee9, 0x51eeadc9,
        0x10e90e2e,
    ];
    let y1 = [
        0x58aa258d, 0x34ebe609, 0x2bb6a88, 0x4ca58963, 0x16ad1f75, 0x4d57a8c6, 0x80d5e042,
        0xc68a3703,
    ];
    let x2 = [
        0x7a1c0a80, 0xf62abc8, 0xc65a9c74, 0x4d625158, 0x2ff9c3, 0xb17c9be7, 0xa614cca5, 0xb6b15a68,
    ];
    let y2 = [
        0x41ce0a03, 0xb6cd0110, 0x82e16ee, 0x9c9a12b3, 0xef6536d4, 0xa54e223e, 0xd6cdb61e,
        0xfae62e14,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x7a1c0a80, 0xf62abc8, 0xc65a9c74, 0x4d625158, 0x2ff9c3, 0xb17c9be7, 0xa614cca5, 0xb6b15a68,
    ];
    let y1 = [
        0x41ce0a03, 0xb6cd0110, 0x82e16ee, 0x9c9a12b3, 0xef6536d4, 0xa54e223e, 0xd6cdb61e,
        0xfae62e14,
    ];
    let x2 = [
        0x92b062d4, 0xa7caa50a, 0x9bb6a141, 0x7a5ce7e5, 0x83ea227a, 0x6fb1712, 0x3256eaca,
        0x35963ea4,
    ];
    let y2 = [
        0xbbb25302, 0xa10aa4d1, 0x64de59b1, 0xd04082b9, 0xf9c08a96, 0xbfcce196, 0x4951e5c9,
        0xf65be145,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x92b062d4, 0xa7caa50a, 0x9bb6a141, 0x7a5ce7e5, 0x83ea227a, 0x6fb1712, 0x3256eaca,
        0x35963ea4,
    ];
    let y1 = [
        0xbbb25302, 0xa10aa4d1, 0x64de59b1, 0xd04082b9, 0xf9c08a96, 0xbfcce196, 0x4951e5c9,
        0xf65be145,
    ];
    let x2 = [
        0x1d33fd27, 0xfa0bf5c5, 0xb646cc62, 0x445f573d, 0xda82361b, 0xd022388e, 0x2263e84c,
        0x9ed73f09,
    ];
    let y2 = [
        0x2716c458, 0x5972b2de, 0xb2e44934, 0x94a823e5, 0x42467254, 0xee75b4f3, 0xebb1eeea,
        0xb6318967,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x1d33fd27, 0xfa0bf5c5, 0xb646cc62, 0x445f573d, 0xda82361b, 0xd022388e, 0x2263e84c,
        0x9ed73f09,
    ];
    let y1 = [
        0x2716c458, 0x5972b2de, 0xb2e44934, 0x94a823e5, 0x42467254, 0xee75b4f3, 0xebb1eeea,
        0xb6318967,
    ];
    let x2 = [
        0xef028d83, 0x579623ae, 0xba743961, 0x6195926d, 0x15de69db, 0x6a5abe5a, 0xe3c785ec,
        0xa7ebf7c4,
    ];
    let y2 = [
        0x99d0bed1, 0x9640392b, 0x4b053919, 0x47a38927, 0x7044804b, 0xcfd9c737, 0xbfe362d5,
        0x6205152f,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xef028d83, 0x579623ae, 0xba743961, 0x6195926d, 0x15de69db, 0x6a5abe5a, 0xe3c785ec,
        0xa7ebf7c4,
    ];
    let y1 = [
        0x99d0bed1, 0x9640392b, 0x4b053919, 0x47a38927, 0x7044804b, 0xcfd9c737, 0xbfe362d5,
        0x6205152f,
    ];
    let x2 = [
        0x7bb61ee5, 0xf2884413, 0xfb1f0c13, 0xda4f04e2, 0x8974ae6e, 0x662638cd, 0xcc8721b8,
        0xd4933230,
    ];
    let y2 = [
        0xe5d694a8, 0x662da4d0, 0x5a438ddc, 0x1ad12c8c, 0x1ecafb5e, 0xedcc5e9d, 0xf51a9d23,
        0x21c09ab,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x7bb61ee5, 0xf2884413, 0xfb1f0c13, 0xda4f04e2, 0x8974ae6e, 0x662638cd, 0xcc8721b8,
        0xd4933230,
    ];
    let y1 = [
        0xe5d694a8, 0x662da4d0, 0x5a438ddc, 0x1ad12c8c, 0x1ecafb5e, 0xedcc5e9d, 0xf51a9d23,
        0x21c09ab,
    ];
    let x2 = [
        0xec04554a, 0x530ddcbc, 0x4688cffe, 0xaadcffbb, 0x7a10a2ec, 0x474652c2, 0x9873d1a0,
        0x896f37c8,
    ];
    let y2 = [
        0x929138df, 0xd68f9fe1, 0xacc417dc, 0xe6085b61, 0x4e811bf1, 0xda622bb0, 0x224ac4ac,
        0x380423e7,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xec04554a, 0x530ddcbc, 0x4688cffe, 0xaadcffbb, 0x7a10a2ec, 0x474652c2, 0x9873d1a0,
        0x896f37c8,
    ];
    let y1 = [
        0x929138df, 0xd68f9fe1, 0xacc417dc, 0xe6085b61, 0x4e811bf1, 0xda622bb0, 0x224ac4ac,
        0x380423e7,
    ];
    let x2 = [
        0xb43fca26, 0x84077d1a, 0xa3bc2367, 0x7dfb841d, 0xbf3578a2, 0xca6c209d, 0x774b6d6c,
        0x11b3b97f,
    ];
    let y2 = [
        0x5b679d58, 0xd3b27eaf, 0x4b9f9d42, 0x3bae231c, 0x2f36d3bb, 0x8cd5650c, 0xae600c50,
        0x65331f9f,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0xb43fca26, 0x84077d1a, 0xa3bc2367, 0x7dfb841d, 0xbf3578a2, 0xca6c209d, 0x774b6d6c,
        0x11b3b97f,
    ];
    let y1 = [
        0x5b679d58, 0xd3b27eaf, 0x4b9f9d42, 0x3bae231c, 0x2f36d3bb, 0x8cd5650c, 0xae600c50,
        0x65331f9f,
    ];
    let x2 = [
        0x48dfd587, 0x79361bb, 0xc9b02656, 0x5ec4ba38, 0x2cf5a12d, 0x34867aaa, 0xacf4508b,
        0x5084b41b,
    ];
    let y2 = [
        0x91470e89, 0x6e79e97f, 0x6891f560, 0x5db6f560, 0x55292747, 0x619aa6c8, 0x1d980d31,
        0x34a9631a,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));

    ///////////////////////////////////////////////////////////////

    let x1 = [
        0x48dfd587, 0x79361bb, 0xc9b02656, 0x5ec4ba38, 0x2cf5a12d, 0x34867aaa, 0xacf4508b,
        0x5084b41b,
    ];
    let y1 = [
        0x91470e89, 0x6e79e97f, 0x6891f560, 0x5db6f560, 0x55292747, 0x619aa6c8, 0x1d980d31,
        0x34a9631a,
    ];
    let x2 = [
        0x6c953fa9, 0x4d05956d, 0xf0b8c3db, 0x28ab2629, 0x4bd18c06, 0x3a5f485d, 0xaaab9323,
        0xa49ed10e,
    ];
    let y2 = [
        0x46fb4c72, 0x67b2bd22, 0x968e181b, 0x5ae87534, 0xa0dfddfb, 0xe03476c0, 0x660f5398,
        0xcc72b894,
    ];
    assert_eq!(ec_double(x1, y1), (x2, y2));
}
