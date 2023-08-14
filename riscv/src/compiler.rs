use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use asm_utils::{
    ast::{BinaryOpKind, UnaryOpKind},
    data_parser::{self, DataValue},
    parser::parse_asm,
    reachability,
};
use itertools::Itertools;

use crate::disambiguator;
use crate::parser::RiscParser;
use crate::{Argument, Expression, Statement};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Register {
    value: u8,
}

pub fn machine_decls() -> Vec<&'static str> {
    vec![
        r#"
// ================= binary/bitwise instructions =================

machine Binary(latch, function_id) {

    degree 262144;

    function and<0> A, B -> C {
    }

    function or<1> A, B -> C {
    }

    function xor<2> A, B -> C {
    }

    constraints{
        macro is_nonzero(X) { match X { 0 => 0, _ => 1, } };
        macro is_zero(X) { 1 - is_nonzero(X) };

        col fixed latch(i) { is_zero((i % 4) - 3) };
        col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

        col fixed P_A(i) { i % 256 };
        col fixed P_B(i) { (i >> 8) % 256 };
        col fixed P_operation(i) { (i / (256 * 256)) % 3 };
        col fixed P_C(i) {
            match P_operation(i) {
                0 => P_A(i) & P_B(i),
                1 => P_A(i) | P_B(i),
                2 => P_A(i) ^ P_B(i),
            } & 0xff
        };

        col witness A_byte;
        col witness B_byte;
        col witness C_byte;

        col witness A;
        col witness B;
        col witness C;

        A' = A * (1 - latch) + A_byte * FACTOR;
        B' = B * (1 - latch) + B_byte * FACTOR;
        C' = C * (1 - latch) + C_byte * FACTOR;

        {function_id', A_byte, B_byte, C_byte} in {P_operation, P_A, P_B, P_C};
    }
}
"#,
        r#"
// ================= shift instructions =================

machine Shift(latch, function_id) {
    degree 262144;

    function shl<0> A, B -> C {
    }

    function shr<1> A, B -> C {
    }

    constraints{
        col fixed latch(i) { is_zero((i % 4) - 3) };
        col fixed FACTOR_ROW(i) { (i + 1) % 4 };
        col fixed FACTOR(i) { 1 << (((i + 1) % 4) * 8) };

        col fixed P_A(i) { i % 256 };
        col fixed P_B(i) { (i / 256) % 32 };
        col fixed P_ROW(i) { (i / (256 * 32)) % 4 };
        col fixed P_operation(i) { (i / (256 * 32 * 4)) % 2 };
        col fixed P_C(i) {
            match P_operation(i) {
                0 => (P_A(i) << (P_B(i) + (P_ROW(i) * 8))),
                1 => (P_A(i) << (P_ROW(i) * 8)) >> P_B(i),
            } & 0xffffffff
        };

        col witness A_byte;
        col witness C_part;

        col witness A;
        col witness B;
        col witness C;

        A' = A * (1 - latch) + A_byte * FACTOR;
        (B' - B) * (1 - latch) = 0;
        C' = C * (1 - latch) + C_part;

        // TODO this way, we cannot prove anything that shifts by more than 31 bits.
        {function_id', A_byte, B', FACTOR_ROW, C_part} in {P_operation, P_A, P_B, P_ROW, P_C};
    }
}
"#,
r#"
// Implements the poseidon permutation for the BN254 curve.
// Note that this relies on the trace table being non-wrapping, so it will
// only work with the Halo2 backend (which is the only backend that supports
// the BN254 curve).
machine PoseidonBN254(LASTBLOCK, function_id) {

    // Hashes two "rate" elements and one "capacity" element to one field element
    // by applying the Poseidon permutation and returning the first rate element.
    // When the hash function is used only once, the capacity element should be
    // set to a constant, where different constants can be used to define different
    // hash functions.
    function poseidon_permutation<0> input_in0, input_in1, input_cap -> in0 {
    }


    constraints {
        // Using parameters from https://eprint.iacr.org/2019/458.pdf
        // See https://extgit.iaik.tugraz.at/krypto/hadeshash/-/blob/master/code/poseidonperm_x5_254_3.sage
        
        // The PIL is heavily inspired by Polygon's Poseidon PIL:
        // https://github.com/0xPolygonHermez/zkevm-proverjs/blob/main/pil/poseidong.pil

        // Number of full rounds
        constant %nRoundsF = 8;
        // Number of partial rounds (half of them before and half of them after the full rounds)
        constant %nRoundsP = 57;
        constant %rowsPerHash = %nRoundsF + %nRoundsP + 1;

        pol constant L0 = [1] + [0]*;
        pol constant FIRSTBLOCK(i) { match i % %rowsPerHash {
            0 => 1,
            _ => 0
        }};
        pol constant LASTBLOCK(i) { match i % %rowsPerHash {
            %rowsPerHash - 1 => 1,
            _ => 0
        }};

        // Whether the current round is a partial round
        pol constant PARTIAL = [0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]*;
        
        // The round constants
        pol constant C_0 = [0x0ee9a592ba9a9518d05986d656f40c2114c4993c11bb29938d21d47304cd8e6e, 0x2f27be690fdaee46c3ce28f7532b13c856c35342c84bda6e20966310fadc01d0, 0x28813dcaebaeaa828a376df87af4a63bc8b7bf27ad49c6298ef7b387bf28526d, 0x15b52534031ae18f7f862cb2cf7cf760ab10a8150a337b1ccd99ff6e8797d428, 0x10520b0ab721cadfe9eff81b016fc34dc76da36c2578937817cb978d069de559, 0x04df5a56ff95bcafb051f7b1cd43a99ba731ff67e47032058fe3d4185697cc7d, 0x052cba2255dfd00c7c483143ba8d469448e43586a9b4cd9183fd0e843a6b9fa6, 0x03150b7cd6d5d17b2529d36be0f67b832c4acfc884ef4ee5ce15be0bfb4a8d09, 0x233237e3289baa34bb147e972ebcb9516469c399fcc069fb88f9da2cc28276b5, 0x2a73b71f9b210cf5b14296572c9d32dbf156e2b086ff47dc5df542365a404ec0, 0x0b7475b102a165ad7f5b18db4e1e704f52900aa3253baac68246682e56e9a28e, 0x29a795e7d98028946e947b75d54e9f044076e87a7b2883b47b675ef5f38bd66e, 0x143fd115ce08fb27ca38eb7cce822b4517822cd2109048d2e6d0ddcca17d71c8, 0x2e4ef510ff0b6fda5fa940ab4c4380f26a6bcb64d89427b824d6755b5db9e30c, 0x30509991f88da3504bbf374ed5aae2f03448a22c76234c8c990f01f33a735206, 0x2a1982979c3ff7f43ddd543d891c2abddd80f804c077d775039aa3502e43adef, 0x21576b438e500449a151e4eeaf17b154285c68f42d42c1808a11abf3764c0750, 0x162f5243967064c390e095577984f291afba2266c38f5abcd89be0f5b2747eab, 0x1d6f347725e4816af2ff453f0cd56b199e1b61e9f601e9ade5e88db870949da9, 0x174ad61a1448c899a25416474f4930301e5c49475279e0639a616ddc45bc7b54, 0x2a4c4fc6ec0b0cf52195782871c6dd3b381cc65f72e02ad527037a62aa1bd804, 0x00ef653322b13d6c889bc81715c37d77a6cd267d595c4a8909a5546c7c97cff1, 0x2a56ef9f2c53febadfda33575dbdbd885a124e2780bbea170e456baace0fa5be, 0x04c6187e41ed881dc1b239c88f7f9d43a9f52fc8c8b6cdd1e76e47615b51f100, 0x2ab3561834ca73835ad05f5d7acb950b4a9a2c666b9726da832239065b7c3b02, 0x154ac98e01708c611c4fa715991f004898f57939d126e392042971dd90e81fc6, 0x06746a6156eba54426b9e22206f15abca9a6f41e6f535c6f3525401ea0654626, 0x2b56973364c4c4f5c1a3ec4da3cdce038811eb116fb3e45bc1768d26fc0b3758, 0x0fdc1f58548b85701a6c5505ea332a29647e6f34ad4243c2ea54ad897cebe54d, 0x16243916d69d2ca3dfb4722224d4c462b57366492f45e90d8a81934f1bc3b147, 0x05a8c4f9968b8aa3b7b478a30f9a5b63650f19a75e7ce11ca9fe16c0b76c00bc, 0x27e88d8c15f37dcee44f1e5425a51decbd136ce5091a6767e49ec9544ccd101a, 0x15742e99b9bfa323157ff8c586f5660eac6783476144cdcadf2874be45466b1a, 0x15a5821565cc2ec2ce78457db197edf353b7ebba2c5523370ddccc3d9f146a67, 0x2ff7bc8f4380cde997da00b616b0fcd1af8f0e91e2fe1ed7398834609e0315d2, 0x00248156142fd0373a479f91ff239e960f599ff7e94be69b7f2a290305e1198d, 0x29aba33f799fe66c2ef3134aea04336ecc37e38c1cd211ba482eca17e2dbfae1, 0x22cdbc8b70117ad1401181d02e15459e7ccd426fe869c7c95d1dd2cb0f24af38, 0x1166d9e554616dba9e753eea427c17b7fecd58c076dfe42708b08f5b783aa9af, 0x2af41fbb61ba8a80fdcf6fff9e3f6f422993fe8f0a4639f962344c8225145086, 0x28201a34c594dfa34d794996c6433a20d152bac2a7905c926c40e285ab32eeb6, 0x0ec868e6d15e51d9644f66e1d6471a94589511ca00d29e1014390e6ee4254f5b, 0x0b2d722d0919a1aad8db58f10062a92ea0c56ac4270e822cca228620188a1d40, 0x0c2d0e3b5fd57549329bf6885da66b9b790b40defd2c8650762305381b168873, 0x1e6ff3216b688c3d996d74367d5cd4c1bc489d46754eb712c243f70d1b53cfbb, 0x2522b60f4ea3307640a0c2dce041fba921ac10a3d5f096ef4745ca838285f019, 0x0f9406b8296564a37304507b8dba3ed162371273a07b1fc98011fcd6ad72205f, 0x193a56766998ee9e0a8652dd2f3b1da0362f4f54f72379544f957ccdeefb420f, 0x04e1181763050e58013444dbcb99f1902b11bc25d90bbdca408d3819f4fed32b, 0x1382edce9971e186497eadb1aeb1f52b23b4b83bef023ab0d15228b4cceca59a, 0x0a59a158e3eec2117e6e94e7f0e9decf18c3ffd5e1531a9219636158bbaf62f2, 0x13d69fa127d834165ad5c7cba7ad59ed52e0b0f0e42d7fea95e1906b520921b1, 0x256e175a1dc079390ecd7ca703fb2e3b19ec61805d4f03ced5f45ee6dd0f69ec, 0x193edd8e9fcf3d7625fa7d24b598a1d89f3362eaf4d582efecad76f879e36860, 0x10646d2f2603de39a1f4ae5e7771a64a702db6e86fb76ab600bf573f9010c711, 0x0a6abd1d833938f33c74154e0404b4b40a555bbbec21ddfafd672dd62047f01a, 0x161b42232e61b84cbf1810af93a38fc0cece3d5628c9282003ebacb5c312c72b, 0x2c8120f268ef054f817064c369dda7ea908377feaba5c4dffbda10ef58e8c556, 0x23ff4f9d46813457cf60d92f57618399a5e022ac321ca550854ae23918a22eea, 0x3050e37996596b7f81f68311431d8734dba7d926d3633595e0c0d8ddf4f0f47f, 0x2796ea90d269af29f5f8acf33921124e4e4fad3dbe658945e546ee411ddaa9cb, 0x054efa1f65b0fce283808965275d877b438da23ce5b13e1963798cb1447d25a4, 0x1cfb5662e8cf5ac9226a80ee17b36abecb73ab5f87e161927b4349e10e4bdf08, 0x0fa3ec5b9488259c2eb4cf24501bfad9be2ec9e42c5cc8ccd419d2a692cad870, 0x0fe0af7858e49859e2a54d6f1ad945b1316aa24bfbdd23ae40a6d0cb70c3eab1, 0]*;
        pol constant C_1 = [0x00f1445235f2148c5986587169fc1bcd887b08d4d00868df5696fff40956e864, 0x2b2ae1acf68b7b8d2416bebf3d4f6234b763fe04b8043ee48b8327bebca16cf2, 0x2727673b2ccbc903f181bf38e1c1d40d2033865200c352bc150928adddf9cb78, 0x0dc8fad6d9e4b35f5ed9a3d186b79ce38e0e8a8d1b58b132d701d4eecf68d1f6, 0x1f6d48149b8e7f7d9b257d8ed5fbbaf42932498075fed0ace88a9eb81f5627f6, 0x0672d995f8fff640151b3d290cedaf148690a10a8c8424a7f6ec282b6e4be828, 0x0b8badee690adb8eb0bd74712b7999af82de55707251ad7716077cb93c464ddc, 0x2cc6182c5e14546e3cf1951f173912355374efb83d80898abe69cb317c9ea565, 0x05c8f4f4ebd4a6e3c980d31674bfbe6323037f21b34ae5a4e80c2d4c24d60280, 0x1ac9b0417abcc9a1935107e9ffc91dc3ec18f2c4dbe7f22976a760bb5c50c460, 0x037c2849e191ca3edb1c5e49f6e8b8917c843e379366f2ea32ab3aa88d7f8448, 0x20439a0c84b322eb45a3857afc18f5826e8c7382c8a1585c507be199981fd22f, 0x0c64cbecb1c734b857968dbbdcf813cdf8611659323dbcbfc84323623be9caf1, 0x0081c95bc43384e663d79270c956ce3b8925b4f6d033b078b96384f50579400e, 0x1c3f20fd55409a53221b7c4d49a356b9f0a1119fb2067b41a7529094424ec6ad, 0x1c74ee64f15e1db6feddbead56d6d55dba431ebc396c9af95cad0f1315bd5c91, 0x2f17c0559b8fe79608ad5ca193d62f10bce8384c815f0906743d6930836d4a9e, 0x2b4cb233ede9ba48264ecd2c8ae50d1ad7a8596a87f29f8a7777a70092393311, 0x204b0c397f4ebe71ebc2d8b3df5b913df9e6ac02b68d31324cd49af5c4565529, 0x1a96177bcf4d8d89f759df4ec2f3cde2eaaa28c177cc0fa13a9816d49a38d2ef, 0x13ab2d136ccf37d447e9f2e14a7cedc95e727f8446f6d9d7e55afc01219fd649, 0x0e25483e45a665208b261d8ba74051e6400c776d652595d9845aca35d8a397d3, 0x1c8361c78eb5cf5decfb7a2d17b5c409f2ae2999a46762e8ee416240a8cb9af1, 0x13b37bd80f4d27fb10d84331f6fb6d534b81c61ed15776449e801b7ddc9c2967, 0x1d4d8ec291e720db200fe6d686c0d613acaf6af4e95d3bf69f7ed516a597b646, 0x0b339d8acca7d4f83eedd84093aef51050b3684c88f8b0b04524563bc6ea4da4, 0x0f18f5a0ecd1423c496f3820c549c27838e5790e2bd0a196ac917c7ff32077fb, 0x123769dd49d5b054dcd76b89804b1bcb8e1392b385716a5d83feb65d437f29ef, 0x12373a8251fea004df68abcf0f7786d4bceff28c5dbbe0c3944f685cc0a0b1f2, 0x1efbe46dd7a578b4f66f9adbc88b4378abc21566e1a0453ca13a4159cac04ac2, 0x20f057712cc21654fbfe59bd345e8dac3f7818c701b9c7882d9d57b72a32e83f, 0x2feed17b84285ed9b8a5c8c5e95a41f66e096619a7703223176c41ee433de4d1, 0x1aac285387f65e82c895fc6887ddf40577107454c6ec0317284f033f27d0c785, 0x2411d57a4813b9980efa7e31a1db5966dcf64f36044277502f15485f28c71727, 0x00b9831b948525595ee02724471bcd182e9521f6b7bb68f1e93be4febb0d3cbe, 0x171d5620b87bfb1328cf8c02ab3f0c9a397196aa6a542c2350eb512a2b2bcda9, 0x1e9bc179a4fdd758fdd1bb1945088d47e70d114a03f6a0e8b5ba650369e64973, 0x0ef042e454771c533a9f57a55c503fcefd3150f52ed94a7cd5ba93b9c7dacefd, 0x2de52989431a859593413026354413db177fbf4cd2ac0b56f855a888357ee466, 0x119e684de476155fe5a6b41a8ebc85db8718ab27889e85e781b214bace4827c3, 0x083efd7a27d1751094e80fefaf78b000864c82eb571187724a761f88c22cc4e7, 0x2af33e3f866771271ac0c9b3ed2e1142ecd3e74b939cd40d00d937ab84c98591, 0x1f790d4d7f8cf094d980ceb37c2453e957b54a9991ca38bbe0061d1ed6e562d4, 0x1162fb28689c27154e5a8228b4e72b377cbcafa589e283c35d3803054407a18d, 0x01ca8be73832b8d0681487d27d157802d741a6f36cdc2a0576881f9326478875, 0x23f0bee001b1029d5255075ddc957f833418cad4f52b6c3f8ce16c235572575b, 0x2360a8eb0cc7defa67b72998de90714e17e75b174a52ee4acb126c8cd995f0a8, 0x2a394a43934f86982f9be56ff4fab1703b2e63c8ad334834e4309805e777ae0f, 0x0fdb253dee83869d40c335ea64de8c5bb10eb82db08b5e8b1f5e5552bfd05f23, 0x03464990f045c6ee0819ca51fd11b0be7f61b8eb99f14b77e1e6634601d9e8b5, 0x06ec54c80381c052b58bf23b312ffd3ce2c4eba065420af8f4c23ed0075fd07b, 0x169a177f63ea681270b1c6877a73d21bde143942fb71dc55fd8a49f19f10c77b, 0x30102d28636abd5fe5f2af412ff6004f75cc360d3205dd2da002813d3e2ceeb2, 0x18168afd34f2d915d0368ce80b7b3347d1c7a561ce611425f2664d7aa51f0b5d, 0x0beb5e07d1b27145f575f1395a55bf132f90c25b40da7b3864d0242dcb1117fb, 0x1a679f5d36eb7b5c8ea12a4c2dedc8feb12dffeec450317270a6f19b34cf1860, 0x0ada10a90c7f0520950f7d47a60d5e6a493f09787f1564e5d09203db47de1a0b, 0x1c7c8824f758753fa57c00789c684217b930e95313bcb73e6e7b8649a4968f70, 0x09945a5d147a4f66ceece6405dddd9d0af5a2c5103529407dff1ea58f180426d, 0x15af1169396830a91600ca8102c35c426ceae5461e3f95d89d829518d30afd78, 0x202d7dd1da0f6b4b0325c8b3307742f01e15612ec8e9304a7cb0319e01d32d60, 0x1b162f83d917e93edb3308c29802deb9d8aa690113b2e14864ccf6e18e4165f1, 0x0f21177e302a771bbae6d8d1ecb373b62c99af346220ac0129c53f666eb24100, 0x193c0e04e0bd298357cb266c1506080ed36edce85c648cc085e8c57b1ab54bba, 0x216f6717bbc7dedb08536a2220843f4e2da5f1daa9ebdefde8a5ea7344798d22, 0]*;
        pol constant C_2 = [0x08dff3487e8ac99e1f29a058d0fa80b930c728730b7ab36ce879f3890ecf73f5, 0x0319d062072bef7ecca5eac06f97d4d55952c175ab6b03eae64b44c7dbf11cfa, 0x234ec45ca27727c2e74abd2b2a1494cd6efbd43e340587d6b8fb9e31e65cc632, 0x1bcd95ffc211fbca600f705fad3fb567ea4eb378f62e1fec97805518a47e4d9c, 0x1d9655f652309014d29e00ef35a2089bfff8dc1c816f0dc9ca34bdb5460c8705, 0x099952b414884454b21200d7ffafdd5f0c9a9dcc06f2708e9fc1d8209b5c75b9, 0x119b1590f13307af5a1ee651020c07c749c15d60683a8050b963d0a8e4b2bdd1, 0x005032551e6378c450cfe129a404b3764218cadedac14e2b92d2cd73111bf0f9, 0x0a7b1db13042d396ba05d818a319f25252bcf35ef3aeed91ee1f09b2590fc65b, 0x12c0339ae08374823fabb076707ef479269f3e4d6cb104349015ee046dc93fc0, 0x05a6811f8556f014e92674661e217e9bd5206c5c93a07dc145fdb176a716346f, 0x2e0ba8d94d9ecf4a94ec2050c7371ff1bb50f27799a84b6d4a2a6f2a0982c887, 0x028a305847c683f646fca925c163ff5ae74f348d62c2b670f1426cef9403da53, 0x2ed5f0c91cbd9749187e2fade687e05ee2491b349c039a0bba8a9f4023a0bb38, 0x10b4e7f3ab5df003049514459b6e18eec46bb2213e8e131e170887b47ddcb96c, 0x07533ec850ba7f98eab9303cace01b4b9e4f2e8b82708cfa9c2fe45a0ae146a0, 0x2d477e3862d07708a79e8aae946170bc9775a4201318474ae665b0b1b7e2730e, 0x2c8fbcb2dd8573dc1dbaf8f4622854776db2eece6d85c4cf4254e7c35e03b07a, 0x0c4cb9dc3c4fd8174f1149b3c63c3c2f9ecb827cd7dc25534ff8fb75bc79c502, 0x066d04b24331d71cd0ef8054bc60c4ff05202c126a233c1a8242ace360b8a30a, 0x1121552fca26061619d24d843dc82769c1b04fcec26f55194c2e3e869acc6a9a, 0x29f536dcb9dd7682245264659e15d88e395ac3d4dde92d8c46448db979eeba89, 0x151aff5f38b20a0fc0473089aaf0206b83e8e68a764507bfd3d0ab4be74319c5, 0x01a5c536273c2d9df578bfbd32c17b7a2ce3664c2a52032c9321ceb1c4e8a8e4, 0x041294d2cc484d228f5784fe7919fd2bb925351240a04b711514c9c80b65af1d, 0x0955e49e6610c94254a4f84cfbab344598f0e71eaff4a7dd81ed95b50839c82e, 0x04f6eeca1751f7308ac59eff5beb261e4bb563583ede7bc92a738223d6f76e13, 0x2147b424fc48c80a88ee52b91169aacea989f6446471150994257b2fb01c63e9, 0x21e4f4ea5f35f85bad7ea52ff742c9e8a642756b6af44203dd8a1f35c1a90035, 0x07ea5e8537cf5dd08886020e23a7f387d468d5525be66f853b672cc96a88969a, 0x04a12ededa9dfd689672f8c67fee31636dcd8e88d01d49019bd90b33eb33db69, 0x1ed7cc76edf45c7c404241420f729cf394e5942911312a0d6972b8bd53aff2b8, 0x25851c3c845d4790f9ddadbdb6057357832e2e7a49775f71ec75a96554d67c77, 0x002e6f8d6520cd4713e335b8c0b6d2e647e9a98e12f4cd2558828b5ef6cb4c9b, 0x0a2f53768b8ebf6a86913b0e57c04e011ca408648a4743a87d77adbf0c9c3512, 0x170a4f55536f7dc970087c7c10d6fad760c952172dd54dd99d1045e4ec34a808, 0x1dd269799b660fad58f7f4892dfb0b5afeaad869a9c4b44f9c9e1c43bdaf8f09, 0x11609e06ad6c8fe2f287f3036037e8851318e8b08a0359a03b304ffca62e8284, 0x3006eb4ffc7a85819a6da492f3a8ac1df51aee5b17b8e89d74bf01cf5f71e9ad, 0x1835b786e2e8925e188bea59ae363537b51248c23828f047cff784b97b3fd800, 0x0b6f88a3577199526158e61ceea27be811c16df7774dd8519e079564f61fd13b, 0x0b520211f904b5e7d09b5d961c6ace7734568c547dd6858b364ce5e47951f178, 0x0171eb95dfbf7d1eaea97cd385f780150885c16235a2a6a8da92ceb01e504233, 0x2f1459b65dee441b64ad386a91e8310f282c5a92a89e19921623ef8249711bc0, 0x1f7735706ffe9fc586f976d5bdf223dc680286080b10cea00b9b5de315f9650e, 0x2bc1ae8b8ddbb81fcaac2d44555ed5685d142633e9df905f66d9401093082d59, 0x15871a5cddead976804c803cbaef255eb4815a5e96df8b006dcbbc2767f88948, 0x1859954cfeb8695f3e8b635dcb345192892cd11223443ba7b4166e8876c0d142, 0x058cbe8a9a5027bdaa4efb623adead6275f08686f1c08984a9d7c5bae9b4f1c0, 0x23f7bfc8720dc296fff33b41f98ff83c6fcab4605db2eb5aaa5bc137aeb70a58, 0x118872dc832e0eb5476b56648e867ec8b09340f7a7bcb1b4962f0ff9ed1f9d01, 0x04ef51591c6ead97ef42f287adce40d93abeb032b922f66ffb7e9a5a7450544d, 0x10998e42dfcd3bbf1c0714bc73eb1bf40443a3fa99bef4a31fd31be182fcc792, 0x29383c01ebd3b6ab0c017656ebe658b6a328ec77bc33626e29e2e95b33ea6111, 0x16d685252078c133dc0d3ecad62b5c8830f95bb2e54b59abdffbf018d96fa336, 0x0980fb233bd456c23974d50e0ebfde4726a423eada4e8f6ffbc7592e3f1b93d6, 0x1a730d372310ba82320345a29ac4238ed3f07a8a2b4e121bb50ddb9af407f451, 0x2cd9ed31f5f8691c8e39e4077a74faa0f400ad8b491eb3f7b47b27fa3fd1cf77, 0x188d9c528025d4c2b67660c6b771b90f7c7da6eaa29d3f268a6dd223ec6fc630, 0x1da6d09885432ea9a06d9f37f873d985dae933e351466b2904284da3320d8acc, 0x096d6790d05bb759156a952ba263d672a2d7f9c788f4c831a29dace4c0f8be5f, 0x21e5241e12564dd6fd9f1cdd2a0de39eedfefc1466cc568ec5ceb745a0506edc, 0x1671522374606992affb0dd7f71b12bec4236aede6290546bcef7e1f515c2320, 0x102adf8ef74735a27e9128306dcbc3c99f6f7291cd406578ce14ea2adaba68f8, 0x1da55cc900f0d21f4a3e694391918a1b3c23b2ac773c6b3ef88e2e4228325161, 0]*;

        // State of the Poseidon permutation
        pol commit in0, in1, cap;

        // The initial state of the Poseidon permutation
        // (constrained to be equal to (in0, in1, cap) in the first row and then repeated until
        // the end of the block)
        pol commit input_in0, input_in1, input_cap;

        // Add round constants
        pol commit a0, a1, a2;
        a0 = in0 + C_0;
        a1 = in1 + C_1;
        a2 = cap + C_2;

        // Compute S-Boxes (x^5)
        pol commit x2_0, x4_0, x5_0;
        x2_0 = a0 * a0;
        x4_0 = x2_0 * x2_0;
        x5_0 = x4_0 * a0;

        pol commit x2_1, x4_1, x5_1;
        x2_1 = a1 * a1;
        x4_1 = x2_1 * x2_1;
        x5_1 = x4_1 * a1;

        pol commit x2_2, x4_2, x5_2;
        x2_2 = a2 * a2;
        x4_2 = x2_2 * x2_2;
        x5_2 = x4_2 * a2;

        // Apply S-Boxes on the first element and otherwise if it is a full round.
        pol commit b0, b1, b2;
        b0 = x5_0;
        b1 = PARTIAL * (a1 - x5_1) + x5_1;
        b2 = PARTIAL * (a2 - x5_2) + x5_2;

        // The MDS matrix
        constant %m_0_0 = 0x109b7f411ba0e4c9b2b70caf5c36a7b194be7c11ad24378bfedb68592ba8118b;
        constant %m_0_1 = 0x16ed41e13bb9c0c66ae119424fddbcbc9314dc9fdbdeea55d6c64543dc4903e0;
        constant %m_0_2 = 0x2b90bba00fca0589f617e7dcbfe82e0df706ab640ceb247b791a93b74e36736d;
        constant %m_1_0 = 0x2969f27eed31a480b9c36c764379dbca2cc8fdd1415c3dded62940bcde0bd771;
        constant %m_1_1 = 0x2e2419f9ec02ec394c9871c832963dc1b89d743c8c7b964029b2311687b1fe23;
        constant %m_1_2 = 0x101071f0032379b697315876690f053d148d4e109f5fb065c8aacc55a0f89bfa;
        constant %m_2_0 = 0x143021ec686a3f330d5f9e654638065ce6cd79e28c5b3753326244ee65a1b1a7;
        constant %m_2_1 = 0x176cc029695ad02582a70eff08a6fd99d057e12e58e7d7b6b16cdfabc8ee2911;
        constant %m_2_2 = 0x19a3fc0a56702bf417ba7fee3802593fa644470307043f7773279cd71d25d5e0;

        // Multiply with MDS Matrix
        pol commit c0, c1, c2;
        c0 = %m_0_0 * b0 + %m_0_1 * b1 + %m_0_2 * b2;
        c1 = %m_1_0 * b0 + %m_1_1 * b1 + %m_1_2 * b2;
        c2 = %m_2_0 * b0 + %m_2_1 * b1 + %m_2_2 * b2;

        (in0' - c0) * (1-LASTBLOCK) = 0;
        (in1' - c1) * (1-LASTBLOCK) = 0;
        (cap' - c2) * (1-LASTBLOCK) = 0;

        FIRSTBLOCK * (input_in0 - in0) = 0;
        FIRSTBLOCK * (input_in1 - in1) = 0;
        FIRSTBLOCK * (input_cap - cap) = 0;

        (1 - LASTBLOCK) * (input_in0 - input_in0') = 0;
        (1 - LASTBLOCK) * (input_in1 - input_in1') = 0;
        (1 - LASTBLOCK) * (input_cap - input_cap') = 0;
    }
}
"#,
    ]
}

impl Register {
    pub fn new(value: u8) -> Self {
        Self { value }
    }

    pub fn is_zero(&self) -> bool {
        self.value == 0
    }
}

impl asm_utils::ast::Register for Register {}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x{}", self.value)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FunctionKind {
    HiDataRef,
    LoDataRef,
}

impl asm_utils::ast::FunctionOpKind for FunctionKind {}

impl fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionKind::HiDataRef => write!(f, "%hi"),
            FunctionKind::LoDataRef => write!(f, "%lo"),
        }
    }
}

#[derive(Default)]
pub struct Risc {}

impl asm_utils::compiler::Compiler for Risc {
    /// Compiles riscv assembly to POWDR assembly. Adds required library routines.
    fn compile(mut assemblies: BTreeMap<String, String>) -> String {
        // stack grows towards zero
        let stack_start = 0x10000;
        // data grows away from zero
        let data_start = 0x10100;

        assert!(assemblies
            .insert("__runtime".to_string(), runtime().to_string())
            .is_none());

        // TODO remove unreferenced files.
        let (mut statements, file_ids) = disambiguator::disambiguate(
            assemblies
                .into_iter()
                .map(|(name, contents)| (name, parse_asm(RiscParser::default(), &contents)))
                .collect(),
        );
        let (mut objects, mut object_order) = data_parser::extract_data_objects(&statements);
        assert_eq!(objects.keys().len(), object_order.len());

        // Reduce to the code that is actually reachable from main
        // (and the objects that are referred from there)
        reachability::filter_reachable_from("__runtime_start", &mut statements, &mut objects);

        // Replace dynamic references to code labels
        replace_dynamic_label_references(&mut statements, &objects);

        // Remove the riscv asm stub function, which is used
        // for compilation, and will not be called.
        statements = replace_coprocessor_stubs(statements).collect::<Vec<_>>();

        // Sort the objects according to the order of the names in object_order.
        // With the single exception: If there is large object, put that at the end.
        // The idea behind this is that there might be a single gigantic object representing the heap
        // and putting that at the end should keep memory addresses small.
        let mut large_objects = objects
            .iter()
            .filter(|(_name, data)| data.iter().map(|d| d.size()).sum::<usize>() > 0x2000);
        if let (Some((heap, _)), None) = (large_objects.next(), large_objects.next()) {
            let heap_pos = object_order.iter().position(|o| o == heap).unwrap();
            object_order.remove(heap_pos);
            object_order.push(heap.clone());
        };
        let sorted_objects = object_order
            .into_iter()
            .filter_map(|n| {
                let value = objects.get_mut(&n).map(std::mem::take);
                value.map(|v| (n, v))
            })
            .collect::<Vec<_>>();
        let (data_code, data_positions) = store_data_objects(&sorted_objects, data_start);

        riscv_machine(
            &machine_decls(),
            &preamble(),
            &[("binary", "Binary"), ("shift", "Shift")],
            file_ids
                .into_iter()
                .map(|(id, dir, file)| format!("debug file {id} {} {};", quote(&dir), quote(&file)))
                .chain(["call __data_init;".to_string()])
                .chain([
                    format!("// Set stack pointer\nx2 <=X= {stack_start};"),
                    "jump __runtime_start;".to_string(),
                ])
                .chain(
                    substitute_symbols_with_values(statements, &data_positions)
                        .into_iter()
                        .flat_map(process_statement),
                )
                .chain(["// This is the data initialization routine.\n__data_init::".to_string()])
                .chain(data_code)
                .chain(["// This is the end of the data initialization routine.\nret;".to_string()])
                .collect(),
        )
    }
}

/// Replace certain patterns of references to code labels by
/// special instructions. We ignore any references to data objects
/// because they will be handled differently.
fn replace_dynamic_label_references(
    statements: &mut Vec<Statement>,
    data_objects: &BTreeMap<String, Vec<DataValue>>,
) {
    /*
    Find patterns of the form
    lui	a0, %hi(LABEL)
    addi	s10, a0, %lo(LABEL)
    -
    turn this into the pseudo-riscv-instruction
    load_dynamic s10, LABEL
    which is then turned into

    s10 <== load_label(LABEL)

    It gets more complicated by the fact that sometimes, labels
    and debugging directives occur between the two statements
    matching that pattern...
    */
    let instruction_indices = statements
        .iter()
        .enumerate()
        .filter_map(|(i, s)| match s {
            Statement::Instruction(_, _) => Some(i),
            _ => None,
        })
        .collect::<Vec<_>>();

    let mut to_delete = BTreeSet::default();
    for (i1, i2) in instruction_indices.into_iter().tuple_windows() {
        if let Some(r) =
            replace_dynamic_label_reference(&statements[i1], &statements[i2], data_objects)
        {
            to_delete.insert(i1);
            statements[i2] = r;
        }
    }

    let mut i = 0;
    statements.retain(|_| (!to_delete.contains(&i), i += 1).0);
}

fn replace_dynamic_label_reference(
    s1: &Statement,
    s2: &Statement,
    data_objects: &BTreeMap<String, Vec<DataValue>>,
) -> Option<Statement> {
    let Statement::Instruction(instr1, args1) = s1 else { return None; };
    let Statement::Instruction(instr2, args2) = s2 else { return None; };
    if instr1.as_str() != "lui" || instr2.as_str() != "addi" {
        return None;
    };
    let [Argument::Register(r1), Argument::Expression(Expression::FunctionOp(FunctionKind::HiDataRef, expr1))] = &args1[..] else { return None; };
    // Maybe should try to reduce expr1 and expr2 before comparing deciding it is a pure symbol?
    let Expression::Symbol(label1) = expr1.as_ref() else { return None; };
    let [Argument::Register(r2), Argument::Register(r3), Argument::Expression(Expression::FunctionOp(FunctionKind::LoDataRef, expr2))] = &args2[..] else { return None; };
    let Expression::Symbol(label2) = expr2.as_ref() else { return None; };
    if r1 != r3 || label1 != label2 || data_objects.contains_key(label1) {
        return None;
    }
    Some(Statement::Instruction(
        "load_dynamic".to_string(),
        vec![
            Argument::Register(*r2),
            Argument::Expression(Expression::Symbol(label1.clone())),
        ],
    ))
}

fn remove_matching_and_next<I: Iterator, F>(iter: I, predicate: F) -> impl Iterator<Item = I::Item>
where
    F: Fn(&I::Item) -> bool,
{
    iter.scan(false, move |filter_next, item| {
        let mut filter_current = *filter_next;
        *filter_next = predicate(&item);
        // if the predicate says this line should be filtered, then
        // the next one should be filtered as well.
        filter_current |= *filter_next;
        Some((filter_current, item))
    })
    .filter_map(|(filter, statement)| (!filter).then_some(statement))
}

fn replace_coprocessor_stubs(
    statements: impl IntoIterator<Item = Statement>,
) -> impl Iterator<Item = Statement> {
    let stub_names: Vec<&str> = COPROCESSOR_SUBSTITUTIONS
        .iter()
        .map(|(name, _)| *name)
        .collect();

    remove_matching_and_next(statements.into_iter(), move |statement| -> bool {
        matches!(&statement, Statement::Label(label) if stub_names.contains(&label.as_str()))
    })
}

fn store_data_objects<'a>(
    objects: impl IntoIterator<Item = &'a (String, Vec<DataValue>)> + Copy,
    mut memory_start: u32,
) -> (Vec<String>, BTreeMap<String, u32>) {
    memory_start = ((memory_start + 7) / 8) * 8;
    let mut current_pos = memory_start;
    let mut positions = BTreeMap::new();
    for (name, data) in objects.into_iter() {
        // TODO check if we need to use multiples of four.
        let size: u32 = data
            .iter()
            .map(|d| next_multiple_of_four(d.size()) as u32)
            .sum();
        positions.insert(name.clone(), current_pos);
        current_pos += size;
    }

    let code = objects
        .into_iter()
        .filter(|(_, data)| !data.is_empty())
        .flat_map(|(name, data)| {
            let mut object_code = vec![];
            let mut pos = positions[name];
            for item in data {
                match &item {
                    DataValue::Zero(_length) => {
                        // We can assume memory to be zero-initialized,
                        // so we do nothing.
                    }
                    DataValue::Direct(bytes) => {
                        for i in (0..bytes.len()).step_by(4) {
                            let v = (0..4)
                                .map(|j| {
                                    (bytes.get(i + j).cloned().unwrap_or_default() as u32)
                                        << (j * 8)
                                })
                                .reduce(|a, b| a | b)
                                .unwrap();
                            // We can assume memory to be zero-initialized.
                            if v != 0 {
                                object_code.extend([
                                    format!("addr <=X= 0x{:x};", pos + i as u32),
                                    format!("mstore 0x{v:x};"),
                                ]);
                            }
                        }
                    }
                    DataValue::Reference(sym) => {
                        object_code.push(format!("addr <=X= 0x{pos:x};"));
                        if let Some(p) = positions.get(sym) {
                            object_code.push(format!("mstore 0x{p:x};"));
                        } else {
                            // code reference
                            // TODO should be possible without temporary
                            object_code.extend([
                                format!("tmp1 <== load_label({});", escape_label(sym)),
                                "mstore tmp1;".to_string(),
                            ]);
                        }
                    }
                    DataValue::Offset(_, _) => {
                        unimplemented!()

                        /*
                        object_code.push(format!("addr <=X= 0x{pos:x};"));

                        I think this solution should be fine but hard to say without
                        an actual code snippet that uses it.

                        // TODO should be possible without temporary
                        object_code.extend([
                            format!("tmp1 <== load_label({});", escape_label(a)),
                            format!("tmp2 <== load_label({});", escape_label(b)),
                            "mstore tmp1 - tmp2;".to_string(),
                        ]);
                        */
                    }
                }
                pos += item.size() as u32;
            }
            if let Some(first_line) = object_code.first_mut() {
                *first_line = format!("// data {name}\n") + first_line;
            }
            object_code
        })
        .collect();
    (code, positions)
}

fn next_multiple_of_four(x: usize) -> usize {
    ((x + 3) / 4) * 4
}

fn substitute_symbols_with_values(
    mut statements: Vec<Statement>,
    data_positions: &BTreeMap<String, u32>,
) -> Vec<Statement> {
    for s in &mut statements {
        let Statement::Instruction(_name, args) = s else { continue; };
        for arg in args {
            arg.post_visit_expressions_mut(&mut |expression| match expression {
                Expression::Number(_) => {}
                Expression::Symbol(symb) => {
                    if let Some(pos) = data_positions.get(symb) {
                        *expression = Expression::Number(*pos as i64)
                    }
                }
                Expression::UnaryOp(op, subexpr) => {
                    if let Expression::Number(num) = subexpr.as_ref() {
                        let result = match op {
                            UnaryOpKind::Negation => -num,
                        };
                        *expression = Expression::Number(result);
                    };
                }
                Expression::BinaryOp(op, subexprs) => {
                    if let (Expression::Number(a), Expression::Number(b)) =
                        (&subexprs[0], &subexprs[1])
                    {
                        let result = match op {
                            BinaryOpKind::Or => a | b,
                            BinaryOpKind::Xor => a ^ b,
                            BinaryOpKind::And => a & b,
                            BinaryOpKind::LeftShift => a << b,
                            BinaryOpKind::RightShift => a >> b,
                            BinaryOpKind::Add => a + b,
                            BinaryOpKind::Sub => a - b,
                            BinaryOpKind::Mul => a * b,
                            BinaryOpKind::Div => a / b,
                            BinaryOpKind::Mod => a % b,
                        };
                        *expression = Expression::Number(result);
                    }
                }
                Expression::FunctionOp(op, subexpr) => {
                    if let Expression::Number(num) = subexpr.as_ref() {
                        let result = match op {
                            FunctionKind::HiDataRef => num >> 12,
                            FunctionKind::LoDataRef => num & 0xfff,
                        };
                        *expression = Expression::Number(result);
                    };
                }
            });
        }
    }
    statements
}

fn riscv_machine(
    machines: &[&str],
    preamble: &str,
    submachines: &[(&str, &str)],
    program: Vec<String>,
) -> String {
    format!(
        r#"
{}
machine Main {{
{}

{}

    function main {{
{}
    }}
}}    
"#,
        machines.join("\n"),
        submachines
            .iter()
            .map(|(instance, ty)| format!("\t\t{} {};", ty, instance))
            .collect::<Vec<_>>()
            .join("\n"),
        preamble,
        program
            .into_iter()
            .map(|line| format!("\t\t{line}"))
            .collect::<Vec<_>>()
            .join("\n")
    )
}

fn preamble() -> String {
    r#"
    degree 262144;
    reg pc[@pc];
    reg X[<=];
    reg Y[<=];
    reg Z[<=];
    reg A[<=];
    reg tmp1;
    reg tmp2;
    reg tmp3;
"#
    .to_string()
        + &(0..32)
            .map(|i| format!("\t\treg x{i};\n"))
            .collect::<Vec<_>>()
            .concat()
        + r#"
    reg addr;

    constraints {
        x0 = 0;
    }

    constraints{
    // ============== iszero check for X =======================
        col witness XInv;
        col witness XIsZero;
        XIsZero = 1 - X * XInv;
        XIsZero * X = 0;
        XIsZero * (1 - XIsZero) = 0;

    // =============== read-write memory =======================
        // Read-write memory. Columns are sorted by m_addr and
        // then by m_step. m_change is 1 if and only if m_addr changes
        // in the next row.
        col witness m_addr;
        col witness m_step;
        col witness m_change;
        col witness m_value;
        // If we have an operation at all (needed because this needs to be a permutation)
        col witness m_op;
        // If the operation is a write operation.
        col witness m_is_write;
        col witness m_is_read;

        // positive numbers (assumed to be much smaller than the field order)
        col fixed POSITIVE(i) { i + 1 };
        col fixed FIRST = [1] + [0]*;
        col fixed LAST(i) { FIRST(i + 1) };
        col fixed STEP(i) { i };

        m_change * (1 - m_change) = 0;

        // if m_change is zero, m_addr has to stay the same.
        (m_addr' - m_addr) * (1 - m_change) = 0;

        // Except for the last row, if m_change is 1, then m_addr has to increase,
        // if it is zero, m_step has to increase.
        (1 - LAST) { m_change * (m_addr' - m_addr) + (1 - m_change) * (m_step' - m_step) } in POSITIVE;

        m_op * (1 - m_op) = 0;
        m_is_write * (1 - m_is_write) = 0;
        m_is_read * (1 - m_is_read) = 0;
        // m_is_write can only be 1 if m_op is 1.
        m_is_write * (1 - m_op) = 0;
        m_is_read * (1 - m_op) = 0;
        m_is_read * m_is_write = 0;


        // If the next line is a read and we stay at the same address, then the
        // value cannot change.
        (1 - m_is_write') * (1 - m_change) * (m_value' - m_value) = 0;

        // If the next line is a read and we have an address change,
        // then the value is zero.
        (1 - m_is_write') * m_change * m_value' = 0;
    }

    // ============== memory instructions ==============

    instr mstore X { { addr, STEP, X } is m_is_write { m_addr, m_step, m_value } }
    instr mload -> X { { addr, STEP, X } is m_is_read { m_addr, m_step, m_value } }

    // ============== control-flow instructions ==============

    instr jump l: label { pc' = l }
    instr load_label l: label -> X { X = l }
    instr jump_dyn X { pc' = X }
    instr jump_and_link_dyn X { pc' = X, x1' = pc + 1 }
    instr call l: label { pc' = l, x1' = pc + 1 }
    // TODO x6 actually stores some relative address, but only part of it.
    instr tail l: label { pc' = l, x6' = l }
    instr ret { pc' = x1 }

    instr branch_if_nonzero X, l: label { pc' = (1 - XIsZero) * l + XIsZero * (pc + 1) }
    instr branch_if_zero X, l: label { pc' = XIsZero * l + (1 - XIsZero) * (pc + 1) }

    // input X is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < X < 2**32
    instr branch_if_positive X, l: label {
        X + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
        pc' = wrap_bit * l + (1 - wrap_bit) * (pc + 1)
    }
    // input X is required to be the difference of two 32-bit unsigend values.
    // i.e. -2**32 < X < 2**32
    instr is_positive X -> Y {
        X + 2**32 - 1 = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 + wrap_bit * 2**32,
        Y = wrap_bit
    }

    // ================= logical instructions =================

    instr is_equal_zero X -> Y { Y = XIsZero }
    instr is_not_equal_zero X -> Y { Y = 1 - XIsZero }

    // ================= coprocessor substitution instructions =================

    PoseidonBN254 poseidon;

    instr poseidon X, Y, Z -> A = poseidon.poseidon_permutation

    // ================= binary/bitwise instructions =================

    instr and Y, Z -> X = binary.and

    instr or Y, Z -> X = binary.or

    instr xor Y, Z -> X = binary.xor

    // ================= shift instructions =================

    instr shl Y, Z -> X = shift.shl

    instr shr Y, Z -> X = shift.shr

    // ================== wrapping instructions ==============

    // Wraps a value in Y to 32 bits.
    // Requires 0 <= Y < 2**33
    instr wrap Y -> X { Y = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    // Requires -2**32 <= Y < 2**32
    instr wrap_signed Y -> X { Y + 2**32 = X + wrap_bit * 2**32, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    constraints{
        col fixed bytes(i) { i & 0xff };
        col witness X_b1;
        col witness X_b2;
        col witness X_b3;
        col witness X_b4;
        { X_b1 } in { bytes };
        { X_b2 } in { bytes };
        { X_b3 } in { bytes };
        { X_b4 } in { bytes };
        col witness wrap_bit;
        wrap_bit * (1 - wrap_bit) = 0;
    }

    // Input is a 32 bit unsigned number. We check the 7th bit and set all higher bits to that value.
    instr sign_extend_byte Y -> X {
        // wrap_bit is used as sign_bit here.
        Y = Y_7bit + wrap_bit * 0x80 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000,
        X = Y_7bit + wrap_bit * 0xffffff80
    }
    constraints{
        col fixed seven_bit(i) { i & 0x7f };
        col witness Y_7bit;
        { Y_7bit } in { seven_bit };
    }

    // Input is a 32 but unsigned number (0 <= Y < 2**32) interpreted as a two's complement numbers.
    // Returns a signed number (-2**31 <= X < 2**31).
    instr to_signed Y -> X {
        // wrap_bit is used as sign_bit here.
        Y = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + Y_7bit * 0x1000000 + wrap_bit * 0x80000000,
        X = Y - wrap_bit * 2**32
    }

    // ======================= assertions =========================

    instr fail { 1 = 0 }

    // Removes up to 16 bits beyond 32
    // TODO is this really safe?
    instr wrap16 Y -> X { Y = Y_b5 * 2**32 + Y_b6 * 2**40 + X, X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000 }
    constraints {
        col witness Y_b5;
        col witness Y_b6;
        col witness Y_b7;
        col witness Y_b8;
        { Y_b5 } in { bytes };
        { Y_b6 } in { bytes };
        { Y_b7 } in { bytes };
        { Y_b8 } in { bytes };

        col witness remainder; 

        col witness REM_b1;
        col witness REM_b2;
        col witness REM_b3;
        col witness REM_b4;
        { REM_b1 } in { bytes };
        { REM_b2 } in { bytes };
        { REM_b3 } in { bytes };
        { REM_b4 } in { bytes };
    }

    // implements Z = Y / X, stores remainder in `remainder`.
    instr divu Y, X -> Z {
        // Y is the known dividend
        // X is the known divisor
        // Z is the unknown quotient
        // main division algorithm;
        // if X is zero, remainder is set to dividend, as per RISC-V specification:
        X * Z + remainder = Y,

        // remainder >= 0:
        remainder = REM_b1 + REM_b2 * 0x100 + REM_b3 * 0x10000 + REM_b4 * 0x1000000,

        // remainder < divisor, conditioned to X not being 0:
        (1 - XIsZero) * (X - remainder - 1 - Y_b5 - Y_b6 * 0x100 - Y_b7 * 0x10000 - Y_b8 * 0x1000000) = 0,

        // in case X is zero, we set quotient according to RISC-V specification
        XIsZero * (Z - 0xffffffff) = 0,

        // quotient is 32 bits:
        Z = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }

    // Removes up to 32 bits beyond 32
    // TODO is this really safe?
    instr mul Y, Z -> X {
        Y * Z = X + Y_b5 * 2**32 + Y_b6 * 2**40 + Y_b7 * 2**48 + Y_b8 * 2**56,
        X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }
    // implements (Y * Z) >> 32
    instr mulhu Y, Z -> X {
        Y * Z = X * 2**32 + Y_b5 + Y_b6 * 0x100 + Y_b7 * 0x10000 + Y_b8 * 0x1000000,
        X = X_b1 + X_b2 * 0x100 + X_b3 * 0x10000 + X_b4 * 0x1000000
    }
"#
}

fn runtime() -> &'static str {
    r#"
.globl __udivdi3@plt
.globl __udivdi3
.set __udivdi3@plt, __udivdi3

.globl memcpy@plt
.globl memcpy
.set memcpy@plt, memcpy

.globl memmove@plt
.globl memmove
.set memmove@plt, memmove

.globl memset@plt
.globl memset
.set memset@plt, memset

.globl memcmp@plt
.globl memcmp
.set memcmp@plt, memcmp

.globl bcmp@plt
.globl bcmp
.set bcmp@plt, bcmp

.globl strlen@plt
.globl strlen
.set strlen@plt, strlen

.globl __rust_alloc
.set __rust_alloc, __rg_alloc

.globl __rust_dealloc
.set __rust_dealloc, __rg_dealloc

.globl __rust_realloc
.set __rust_realloc, __rg_realloc

.globl __rust_alloc_zeroed
.set __rust_alloc_zeroed, __rg_alloc_zeroed

.globl __rust_alloc_error_handler
.set __rust_alloc_error_handler, __rg_oom

.globl poseidon_coprocessor
poseidon_coprocessor:
    ret
"#
}

fn process_statement(s: Statement) -> Vec<String> {
    match &s {
        Statement::Label(l) => vec![format!("{}::", escape_label(l))],
        Statement::Directive(directive, args) => match (directive.as_str(), &args[..]) {
            (
                ".loc",
                [Argument::Expression(Expression::Number(file)), Argument::Expression(Expression::Number(line)), Argument::Expression(Expression::Number(column)), ..],
            ) => {
                vec![format!("  debug loc {file} {line} {column};")]
            }
            (".file", _) => {
                // We ignore ".file" directives because they have been extracted to the top.
                vec![]
            }
            _ if directive.starts_with(".cfi_") => vec![],
            _ => panic!(
                "Leftover directive in code: {directive} {}",
                args.iter().map(|s| s.to_string()).join(", ")
            ),
        },
        Statement::Instruction(instr, args) => process_instruction(instr, args)
            .into_iter()
            .map(|s| "  ".to_string() + &s)
            .collect(),
    }
}

fn quote(s: &str) -> String {
    // TODO more things to quote
    format!("\"{}\"", s.replace('\\', "\\\\").replace('\"', "\\\""))
}

fn escape_label(l: &str) -> String {
    // TODO make this proper
    l.replace('.', "_dot_").replace('/', "_slash_")
}

fn argument_to_number(x: &Argument) -> u32 {
    if let Argument::Expression(expr) = x {
        expression_to_number(expr)
    } else {
        panic!("Expected numeric expression, got {x}")
    }
}

fn expression_to_number(expr: &Expression) -> u32 {
    if let Expression::Number(n) = expr {
        *n as u32
    } else {
        panic!("Constant expression could not be fully resolved to a number during preprocessing: {expr}");
    }
}

fn argument_to_escaped_symbol(x: &Argument) -> String {
    if let Argument::Expression(Expression::Symbol(symb)) = x {
        escape_label(symb)
    } else {
        panic!("Expected a symbol, got {x}");
    }
}

fn r(args: &[Argument]) -> Register {
    match args {
        [Argument::Register(r1)] => *r1,
        _ => panic!(),
    }
}

fn rri(args: &[Argument]) -> (Register, Register, u32) {
    match args {
        [Argument::Register(r1), Argument::Register(r2), n] => (*r1, *r2, argument_to_number(n)),
        _ => panic!(),
    }
}

fn rrr(args: &[Argument]) -> (Register, Register, Register) {
    match args {
        [Argument::Register(r1), Argument::Register(r2), Argument::Register(r3)] => (*r1, *r2, *r3),
        _ => panic!(),
    }
}

fn ri(args: &[Argument]) -> (Register, u32) {
    match args {
        [Argument::Register(r1), n] => (*r1, argument_to_number(n)),
        _ => panic!(),
    }
}

fn rr(args: &[Argument]) -> (Register, Register) {
    match args {
        [Argument::Register(r1), Argument::Register(r2)] => (*r1, *r2),
        _ => panic!(),
    }
}

fn rrl(args: &[Argument]) -> (Register, Register, String) {
    match args {
        [Argument::Register(r1), Argument::Register(r2), l] => {
            (*r1, *r2, argument_to_escaped_symbol(l))
        }
        _ => panic!(),
    }
}

fn rl(args: &[Argument]) -> (Register, String) {
    match args {
        [Argument::Register(r1), l] => (*r1, argument_to_escaped_symbol(l)),
        _ => panic!(),
    }
}

fn rro(args: &[Argument]) -> (Register, Register, u32) {
    match args {
        [Argument::Register(r1), Argument::RegOffset(r2, off)] => {
            (*r1, *r2, expression_to_number(off))
        }
        _ => panic!(),
    }
}

fn only_if_no_write_to_zero(statement: String, reg: Register) -> Vec<String> {
    only_if_no_write_to_zero_vec(vec![statement], reg)
}

fn only_if_no_write_to_zero_vec(statements: Vec<String>, reg: Register) -> Vec<String> {
    if reg.is_zero() {
        vec![]
    } else {
        statements
    }
}

static COPROCESSOR_SUBSTITUTIONS: &[(&str, &str)] =
    &[("poseidon_coprocessor", "x10 <== poseidon(x10, x11, x12);")];

fn try_coprocessor_substitution(label: &str) -> Option<&'static str> {
    COPROCESSOR_SUBSTITUTIONS
        .iter()
        .find(|(l, _)| *l == label)
        .map(|&(_, subst)| subst)
}

fn process_instruction(instr: &str, args: &[Argument]) -> Vec<String> {
    match instr {
        // load/store registers
        "li" => {
            let (rd, imm) = ri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {imm};"), rd)
        }
        // TODO check if it is OK to clear the lower order bits
        "lui" => {
            let (rd, imm) = ri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {};", imm << 12), rd)
        }
        "la" => {
            let (rd, addr) = ri(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {};", addr), rd)
        }
        "mv" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=X= {rs};"), rd)
        }

        // Arithmetic
        "add" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap({r1} + {r2});"), rd)
        }
        "addi" => {
            let (rd, rs, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap({rs} + {imm});"), rd)
        }
        "sub" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed({r1} - {r2});"), rd)
        }
        "neg" => {
            let (rd, r1) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed(0 - {r1});"), rd)
        }
        "mul" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== mul({r1}, {r2});"), rd)
        }
        "mulhu" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== mulhu({r1}, {r2});"), rd)
        }
        "divu" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=Z= divu({r1}, {r2});"), rd)
        }

        // bitwise
        "xor" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== xor({r1}, {r2});"), rd)
        }
        "xori" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== xor({r1}, {imm});"), rd)
        }
        "and" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== and({r1}, {r2});"), rd)
        }
        "andi" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== and({r1}, {imm});"), rd)
        }
        "or" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <== or({r1}, {r2});"), rd)
        }
        "ori" => {
            let (rd, r1, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <== or({r1}, {imm});"), rd)
        }
        "not" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <== wrap_signed(-{rs} - 1);"), rd)
        }

        // shift
        "slli" => {
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            only_if_no_write_to_zero_vec(
                if amount <= 16 {
                    vec![format!("{rd} <== wrap16({rs} * {});", 1 << amount)]
                } else {
                    vec![
                        format!("tmp1 <== wrap16({rs} * {});", 1 << 16),
                        format!("{rd} <== wrap16(tmp1 * {});", 1 << (amount - 16)),
                    ]
                },
                rd,
            )
        }
        "sll" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== and({r2}, 0x1f);"),
                    format!("{rd} <== shl({r1}, tmp1);"),
                ],
                rd,
            )
        }
        "srli" => {
            // logical shift right
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            only_if_no_write_to_zero(format!("{rd} <== shr({rs}, {amount});"), rd)
        }
        "srl" => {
            // logical shift right
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== and({r2}, 0x1f);"),
                    format!("{rd} <== shr({r1}, tmp1);"),
                ],
                rd,
            )
        }
        "srai" => {
            // arithmetic shift right
            // TODO see if we can implement this directly with a machine.
            // Now we are using the equivalence
            // a >>> b = (a >= 0 ? a >> b : ~(~a >> b))
            let (rd, rs, amount) = rri(args);
            assert!(amount <= 31);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({rs});"),
                    format!("tmp1 <== is_positive(0 - tmp1);"),
                    format!("tmp1 <=X= tmp1 * 0xffffffff;"),
                    // Here, tmp1 is the full bit mask if rs is negative
                    // and zero otherwise.
                    format!("{rd} <== xor(tmp1, {rs});"),
                    format!("{rd} <== shr({rd}, {amount});"),
                    format!("{rd} <== xor(tmp1, {rd});"),
                ],
                rd,
            )
        }

        // comparison
        "seqz" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_equal_zero({rs});"), rd)
        }
        "snez" => {
            let (rd, rs) = rr(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_not_equal_zero({rs});"), rd)
        }
        "slti" => {
            let (rd, rs, imm) = rri(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== to_signed({rs});"),
                    format!("{rd} <=Y= is_positive({} - tmp1);", imm as i32),
                ],
                rd,
            )
        }
        "slt" => {
            let (rd, r1, r2) = rrr(args);
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("tmp2 <== to_signed({r2});"),
                format!("{rd} <=Y= is_positive(tmp2 - tmp1);"),
            ]
        }
        "sltiu" => {
            let (rd, rs, imm) = rri(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_positive({imm} - {rs});"), rd)
        }
        "sltu" => {
            let (rd, r1, r2) = rrr(args);
            only_if_no_write_to_zero(format!("{rd} <=Y= is_positive({r2} - {r1});"), rd)
        }
        "sgtz" => {
            let (rd, rs) = rr(args);
            vec![
                format!("tmp1 <== to_signed({rs});"),
                format!("{rd} <=Y= is_positive(tmp1);"),
            ]
        }

        // branching
        "beq" => {
            let (r1, r2, label) = rrl(args);
            vec![format!("branch_if_zero {r1} - {r2}, {label};")]
        }
        "beqz" => {
            let (r1, label) = rl(args);
            vec![format!("branch_if_zero {r1}, {label};")]
        }
        "bgeu" => {
            let (r1, r2, label) = rrl(args);
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![format!("branch_if_positive {r1} - {r2} + 1, {label};")]
        }
        "bgez" => {
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("branch_if_positive tmp1 + 1, {label};"),
            ]
        }
        "bltu" => {
            let (r1, r2, label) = rrl(args);
            vec![format!("branch_if_positive {r2} - {r1}, {label};")]
        }
        "blt" => {
            let (r1, r2, label) = rrl(args);
            // Branch if r1 < r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("tmp2 <== to_signed({r2});"),
                format!("branch_if_positive tmp2 - tmp1, {label};"),
            ]
        }
        "bge" => {
            let (r1, r2, label) = rrl(args);
            // Branch if r1 >= r2 (signed).
            // TODO does this fulfill the input requirements for branch_if_positive?
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("tmp2 <== to_signed({r2});"),
                format!("branch_if_positive tmp1 - tmp2 + 1, {label};"),
            ]
        }
        "bltz" => {
            // branch if 2**31 <= r1 < 2**32
            let (r1, label) = rl(args);
            vec![format!("branch_if_positive {r1} - 2**31 + 1, {label};")]
        }

        "blez" => {
            // branch less or equal zero
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("branch_if_positive -tmp1 + 1, {label};"),
            ]
        }
        "bgtz" => {
            // branch if 0 < r1 < 2**31
            let (r1, label) = rl(args);
            vec![
                format!("tmp1 <== to_signed({r1});"),
                format!("branch_if_positive tmp1, {label};"),
            ]
        }
        "bne" => {
            let (r1, r2, label) = rrl(args);
            vec![format!("branch_if_nonzero {r1} - {r2}, {label};")]
        }
        "bnez" => {
            let (r1, label) = rl(args);
            vec![format!("branch_if_nonzero {r1}, {label};")]
        }

        // jump and call
        "j" => {
            if let [label] = args {
                vec![format!("jump {};", argument_to_escaped_symbol(label))]
            } else {
                panic!()
            }
        }
        "jr" => {
            let rs = r(args);
            vec![format!("jump_dyn {rs};")]
        }
        "jal" => {
            let (_rd, _label) = rl(args);
            todo!();
        }
        "jalr" => {
            // TODO there is also a form that takes more arguments
            let rs = r(args);
            vec![format!("jump_and_link_dyn {rs};")]
        }
        "call" => {
            // Depending on what symbol is called, the call is replaced by a
            // powdr asm call, or a call to a coprocessor if a special function
            // has been recognized.
            match args {
                [label] => match label {
                    Argument::Expression(Expression::Symbol(l)) => {
                        match try_coprocessor_substitution(l) {
                            Some(replacement) => vec![replacement.to_string()],
                            _ => vec![format!("call {};", argument_to_escaped_symbol(label))],
                        }
                    }
                    _ => vec![format!("call {};", argument_to_escaped_symbol(label))],
                },
                _ => panic!(),
            }
        }
        "ecall" => {
            assert!(args.is_empty());
            vec!["x10 <=X= ${ (\"input\", x10) };".to_string()]
        }
        "ebreak" => {
            assert!(args.is_empty());
            // This is using x0 on purpose, because we do not want to introduce
            // nondeterminism with this.
            vec!["x0 <=X= ${ (\"print_char\", x10) };\n".to_string()]
        }
        "tail" => {
            // Depending on what symbol is called, the tail call is replaced by a
            // powdr asm tail, or a call to a coprocessor if a special function
            // has been recognized.
            match args {
                [label] => match label {
                    Argument::Expression(Expression::Symbol(l)) => {
                        match try_coprocessor_substitution(l) {
                            Some(replacement) => vec![replacement.to_string()],
                            _ => vec![format!("tail {};", argument_to_escaped_symbol(label))],
                        }
                    }
                    _ => vec![format!("tail {};", argument_to_escaped_symbol(label))],
                },
                _ => panic!(),
            }
        }
        "ret" => {
            assert!(args.is_empty());
            vec!["ret;".to_string()]
        }

        // memory access
        "lw" => {
            let (rd, rs, off) = rro(args);
            // TODO we need to consider misaligned loads / stores
            only_if_no_write_to_zero_vec(
                vec![
                    format!("addr <== wrap({rs} + {off});"),
                    format!("{rd} <== mload();"),
                ],
                rd,
            )
        }
        "lb" => {
            // load byte and sign-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== wrap({rs} + {off});"),
                    "addr <== and(tmp1, 0xfffffffc);".to_string(),
                    "tmp2 <== and(tmp1, 0x3);".to_string(),
                    format!("{rd} <== mload();"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== sign_extend_byte({rd});"),
                ],
                rd,
            )
        }
        "lbu" => {
            // load byte and zero-extend. the memory is little-endian.
            let (rd, rs, off) = rro(args);
            only_if_no_write_to_zero_vec(
                vec![
                    format!("tmp1 <== wrap({rs} + {off});"),
                    "addr <== and(tmp1, 0xfffffffc);".to_string(),
                    "tmp2 <== and(tmp1, 0x3);".to_string(),
                    format!("{rd} <== mload();"),
                    format!("{rd} <== shr({rd}, 8 * tmp2);"),
                    format!("{rd} <== and({rd}, 0xff);"),
                ],
                rd,
            )
        }
        "sw" => {
            let (r1, r2, off) = rro(args);
            vec![
                format!("addr <== wrap({r2} + {off});"),
                format!("mstore {r1};"),
            ]
        }
        "sh" => {
            // store half word (two bytes)
            // TODO this code assumes it is at least aligned on
            // a two-byte boundary

            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1 <== wrap({rd} + {off});"),
                "addr <== and(tmp1, 0xfffffffc);".to_string(),
                "tmp2 <== and(tmp1, 0x3);".to_string(),
                "tmp1 <== mload();".to_string(),
                "tmp3 <== shl(0xffff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({rs}, 0xffff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                "mstore tmp1;".to_string(),
            ]
        }
        "sb" => {
            // store byte
            let (rs, rd, off) = rro(args);
            vec![
                format!("tmp1 <== wrap({rd} + {off});"),
                "addr <== and(tmp1, 0xfffffffc);".to_string(),
                "tmp2 <== and(tmp1, 0x3);".to_string(),
                "tmp1 <== mload();".to_string(),
                "tmp3 <== shl(0xff, 8 * tmp2);".to_string(),
                "tmp3 <== xor(tmp3, 0xffffffff);".to_string(),
                "tmp1 <== and(tmp1, tmp3);".to_string(),
                format!("tmp3 <== and({rs}, 0xff);"),
                "tmp3 <== shl(tmp3, 8 * tmp2);".to_string(),
                "tmp1 <== or(tmp1, tmp3);".to_string(),
                "mstore tmp1;".to_string(),
            ]
        }
        "nop" => vec![],
        "unimp" => vec!["fail;".to_string()],

        // Special instruction that is inserted to allow dynamic label references
        "load_dynamic" => {
            let (rd, label) = rl(args);
            only_if_no_write_to_zero(format!("{rd} <== load_label({label});"), rd)
        }

        _ => {
            panic!("Unknown instruction: {instr}");
        }
    }
}

#[cfg(test)]
mod test {
    use env_logger::filter;

    use super::*;

    #[test]
    fn test_remove_matching_and_next_integers() {
        assert_eq!(
            remove_matching_and_next([0, 1, 2, 0, 2, 0, 0, 3, 2, 1].iter(), |&&i| { i == 0 })
                .map(|i| *i)
                .collect::<Vec<_>>(),
            vec![2, 2, 1]
        );
    }

    #[test]
    fn test_remove_matching_and_next_strings() {
        assert_eq!(
            remove_matching_and_next(
                [
                    "croissant",
                    "pain au chocolat",
                    "chausson aux pommes",
                    "croissant" // corner case: if the label is at the end of the program
                ]
                .iter(),
                |&&s| { s == "croissant" }
            )
            .map(|s| { *s })
            .collect::<Vec<_>>(),
            vec!["chausson aux pommes"]
        );
    }
}
