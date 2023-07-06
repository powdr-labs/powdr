set -e

cargo run pil -f test_data/asm/secondary_state_machine.asm --field bn254
cargo run prove secondary_state_machine.pil --field bn254 --backend halo2-mock
cargo run export-csv secondary_state_machine.pil --field bn254
csvtool namedcol -u TAB A,B,binary_RESET,binary_A,binary_B,binary_A_byte,binary_B_byte columns.csv | head