set -e

cargo run pil -f test_data/asm/secondary_state_machine_add2.asm --field bn254
cargo run export-csv secondary_state_machine_add2.pil --field bn254
csvtool namedcol -u TAB A,B,add_two_RESET,add_two_state,add_two_input columns.csv | head -n 20
cargo run prove secondary_state_machine_add2.pil --field bn254 --backend halo2-mock 2>&1 | head -n 30