cargo openvm build
cargo openvm keygen
# Start with 0x01, then little-endian encoding of the number of hashes.
# hex(10000) = 0x2710 -> little-endian 0x1027
# hex(256) = 0x0100 -> little-endian 0x0001
mkdir -p $1
RUST_LOG=debug cargo openvm prove app --input "0x010001000000000000" > $1/output.txt

grep "Cells =" $1/output.txt > $1/cells.txt
grep "prove_segment" $1/output.txt > $1/prove_times.txt