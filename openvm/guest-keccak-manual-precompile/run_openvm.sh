cargo openvm build
cargo openvm keygen
# Start with 0x01, then little-endian encoding of the number of hashes.
# hex(10000) = 0x2710 -> little-endian 0x1027
# hex(256) = 0x0100 -> little-endian 0x0001
RUST_LOG=debug cargo openvm prove app --input "0x010001000000000000" > $1.txt

echo "\n\nTrace cells:"
grep "Cells =" $1.txt

echo "\n\nSegment proving times:"
grep "prove_segment" $1.txt