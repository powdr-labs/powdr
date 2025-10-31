#cargo openvm build
#cargo openvm keygen
# Start with 0x01, then little-endian encoding of the number of hashes.
# hex(10000) = 0x2710 -> little-endian 0x1027
RUST_LOG=debug cargo openvm prove app --input "0x010102700000000000" > output.txt

echo "\n\nTrace cells:"
grep "Cells =" output.txt

echo "\n\nSegment proving times:"
grep "prove_segment" output.txt