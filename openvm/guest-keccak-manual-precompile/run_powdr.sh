
mkdir -p $1
RUST_LOG=debug ../../target/release/powdr_openvm prove --input 10000 $(pwd) > $1/output.txt
./parse_logs.sh $1
