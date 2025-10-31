
mkdir -p $1
RUST_LOG=debug ../../target/release/powdr_openvm prove --input $2 --metrics $1/metrics.json $(pwd) > $1/output.txt
./parse_logs.sh $1
