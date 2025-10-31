### Prerequisites:
# From project root:
# cargo build -r --bin powdr_openvm 
# python3 -m venv .venv
# source .venv/bin/activate
# pip install -r openvm/scripts/requirements.txt

mkdir -p $1
RUST_LOG=debug ../../target/release/powdr_openvm prove --input $2 --metrics $1/metrics.json $(pwd) > $1/output.txt
python ../scripts/basic_metrics.py --csv $1/metrics.json > $1/metrics.csv

./parse_logs.sh $1
