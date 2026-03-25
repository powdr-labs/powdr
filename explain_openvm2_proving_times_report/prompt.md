Your task is to debug something and write a comprehensive report.

# Data:

All experiments were run on the `openvm-v2-integration` branch, using `openvm-riscv/scripts/run_guest_benches.sh`, on a GPU server.

## Pairing guest

Metrics: https://gist.githubusercontent.com/leonardoalt/f2b810eaf2d5d37491da491f496639d1/raw/551f8590bd372dc909db3d3da2235b6a8aff70d0/metrics_pairing_v2_combined.json
APC candidates: https://gist.githubusercontent.com/leonardoalt/73b455213ef4f987bf330559b714cb65/raw/521c6644ae3efa35d39b5e742520f9dddf2a7480/apc_candidates_pairing_v2.json

## Keccak guest

Metrics: https://gist.githubusercontent.com/leonardoalt/5da6435f9a87c62a2828cb3f30f82935/raw/3b07db8921ce529a937cfb273070c4653529c94a/metrics_keccak_v2_combined.json
APC candidates: https://gist.githubusercontent.com/leonardoalt/9712ad832858858358674749ca1b6e29/raw/b6e3b961b09073152032159fd8b1674b7142f0a5/apc_candidates_keccak_v2.json

Check out the OpenVM metrics viewer (openvm/metrics-viewer) and apc analyzer (autoprecompile-analyzer) for context. It contains information on the schema in the JSON files above.

`openvm/metrics-viewer/spec.py` should be useful to compute the data discussed below from these JSON files.

# Problem:

We are building autoprecompiles (APCs) and want to understand the relationship between statistics of the AIRs we add and the end-to-end proving time. In this task, let's focus on "STARK (excl. trace)".

I would expect this time to be a function of the number of trace cells, constraint instances and bus interaction messages. However, this is not what we are seeing.

On the Pairing guest:
- With 500 APCs we reduce the number of trace cells by 2.4x, the constraint instances by 2.3x and the bus interaction messages by 2.1x.
- Yet, the "STARK (excl. trace)" time goes UP by 1.3x.

On the Keccak guest:
- With 3 APCs, we reduce the number of trace cells by 9.6x, the constraint instances by 24.7x and the bus interaction messages by 6.5x.
- Yet, the "STARK (excl. trace)" time only goes down by 5x.
- It is interesting to compare this to the manually-written precompile:
  - Trace cells are reduced 8.9x, constraint instances by 7.1x and bus interaction messages by 31.4x.
  - The "STARK (excl. trace)" time goes down by 9.8x, which is more in line with what I would expect.

My suspicion is that the proving time does not only depend on the trace cells, constraint instances and bus interaction messages, but perhaps also on the number of AIRs, segments, constraints, bus interactions, columns, or something else. It would be great to understand this relationship better, so that we can build better APCs in the future.

I also want to understand whether this scaling behavior is inherent to the proof system, or whether this is an artifact of OpenVM's implementation. For example, they usually have 10s of AIRs, we might have 100s. It would not be surprising if they skipped optimizations would reduce the per-AIR overhead.

# Debugging steps:

1. Confirm my observations by loading the data, writing Python scripts to compute the numbers from the data and making sure that my observations are correct and that there is no bug in the viewers. If my observations are incorrect, stop and let me know about that.
2. Dive into the code of this repository and the OpenVM dependencies to understand what this data shows. For this task, use the `openvm-v2-integration` branch. (The viewers are more up-to-date on `main`, but `openvm-v2-integration` is using OpenVM 2.0, which is the version used to generate the data.)
3. Dive into the OpenVM 2.0 code (make sure to check out the `openvm-v2-integration` branch) to understand what the GPU prover does in each step. Analyze the scaling behavior of the steps taking significant time depending on the statistics of the AIRs (e.g. number of trace cells, constraint instances, bus interaction messages, but also number of AIRs, segments, constraints, bus interactions, columns, etc.). Try to understand whether the scaling behavior we are seeing is inherent to the proof system or whether it is an artifact of OpenVM's implementation.
4. Write a comprehensive report in a Markdown file. If you want to visualize something or compute data, write python scripts and include the results in the report. The report should contain:
  - A recap of all of the relevant proving steps. Include a description of what they do (e.g. what exactly is "Round 0"?), some pseudo-code if relevant and the expected scaling behavior of each step depending on the AIR statistics.
  - Commentary on how the statistics of our APCs affect the proving time, and whether this is expected or something that can be optimized in the implementation.
  - A cost function. Ideally we can predict the STARK proving time given the AIR statistics.

Put any files you generate in a folder called `explain_openvm2_proving_times_report`. You can create subfolders in there if you want. Make sure to include the code you used to generate any insights, so that I can reproduce your results.