window.BENCHMARK_DATA = {
  "lastUpdate": 1733787984754,
  "repoUrl": "https://github.com/powdr-labs/powdr",
  "entries": {
    "Benchmarks": [
      {
        "commit": {
          "author": {
            "name": "powdr-labs",
            "username": "powdr-labs"
          },
          "committer": {
            "name": "powdr-labs",
            "username": "powdr-labs"
          },
          "id": "e5f4baf01bef5662267a3a9b4feb7bf565ac2e1f",
          "message": "Fix CI bench",
          "timestamp": "2024-12-09T16:21:48Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2211/commits/e5f4baf01bef5662267a3a9b4feb7bf565ac2e1f"
        },
        "date": 1733763622904,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6843,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 599,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1067,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28889,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 23989,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23983,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16962,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 781970,
            "range": "± 1836",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2692632,
            "range": "± 9092",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9452621,
            "range": "± 38393",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34630707,
            "range": "± 155185",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138283576,
            "range": "± 613516",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3220,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2537,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2502,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1368,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 68192,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 255448,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1043449,
            "range": "± 1268",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4895182,
            "range": "± 16148",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27891101,
            "range": "± 217249",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8408360975,
            "range": "± 52469453",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Thibaut Schaeffer",
            "username": "Schaeff",
            "email": "schaeffer.thibaut@gmail.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "d7170e13049919c0e5f28d07d5c559f0da047975",
          "message": "Fix CI bench (#2211)\n\nAll cargo.toml must have this `bench = false`. This will be caught in pr\ntests in the future.",
          "timestamp": "2024-12-09T22:40:52Z",
          "url": "https://github.com/powdr-labs/powdr/commit/d7170e13049919c0e5f28d07d5c559f0da047975"
        },
        "date": 1733785988316,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6803,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 600,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1091,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29441,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24467,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24467,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17259,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785384,
            "range": "± 732",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2725761,
            "range": "± 2820",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9538190,
            "range": "± 14519",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35085790,
            "range": "± 66522",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138692390,
            "range": "± 211142",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3268,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2500,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2492,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1373,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65918,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254744,
            "range": "± 263",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032452,
            "range": "± 1147",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4880336,
            "range": "± 4177",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27753508,
            "range": "± 59932",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8164990069,
            "range": "± 31976600",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "schaeffer.thibaut@gmail.com",
            "name": "Thibaut Schaeffer",
            "username": "Schaeff"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d7170e13049919c0e5f28d07d5c559f0da047975",
          "message": "Fix CI bench (#2211)\n\nAll cargo.toml must have this `bench = false`. This will be caught in pr\ntests in the future.",
          "timestamp": "2024-12-09T22:40:52Z",
          "tree_id": "70f5cd498e20c8956322b541b16dad6ebea7fe74",
          "url": "https://github.com/powdr-labs/powdr/commit/d7170e13049919c0e5f28d07d5c559f0da047975"
        },
        "date": 1733787984223,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6834,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 606,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1088,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29145,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24292,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24262,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17285,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 804124,
            "range": "± 1055",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2765167,
            "range": "± 5157",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9639840,
            "range": "± 11021",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35453851,
            "range": "± 35944",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140364979,
            "range": "± 548434",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3309,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2586,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2547,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65959,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252878,
            "range": "± 212",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1034091,
            "range": "± 2253",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4870764,
            "range": "± 5343",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27643068,
            "range": "± 61636",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8168780074,
            "range": "± 52094064",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}