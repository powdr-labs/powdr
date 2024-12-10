window.BENCHMARK_DATA = {
  "lastUpdate": 1733852416234,
  "repoUrl": "https://github.com/powdr-labs/powdr",
  "entries": {
    "Benchmarks": [
      {
        "commit": {
          "author": {
            "email": "chris@ethereum.org",
            "name": "chriseth",
            "username": "chriseth"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0a12ffe1884b9fa39585bba18618baf2d26b166f",
          "message": "Remove dependencies on build (#2217)\n\nThese runs make no use of the artefacts created in build and do a full\nre-build, so we might as well run them from the start.",
          "timestamp": "2024-12-10T13:28:44Z",
          "tree_id": "82f228ff2e5de67c94dd4d250fca00c3d961284b",
          "url": "https://github.com/powdr-labs/powdr/commit/0a12ffe1884b9fa39585bba18618baf2d26b166f"
        },
        "date": 1733840416560,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6946,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 599,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1106,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29190,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24529,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24437,
            "range": "± 349",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17388,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 783889,
            "range": "± 1113",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2707649,
            "range": "± 2204",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9454093,
            "range": "± 9186",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34895377,
            "range": "± 48970",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137826107,
            "range": "± 296266",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3181,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2502,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2466,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65958,
            "range": "± 167",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254512,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032685,
            "range": "± 1627",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4883911,
            "range": "± 3773",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27696183,
            "range": "± 17587",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8085657272,
            "range": "± 39880323",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "192675f3d4f396d8a49f13733ccfaf7c540d033d",
          "message": "Use arith-memory in RISCV",
          "timestamp": "2024-12-10T12:27:33Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2199/commits/192675f3d4f396d8a49f13733ccfaf7c540d033d"
        },
        "date": 1733840451463,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6785,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 596,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1074,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28943,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24314,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24294,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17180,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 784201,
            "range": "± 1172",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2707399,
            "range": "± 3701",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9456314,
            "range": "± 13185",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34742470,
            "range": "± 40148",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137915594,
            "range": "± 163445",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3148,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2462,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2452,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1359,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66302,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254036,
            "range": "± 329",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032186,
            "range": "± 1231",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4884420,
            "range": "± 4782",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27821008,
            "range": "± 42753",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8107346584,
            "range": "± 14354288",
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
          "id": "670802aaee125602269e7ef0e0f5422119598e69",
          "message": "Fix book deploy again (#2218)\n\nTurns out `git worktree` does not track the remote branch, so the\nbenchmarks are not there.\nRevert to what we did before, and explicitly restore the benchmarks from\nthe remote `gh-pages` branch.",
          "timestamp": "2024-12-10T14:09:38Z",
          "url": "https://github.com/powdr-labs/powdr/commit/670802aaee125602269e7ef0e0f5422119598e69"
        },
        "date": 1733840740595,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6926,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 597,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1068,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29024,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24212,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24193,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17006,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 780859,
            "range": "± 1366",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2699338,
            "range": "± 2649",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9393402,
            "range": "± 10898",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34703373,
            "range": "± 95989",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137272431,
            "range": "± 103376",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3117,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2457,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2492,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1360,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65658,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251829,
            "range": "± 437",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028092,
            "range": "± 1097",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4872321,
            "range": "± 2677",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27607209,
            "range": "± 14774",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8128908854,
            "range": "± 63333675",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "33b9df440608e758f1000cd52a2b209a9f8ffe46",
          "message": "Fix Plonky3 proofs for removed machines",
          "timestamp": "2024-12-10T14:04:35Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2221/commits/33b9df440608e758f1000cd52a2b209a9f8ffe46"
        },
        "date": 1733841086390,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6820,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 590,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1063,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28780,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24030,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23991,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16952,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 783053,
            "range": "± 629",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2716059,
            "range": "± 2291",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9496616,
            "range": "± 14580",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34858305,
            "range": "± 69607",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138102849,
            "range": "± 155048",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3124,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2451,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2453,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1373,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65931,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252344,
            "range": "± 247",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028834,
            "range": "± 919",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4892583,
            "range": "± 5328",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27601338,
            "range": "± 22526",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8193816946,
            "range": "± 27359818",
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
          "distinct": false,
          "id": "670802aaee125602269e7ef0e0f5422119598e69",
          "message": "Fix book deploy again (#2218)\n\nTurns out `git worktree` does not track the remote branch, so the\nbenchmarks are not there.\nRevert to what we did before, and explicitly restore the benchmarks from\nthe remote `gh-pages` branch.",
          "timestamp": "2024-12-10T14:09:38Z",
          "tree_id": "bb3d3857b965109b4e4a9bb68d505dee34587967",
          "url": "https://github.com/powdr-labs/powdr/commit/670802aaee125602269e7ef0e0f5422119598e69"
        },
        "date": 1733842965112,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6860,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 593,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1097,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28906,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24094,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24128,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17032,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786828,
            "range": "± 1317",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2724686,
            "range": "± 4274",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9487322,
            "range": "± 25646",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35002965,
            "range": "± 159836",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139679244,
            "range": "± 633345",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3117,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2450,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2440,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1360,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65121,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251539,
            "range": "± 156",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1025114,
            "range": "± 1076",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4894555,
            "range": "± 18968",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27886722,
            "range": "± 203357",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8358911831,
            "range": "± 47137776",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Georg Wiese",
            "username": "georgwiese",
            "email": "georgwiese@gmail.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "11c3c7023c8febdbb55cafcbd76003bd9f93ac58",
          "message": "Fix Plonky3 proofs for removed machines (#2221)\n\nWe removed them in the proof, but still ran second-stage witgen for it.",
          "timestamp": "2024-12-10T14:50:14Z",
          "url": "https://github.com/powdr-labs/powdr/commit/11c3c7023c8febdbb55cafcbd76003bd9f93ac58"
        },
        "date": 1733843172929,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7112,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 593,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1043,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29058,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24266,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24140,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17028,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792368,
            "range": "± 1604",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2741509,
            "range": "± 4898",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9601950,
            "range": "± 29294",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35349442,
            "range": "± 199344",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140483491,
            "range": "± 500117",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3156,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2482,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2448,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1358,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65603,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252742,
            "range": "± 329",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027643,
            "range": "± 1025",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4882232,
            "range": "± 8196",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28062539,
            "range": "± 226821",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8563080642,
            "range": "± 64921889",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "georgwiese@gmail.com",
            "name": "Georg Wiese",
            "username": "georgwiese"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "11c3c7023c8febdbb55cafcbd76003bd9f93ac58",
          "message": "Fix Plonky3 proofs for removed machines (#2221)\n\nWe removed them in the proof, but still ran second-stage witgen for it.",
          "timestamp": "2024-12-10T14:50:14Z",
          "tree_id": "ec8a7c6fd1de246be8fc15f9b58db2fb0fae2fba",
          "url": "https://github.com/powdr-labs/powdr/commit/11c3c7023c8febdbb55cafcbd76003bd9f93ac58"
        },
        "date": 1733845315646,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6902,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 609,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1080,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29363,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24550,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24457,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17308,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792151,
            "range": "± 1530",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2726890,
            "range": "± 4132",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9605808,
            "range": "± 39812",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35375025,
            "range": "± 160219",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141070332,
            "range": "± 984061",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3129,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2467,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2447,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1360,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65990,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254335,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1034316,
            "range": "± 4964",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4926834,
            "range": "± 22926",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28295362,
            "range": "± 383996",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8700422324,
            "range": "± 74187665",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "0a2099e7c5e5714ba026ab4fe715663e4d74dee9",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T15:27:05Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/0a2099e7c5e5714ba026ab4fe715663e4d74dee9"
        },
        "date": 1733846261955,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6773,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 616,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1043,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 31093,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 26078,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 26015,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18391,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 797890,
            "range": "± 1364",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2767754,
            "range": "± 4526",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9658930,
            "range": "± 11845",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35338683,
            "range": "± 153814",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140285564,
            "range": "± 387460",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3148,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2470,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2462,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1359,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65554,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251086,
            "range": "± 159",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028937,
            "range": "± 1334",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4860606,
            "range": "± 5227",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27626228,
            "range": "± 71084",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8093755162,
            "range": "± 68384782",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "5014950ae8fe812a160581718a7c0b04c52c495b",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T15:27:05Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/5014950ae8fe812a160581718a7c0b04c52c495b"
        },
        "date": 1733847868523,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6822,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 699,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1058,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29045,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24285,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24260,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17111,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 788714,
            "range": "± 1125",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2715407,
            "range": "± 4296",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9529092,
            "range": "± 33226",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35046022,
            "range": "± 156792",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141582231,
            "range": "± 1064401",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3497,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2514,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2472,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1360,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65300,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250506,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024135,
            "range": "± 1050",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4862130,
            "range": "± 9893",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28007293,
            "range": "± 99052",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8567617485,
            "range": "± 81738345",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "2a6a10ee65e259ee0e05f6e09c6abeb4be5b94d4",
          "message": "Refactorings around `process_lookup_direct`",
          "timestamp": "2024-12-10T15:27:05Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2209/commits/2a6a10ee65e259ee0e05f6e09c6abeb4be5b94d4"
        },
        "date": 1733847978836,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7003,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 612,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1098,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29421,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24534,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24448,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17323,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 771257,
            "range": "± 607",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2656028,
            "range": "± 2622",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9381022,
            "range": "± 12246",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34231894,
            "range": "± 30421",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 136669839,
            "range": "± 105912",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3127,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2461,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2449,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1359,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65313,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251758,
            "range": "± 293",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031035,
            "range": "± 1399",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4857603,
            "range": "± 3125",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27650171,
            "range": "± 21851",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 7994134808,
            "range": "± 38354678",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Georg Wiese",
            "username": "georgwiese",
            "email": "georgwiese@gmail.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "0180542559db8ea6811c2b57d5b2ca292ea7ea69",
          "message": "Refactorings around `process_lookup_direct` (#2209)\n\nThis PR refactors a few things:\n- `process_lookup_direct` no longer has a default implementation.\nEventually, we want all machines to implement it, so I figured it would\nbe better to explicitly panic in each machine.\n- Refactored the implementation of\n`FixedLookupMachine::process_plookup`, pulling some stuff out into a new\n`CallerData` struct. This is similar to what @chriseth has done on\n[`call_jit_from_block`](https://github.com/powdr-labs/powdr/compare/main...call_jit_from_block),\nsee the comment below.\n- As a first test, I implemented `process_lookup_direct` for the\n\"large\"-field memory machine (and `process_plookup` by wrapping\n`process_lookup_direct`)",
          "timestamp": "2024-12-10T16:44:15Z",
          "url": "https://github.com/powdr-labs/powdr/commit/0180542559db8ea6811c2b57d5b2ca292ea7ea69"
        },
        "date": 1733850030986,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6948,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 675,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1212,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28723,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24025,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24033,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17008,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 900069,
            "range": "± 945",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 3108252,
            "range": "± 2877",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 10867962,
            "range": "± 19871",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 39435248,
            "range": "± 182221",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 154032830,
            "range": "± 430375",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3182,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2496,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2499,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1381,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65899,
            "range": "± 153",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252882,
            "range": "± 237",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029355,
            "range": "± 1201",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4850843,
            "range": "± 3422",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27680141,
            "range": "± 139311",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8124413452,
            "range": "± 58727336",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "e3e1fdb5035e6c43248799c6914a795f371460e9",
          "message": "witgen inference.",
          "timestamp": "2024-12-10T15:27:05Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/e3e1fdb5035e6c43248799c6914a795f371460e9"
        },
        "date": 1733850273981,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7082,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 615,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1107,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29306,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24491,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24470,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17297,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786735,
            "range": "± 1343",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2710580,
            "range": "± 5452",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9537467,
            "range": "± 32851",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35112377,
            "range": "± 152844",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141420686,
            "range": "± 1083512",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3118,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2443,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2452,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1370,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66175,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254381,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1041433,
            "range": "± 983",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4909351,
            "range": "± 11011",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28427496,
            "range": "± 205965",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8647094773,
            "range": "± 48960392",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "georgwiese@gmail.com",
            "name": "Georg Wiese",
            "username": "georgwiese"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0180542559db8ea6811c2b57d5b2ca292ea7ea69",
          "message": "Refactorings around `process_lookup_direct` (#2209)\n\nThis PR refactors a few things:\n- `process_lookup_direct` no longer has a default implementation.\nEventually, we want all machines to implement it, so I figured it would\nbe better to explicitly panic in each machine.\n- Refactored the implementation of\n`FixedLookupMachine::process_plookup`, pulling some stuff out into a new\n`CallerData` struct. This is similar to what @chriseth has done on\n[`call_jit_from_block`](https://github.com/powdr-labs/powdr/compare/main...call_jit_from_block),\nsee the comment below.\n- As a first test, I implemented `process_lookup_direct` for the\n\"large\"-field memory machine (and `process_plookup` by wrapping\n`process_lookup_direct`)",
          "timestamp": "2024-12-10T16:44:15Z",
          "tree_id": "3c3050b6316c6db5af054b636680e99dbfb8e8f4",
          "url": "https://github.com/powdr-labs/powdr/commit/0180542559db8ea6811c2b57d5b2ca292ea7ea69"
        },
        "date": 1733852047972,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6848,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 593,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1069,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29045,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24336,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24954,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17278,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794429,
            "range": "± 2244",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2749887,
            "range": "± 4172",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9557325,
            "range": "± 19595",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34950155,
            "range": "± 50678",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139339462,
            "range": "± 261637",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3118,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2482,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2439,
            "range": "± 5",
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
            "value": 66209,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250827,
            "range": "± 266",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1020246,
            "range": "± 1272",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4851170,
            "range": "± 7651",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27600525,
            "range": "± 158186",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8132598043,
            "range": "± 57065964",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "8226810aafc3646e17da94f287dde5406c1d984a",
          "message": "riscv-executor: handle machines that have been optimized away",
          "timestamp": "2024-12-10T17:18:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2222/commits/8226810aafc3646e17da94f287dde5406c1d984a"
        },
        "date": 1733852415728,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6798,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 590,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1088,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29231,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24519,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24428,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17329,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 781199,
            "range": "± 764",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2703162,
            "range": "± 3144",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9427656,
            "range": "± 13342",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34768873,
            "range": "± 85096",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138600524,
            "range": "± 435092",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3127,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2469,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1361,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65296,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251140,
            "range": "± 506",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029373,
            "range": "± 1121",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4861902,
            "range": "± 2810",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27597517,
            "range": "± 76464",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8131295941,
            "range": "± 37691411",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}