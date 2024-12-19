window.BENCHMARK_DATA = {
  "lastUpdate": 1734630926362,
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
          "id": "1070ce9f35f241cf1422090f31eaf1a964e185c1",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/1070ce9f35f241cf1422090f31eaf1a964e185c1"
        },
        "date": 1733867500663,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6992,
            "range": "± 146",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 602,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1095,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29163,
            "range": "± 667",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24465,
            "range": "± 803",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24240,
            "range": "± 295",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17239,
            "range": "± 199",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 809356,
            "range": "± 14115",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2810556,
            "range": "± 56851",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9943056,
            "range": "± 168297",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36547343,
            "range": "± 738159",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 145662435,
            "range": "± 2792554",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3133,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2457,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2500,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1377,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66434,
            "range": "± 804",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253708,
            "range": "± 2778",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1038743,
            "range": "± 17060",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4935954,
            "range": "± 60494",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28722311,
            "range": "± 484488",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8905121057,
            "range": "± 132840153",
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
          "id": "aa91ce3ded82584f74ea90aeacf5032174315fbb",
          "message": "witgen inference.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/aa91ce3ded82584f74ea90aeacf5032174315fbb"
        },
        "date": 1733869034512,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6946,
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
            "value": 1062,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29269,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24448,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24777,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17211,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 784069,
            "range": "± 840",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2709876,
            "range": "± 4912",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9462919,
            "range": "± 28230",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34755597,
            "range": "± 55975",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138068649,
            "range": "± 282523",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3153,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2444,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2441,
            "range": "± 3",
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
            "value": 65794,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252510,
            "range": "± 284",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031902,
            "range": "± 1074",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4874113,
            "range": "± 6892",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27929252,
            "range": "± 181440",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8228646763,
            "range": "± 62235781",
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
          "id": "e6843ba1ea3671a7c793b40aae4e6b690285e349",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/e6843ba1ea3671a7c793b40aae4e6b690285e349"
        },
        "date": 1733869383325,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6964,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 630,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1086,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28886,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24150,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24048,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17105,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 788695,
            "range": "± 1089",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2733969,
            "range": "± 3096",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9564649,
            "range": "± 11050",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34944178,
            "range": "± 81546",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138822521,
            "range": "± 152179",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3173,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2510,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2469,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65964,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253636,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030681,
            "range": "± 1170",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4869144,
            "range": "± 4434",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27637260,
            "range": "± 34096",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8085384291,
            "range": "± 18030517",
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
          "id": "eb62678bd00f6cc4083367ef86f32e1765b2312f",
          "message": "witgen inference.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/eb62678bd00f6cc4083367ef86f32e1765b2312f"
        },
        "date": 1733870501651,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6864,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 585,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1090,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29017,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24234,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24210,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17146,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 780696,
            "range": "± 1386",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2682337,
            "range": "± 2084",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9401869,
            "range": "± 43191",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34518104,
            "range": "± 74593",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138007852,
            "range": "± 369781",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3132,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2453,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2448,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65935,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251497,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026186,
            "range": "± 928",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4903704,
            "range": "± 42636",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27994275,
            "range": "± 101196",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8384164136,
            "range": "± 50323224",
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
          "id": "53dddb47c6ef599e793dca6e7d01bf5cb41bcb34",
          "message": "witgen inference.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/53dddb47c6ef599e793dca6e7d01bf5cb41bcb34"
        },
        "date": 1733908642541,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6919,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1068,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28839,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24116,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24078,
            "range": "± 176",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17037,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790223,
            "range": "± 1700",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2728802,
            "range": "± 8949",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9495569,
            "range": "± 19358",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34885126,
            "range": "± 101185",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139222307,
            "range": "± 317754",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3130,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2451,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2455,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1369,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66410,
            "range": "± 210",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 258066,
            "range": "± 426",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1055704,
            "range": "± 3594",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4892673,
            "range": "± 11940",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27649832,
            "range": "± 215939",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8125199649,
            "range": "± 30392651",
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
          "id": "67e238e7726a6ba2bb4f75bafb28d8969270a3d0",
          "message": "witgen inference.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/67e238e7726a6ba2bb4f75bafb28d8969270a3d0"
        },
        "date": 1733912379668,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6984,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 576,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1055,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28672,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 23955,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23941,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16874,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 783514,
            "range": "± 1277",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2706418,
            "range": "± 3614",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9476639,
            "range": "± 26654",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34826178,
            "range": "± 107254",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138301311,
            "range": "± 267051",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3123,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2448,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1363,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65918,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253528,
            "range": "± 214",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031295,
            "range": "± 1218",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4882176,
            "range": "± 6637",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27833132,
            "range": "± 105169",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8515955531,
            "range": "± 60698934",
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
          "id": "57da9452218c6bbf1dbef10c8c619d676ed3817e",
          "message": "Use arith-memory in RISCV",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2199/commits/57da9452218c6bbf1dbef10c8c619d676ed3817e"
        },
        "date": 1733913490627,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6990,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1093,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29439,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24633,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24659,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17469,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791267,
            "range": "± 3702",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2743625,
            "range": "± 5903",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9590665,
            "range": "± 13231",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35156565,
            "range": "± 73714",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139333335,
            "range": "± 400542",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3136,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2457,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2460,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66557,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253801,
            "range": "± 184",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1037939,
            "range": "± 1020",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4888182,
            "range": "± 3422",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27722900,
            "range": "± 52353",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8133117144,
            "range": "± 17016444",
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
          "id": "cad7f37b3b3e02686aab364bc7482b89310bf707",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/cad7f37b3b3e02686aab364bc7482b89310bf707"
        },
        "date": 1733915892928,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6832,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 599,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1053,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29257,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24383,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24297,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17105,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 789599,
            "range": "± 1137",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2709044,
            "range": "± 3708",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9424252,
            "range": "± 22784",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34571270,
            "range": "± 181474",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138626634,
            "range": "± 520676",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3143,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2468,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2449,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65370,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250842,
            "range": "± 189",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024417,
            "range": "± 902",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885528,
            "range": "± 19379",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27817144,
            "range": "± 174479",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8369665079,
            "range": "± 51709188",
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
          "id": "84c5d7575d52dc71905190a6e92c8a2b134bb928",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/84c5d7575d52dc71905190a6e92c8a2b134bb928"
        },
        "date": 1733919349948,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6730,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 573,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1040,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29492,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24494,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24464,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17311,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794693,
            "range": "± 733",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2745211,
            "range": "± 3148",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9627545,
            "range": "± 14336",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35196482,
            "range": "± 68460",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139661535,
            "range": "± 227692",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3133,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2464,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2448,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65869,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252155,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026857,
            "range": "± 1074",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4855905,
            "range": "± 3774",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27532762,
            "range": "± 17906",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 7974903347,
            "range": "± 24624152",
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
          "id": "a21ca0c84f5a496d77622d1483b1cf12e6a8a9a5",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/a21ca0c84f5a496d77622d1483b1cf12e6a8a9a5"
        },
        "date": 1733921195590,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7100,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 589,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1103,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29483,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24626,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24529,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17291,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 805029,
            "range": "± 3933",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2758331,
            "range": "± 4919",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9640749,
            "range": "± 14302",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35294814,
            "range": "± 43076",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139762146,
            "range": "± 162927",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3146,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2457,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1359,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65742,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253032,
            "range": "± 253",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031183,
            "range": "± 1021",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4878841,
            "range": "± 4472",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27579164,
            "range": "± 32664",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8029083668,
            "range": "± 35739995",
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
          "id": "af614130ac955fe011d9812718eb5bbde2cb8457",
          "message": "Add support for constant column in STWO backend",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2112/commits/af614130ac955fe011d9812718eb5bbde2cb8457"
        },
        "date": 1733921510976,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6816,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 584,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1078,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29240,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24376,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24299,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17238,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790032,
            "range": "± 910",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2730916,
            "range": "± 2823",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9583956,
            "range": "± 11620",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35123782,
            "range": "± 54490",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139354229,
            "range": "± 180956",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3215,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2462,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1378,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65824,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252224,
            "range": "± 168",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035987,
            "range": "± 1175",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4890308,
            "range": "± 2814",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27676254,
            "range": "± 15839",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8065243786,
            "range": "± 35995453",
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
          "id": "2b4d6b688e03a314526ced48cd277fbed91c319c",
          "message": "[WIP] RISCV executor trace",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2207/commits/2b4d6b688e03a314526ced48cd277fbed91c319c"
        },
        "date": 1733925023820,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7616,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 704,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1062,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30120,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25155,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25071,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17722,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 808527,
            "range": "± 1586",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2791858,
            "range": "± 4801",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9769054,
            "range": "± 20234",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35746236,
            "range": "± 91804",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141674739,
            "range": "± 948073",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3286,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2591,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2550,
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
            "value": 67249,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253499,
            "range": "± 184",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032404,
            "range": "± 1482",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4905880,
            "range": "± 21904",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27930416,
            "range": "± 99318",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8352919264,
            "range": "± 70145074",
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
          "id": "479a96d5fe973a35e2bf82c415a0633681b88165",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-10T20:23:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/479a96d5fe973a35e2bf82c415a0633681b88165"
        },
        "date": 1733925944743,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6909,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1051,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29202,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24437,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24395,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17339,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 781401,
            "range": "± 1314",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2708347,
            "range": "± 3537",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9494831,
            "range": "± 27912",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34750249,
            "range": "± 68379",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137298704,
            "range": "± 758539",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3127,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2500,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2454,
            "range": "± 3",
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
            "value": 65443,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251181,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031115,
            "range": "± 853",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4873408,
            "range": "± 6190",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27614964,
            "range": "± 115397",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8167146128,
            "range": "± 100570163",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Leo",
            "username": "leonardoalt",
            "email": "leo@powdrlabs.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "4f1aa4a5f3879e974f8f668b728a8f7eb656b528",
          "message": "Use arith-memory in RISCV (#2199)",
          "timestamp": "2024-12-11T13:52:34Z",
          "url": "https://github.com/powdr-labs/powdr/commit/4f1aa4a5f3879e974f8f668b728a8f7eb656b528"
        },
        "date": 1733926118227,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6764,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1030,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30115,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24656,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24590,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17445,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 783697,
            "range": "± 3496",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2701887,
            "range": "± 7363",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9451000,
            "range": "± 40230",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34702673,
            "range": "± 160801",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138424800,
            "range": "± 520341",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3142,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2534,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2455,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1366,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65353,
            "range": "± 208",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250885,
            "range": "± 2469",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027114,
            "range": "± 5995",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4890549,
            "range": "± 33422",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27744278,
            "range": "± 203185",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8338234447,
            "range": "± 59173764",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "leo@powdrlabs.com",
            "name": "Leo",
            "username": "leonardoalt"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4f1aa4a5f3879e974f8f668b728a8f7eb656b528",
          "message": "Use arith-memory in RISCV (#2199)",
          "timestamp": "2024-12-11T13:52:34Z",
          "tree_id": "c7c08c44429b486f3e1e65dfcf64de0dba717680",
          "url": "https://github.com/powdr-labs/powdr/commit/4f1aa4a5f3879e974f8f668b728a8f7eb656b528"
        },
        "date": 1733927612294,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6777,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 571,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1044,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28958,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24318,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24240,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17150,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798226,
            "range": "± 2612",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2756130,
            "range": "± 2331",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9763550,
            "range": "± 44170",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35974989,
            "range": "± 248911",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 143369682,
            "range": "± 1758683",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3157,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2474,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2465,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1368,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66472,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251308,
            "range": "± 153",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029249,
            "range": "± 1332",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4893876,
            "range": "± 28253",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28659553,
            "range": "± 182488",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9059597557,
            "range": "± 109381449",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "t",
            "username": "MuhtasimTanmoy",
            "email": "mtanmoy5086@gmail.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "56d43c48b94d1e57b3504da5191c4a5dcb4dcc7e",
          "message": "cargo-powdr gitignore add on template (#2213)\n\nhttps://github.com/powdr-labs/powdr/issues/2210",
          "timestamp": "2024-12-11T14:48:04Z",
          "url": "https://github.com/powdr-labs/powdr/commit/56d43c48b94d1e57b3504da5191c4a5dcb4dcc7e"
        },
        "date": 1733929419387,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6802,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 576,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1081,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29384,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24595,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24521,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17230,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 781565,
            "range": "± 960",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2691559,
            "range": "± 2712",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9417262,
            "range": "± 8204",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34600923,
            "range": "± 41697",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 136993715,
            "range": "± 133067",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3494,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2459,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2457,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65973,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252922,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1034834,
            "range": "± 978",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4879814,
            "range": "± 4496",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27654465,
            "range": "± 22599",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8099162120,
            "range": "± 32285667",
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
          "id": "f446e46182796b34c821b6087080ae85ab8ffa5d",
          "message": "Add support for constant column in STWO backend",
          "timestamp": "2024-12-11T14:17:05Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2112/commits/f446e46182796b34c821b6087080ae85ab8ffa5d"
        },
        "date": 1733929684550,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6768,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1056,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28801,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24641,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24152,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17130,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 802367,
            "range": "± 1596",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2768950,
            "range": "± 5129",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9749418,
            "range": "± 42539",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35727786,
            "range": "± 215616",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142213134,
            "range": "± 747953",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3116,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2455,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2440,
            "range": "± 6",
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
            "value": 65795,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252426,
            "range": "± 325",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027952,
            "range": "± 1049",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4907013,
            "range": "± 19592",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28098290,
            "range": "± 239473",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8680437473,
            "range": "± 34458983",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "mtanmoy5086@gmail.com",
            "name": "t",
            "username": "MuhtasimTanmoy"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "56d43c48b94d1e57b3504da5191c4a5dcb4dcc7e",
          "message": "cargo-powdr gitignore add on template (#2213)\n\nhttps://github.com/powdr-labs/powdr/issues/2210",
          "timestamp": "2024-12-11T14:48:04Z",
          "tree_id": "263829c688edac6d59070ee6e10a9bdcf27a810c",
          "url": "https://github.com/powdr-labs/powdr/commit/56d43c48b94d1e57b3504da5191c4a5dcb4dcc7e"
        },
        "date": 1733930955256,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6942,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 572,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1051,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29341,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24566,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24509,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17423,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786881,
            "range": "± 949",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2711320,
            "range": "± 4486",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9551580,
            "range": "± 30120",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34850900,
            "range": "± 74509",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138132465,
            "range": "± 202563",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3125,
            "range": "± 6",
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
            "value": 2457,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65548,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254010,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026248,
            "range": "± 859",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4875140,
            "range": "± 14772",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27652815,
            "range": "± 114851",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8270525389,
            "range": "± 84237535",
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
          "id": "0557c6059583403875d627f9bb5ea6c968c43180",
          "message": "Add support for constant column in STWO backend",
          "timestamp": "2024-12-11T15:13:35Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2112/commits/0557c6059583403875d627f9bb5ea6c968c43180"
        },
        "date": 1733931815775,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6932,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 588,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1059,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29590,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24562,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24560,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17324,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 797354,
            "range": "± 1262",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2735583,
            "range": "± 5614",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9594165,
            "range": "± 35668",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35310689,
            "range": "± 182100",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140167213,
            "range": "± 817173",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3307,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2584,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2523,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1373,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65830,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 255144,
            "range": "± 294",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033883,
            "range": "± 1387",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4924157,
            "range": "± 27829",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27993876,
            "range": "± 223611",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8613457003,
            "range": "± 89918067",
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
          "id": "81063fb8a5e69f848d5df706689b30cdd5742be7",
          "message": "pilopt: equal-constrained witness columns removal",
          "timestamp": "2024-12-11T15:13:35Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2224/commits/81063fb8a5e69f848d5df706689b30cdd5742be7"
        },
        "date": 1733933089375,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6825,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 598,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1065,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28885,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24202,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24204,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17169,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 783516,
            "range": "± 835",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2715460,
            "range": "± 2927",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9465747,
            "range": "± 22941",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34801157,
            "range": "± 56693",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138104254,
            "range": "± 171783",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3149,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2459,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2469,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1376,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66278,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253632,
            "range": "± 194",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033363,
            "range": "± 1283",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4881089,
            "range": "± 4360",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27705315,
            "range": "± 37530",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8089663985,
            "range": "± 43465302",
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
          "id": "b3dc67843c9f286018dc8331978484c471a2e9c1",
          "message": "pilopt: optimize until fixpoint",
          "timestamp": "2024-12-11T15:13:35Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2225/commits/b3dc67843c9f286018dc8331978484c471a2e9c1"
        },
        "date": 1733934891791,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6880,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 578,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1048,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28966,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24104,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24141,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16947,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785611,
            "range": "± 800",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2709213,
            "range": "± 2523",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9487988,
            "range": "± 12733",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34763337,
            "range": "± 31600",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139253586,
            "range": "± 139861",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3130,
            "range": "± 3",
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
            "value": 2443,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1370,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65372,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251093,
            "range": "± 191",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1022528,
            "range": "± 1121",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4867443,
            "range": "± 4409",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27569041,
            "range": "± 22596",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8186551675,
            "range": "± 38227913",
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
          "id": "0e2a45248393c03a90f59771bffc0268d7e244be",
          "message": "pilopt: equal-constrained witness columns removal",
          "timestamp": "2024-12-11T15:13:35Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2224/commits/0e2a45248393c03a90f59771bffc0268d7e244be"
        },
        "date": 1733935075893,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7089,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 605,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1101,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29616,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24959,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24919,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17688,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 812360,
            "range": "± 3143",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2781359,
            "range": "± 13913",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9751576,
            "range": "± 50970",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35694065,
            "range": "± 172327",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142101299,
            "range": "± 918719",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3126,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2459,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2450,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65794,
            "range": "± 418",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251468,
            "range": "± 1108",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026233,
            "range": "± 2197",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4910368,
            "range": "± 41555",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28429967,
            "range": "± 261212",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8899201331,
            "range": "± 76750273",
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
          "id": "818362075d532b0acc2f0fbc7c3c422e890436a2",
          "message": "Expressions and solving routines.",
          "timestamp": "2024-12-11T15:13:35Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2212/commits/818362075d532b0acc2f0fbc7c3c422e890436a2"
        },
        "date": 1733935894024,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6949,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 591,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1096,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29120,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24275,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24124,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17201,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792066,
            "range": "± 904",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2732522,
            "range": "± 3377",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9648150,
            "range": "± 17454",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35758310,
            "range": "± 113503",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142629101,
            "range": "± 636748",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3204,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2510,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2474,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66096,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252972,
            "range": "± 192",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1038051,
            "range": "± 919",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4988062,
            "range": "± 24621",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28657614,
            "range": "± 95381",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9098258915,
            "range": "± 41205932",
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
          "id": "be36d962aa355fcebf4e30c720d74591cc917636",
          "message": "witgen inference.",
          "timestamp": "2024-12-11T15:13:35Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/be36d962aa355fcebf4e30c720d74591cc917636"
        },
        "date": 1733937298133,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6911,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 586,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1044,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30859,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25822,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25781,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18232,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 808948,
            "range": "± 827",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2791553,
            "range": "± 1884",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9728646,
            "range": "± 10806",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35679750,
            "range": "± 36495",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140871348,
            "range": "± 149558",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3185,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2480,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2672,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1369,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65482,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252648,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035208,
            "range": "± 2058",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4880295,
            "range": "± 4094",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27648346,
            "range": "± 17989",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 7980045637,
            "range": "± 49719428",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "598352a23d7f791124541788d8de5f637124b9d8",
          "message": "Expressions and solving routines. (#2212)\n\nThis module is an equivalent of the existing affine_expression.rs, but\nfor compile-time execution on symbolic values instead of run-time\nexecution on concrete values.\n\nUsing the operators defined on that that type, you can build a\nSymbolicAffineExpression from a polynomial identity and then use\n`.solve()` to try to solve for one unknown variable. The result (instead\nof a concrete assignment as in affine_expression.rs) is a\nSymbolicExpression, i.e. a complex expression involving variables\n(assumed to have a concrete value known at run time), constants and\ncertain operators on them.\n\nThe idea is that SymbolicAffineExpression is used on polynomial\nidentities in turn and solving for one cell after the other in the\ntrace. The resulting SymbolicExpression can be translated to rust or\npil.",
          "timestamp": "2024-12-11T17:06:18Z",
          "url": "https://github.com/powdr-labs/powdr/commit/598352a23d7f791124541788d8de5f637124b9d8"
        },
        "date": 1733937829630,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6880,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1081,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29733,
            "range": "± 126",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24921,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25025,
            "range": "± 488",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17646,
            "range": "± 137",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 803563,
            "range": "± 13691",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2753869,
            "range": "± 19307",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9815521,
            "range": "± 94917",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36417923,
            "range": "± 703043",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142713727,
            "range": "± 2755336",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3251,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2474,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2444,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1363,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66882,
            "range": "± 1313",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 255010,
            "range": "± 1329",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1048079,
            "range": "± 19402",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4982166,
            "range": "± 62784",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28134755,
            "range": "± 364422",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8904285465,
            "range": "± 94188375",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "598352a23d7f791124541788d8de5f637124b9d8",
          "message": "Expressions and solving routines. (#2212)\n\nThis module is an equivalent of the existing affine_expression.rs, but\nfor compile-time execution on symbolic values instead of run-time\nexecution on concrete values.\n\nUsing the operators defined on that that type, you can build a\nSymbolicAffineExpression from a polynomial identity and then use\n`.solve()` to try to solve for one unknown variable. The result (instead\nof a concrete assignment as in affine_expression.rs) is a\nSymbolicExpression, i.e. a complex expression involving variables\n(assumed to have a concrete value known at run time), constants and\ncertain operators on them.\n\nThe idea is that SymbolicAffineExpression is used on polynomial\nidentities in turn and solving for one cell after the other in the\ntrace. The resulting SymbolicExpression can be translated to rust or\npil.",
          "timestamp": "2024-12-11T17:06:18Z",
          "tree_id": "30faa5d361e14b3a2606a744750845a8204a85ed",
          "url": "https://github.com/powdr-labs/powdr/commit/598352a23d7f791124541788d8de5f637124b9d8"
        },
        "date": 1733939580383,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7038,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 578,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1059,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30277,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25303,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25169,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17740,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 793151,
            "range": "± 844",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2725395,
            "range": "± 3014",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9562798,
            "range": "± 18926",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34999900,
            "range": "± 39224",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138423933,
            "range": "± 138651",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3116,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2449,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2444,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1364,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65522,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252207,
            "range": "± 184",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030792,
            "range": "± 2214",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4880419,
            "range": "± 3607",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27549003,
            "range": "± 19272",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8102998803,
            "range": "± 27472621",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "ShuangWu121",
            "username": "ShuangWu121",
            "email": "47602565+ShuangWu121@users.noreply.github.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "ad409564caae4aa407abf10170783ce16ce27534",
          "message": "Add support for constant column in STWO backend (#2112)\n\n# Add Support for Constant Columns in Stwo Backend\n\nStwo's recent development introduced new APIs to support\nconstant/pre-processed columns. However, the `dev` branch of Stwo is\nstill using an older nightly toolchain, which is incompatible with\nPowdr.\n\nCurrently, Stwo has two open PRs\n[PR1](https://github.com/starkware-libs/stwo/pull/847),\n[PR2](https://github.com/starkware-libs/stwo/pull/871) aimed at updating\nthe toolchain to nightly `11-06`. Previously, our Stwo backend\ndependency relied on one of these PRs' branches. However, this branch is\nno longer updated with the latest commits from their `dev` branch. These\nnew commits are required to support constant columns.\n\n### Temporary Solution\nTo address this issue temporarily:\n- I moved Stwo's dependency to my fork of Stwo, where the branch is\nupdated with both the latest `dev` branch commits and the newer\ntoolchain.\n\n---\n\n### Tasks in this PR\n1. **Add APIs to support constant columns**:\n2. **Update test cases**:\n   - Modify and add test cases to validate constant column support.\n\n---\n\n---------\n\nCo-authored-by: Thibaut Schaeffer <schaeffer.thibaut@gmail.com>",
          "timestamp": "2024-12-11T17:45:46Z",
          "url": "https://github.com/powdr-labs/powdr/commit/ad409564caae4aa407abf10170783ce16ce27534"
        },
        "date": 1733940122271,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6847,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 585,
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
            "value": 29562,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24715,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24656,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17391,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 796328,
            "range": "± 1458",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2749032,
            "range": "± 5036",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9630490,
            "range": "± 37954",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35558310,
            "range": "± 217805",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140365751,
            "range": "± 722392",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3211,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2501,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2471,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1358,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66355,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253815,
            "range": "± 211",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032165,
            "range": "± 1961",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4913147,
            "range": "± 28722",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28000741,
            "range": "± 162394",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8517560973,
            "range": "± 43336867",
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
          "id": "b98ab700a12e36b717e18c693ac2595972547ce3",
          "message": "pilopt: equal-constrained witness columns removal",
          "timestamp": "2024-12-11T17:36:52Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2224/commits/b98ab700a12e36b717e18c693ac2595972547ce3"
        },
        "date": 1733941003852,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6840,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 630,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1131,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28673,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 23973,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23945,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16955,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 784804,
            "range": "± 1178",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2715969,
            "range": "± 4445",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9434893,
            "range": "± 15877",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34746339,
            "range": "± 69508",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137916430,
            "range": "± 248255",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3127,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2461,
            "range": "± 5",
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
            "value": 1363,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65324,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250597,
            "range": "± 261",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026384,
            "range": "± 1462",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4891388,
            "range": "± 9302",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27796180,
            "range": "± 45889",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8494001989,
            "range": "± 35951610",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "47602565+ShuangWu121@users.noreply.github.com",
            "name": "ShuangWu121",
            "username": "ShuangWu121"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "ad409564caae4aa407abf10170783ce16ce27534",
          "message": "Add support for constant column in STWO backend (#2112)\n\n# Add Support for Constant Columns in Stwo Backend\n\nStwo's recent development introduced new APIs to support\nconstant/pre-processed columns. However, the `dev` branch of Stwo is\nstill using an older nightly toolchain, which is incompatible with\nPowdr.\n\nCurrently, Stwo has two open PRs\n[PR1](https://github.com/starkware-libs/stwo/pull/847),\n[PR2](https://github.com/starkware-libs/stwo/pull/871) aimed at updating\nthe toolchain to nightly `11-06`. Previously, our Stwo backend\ndependency relied on one of these PRs' branches. However, this branch is\nno longer updated with the latest commits from their `dev` branch. These\nnew commits are required to support constant columns.\n\n### Temporary Solution\nTo address this issue temporarily:\n- I moved Stwo's dependency to my fork of Stwo, where the branch is\nupdated with both the latest `dev` branch commits and the newer\ntoolchain.\n\n---\n\n### Tasks in this PR\n1. **Add APIs to support constant columns**:\n2. **Update test cases**:\n   - Modify and add test cases to validate constant column support.\n\n---\n\n---------\n\nCo-authored-by: Thibaut Schaeffer <schaeffer.thibaut@gmail.com>",
          "timestamp": "2024-12-11T17:45:46Z",
          "tree_id": "957a21733e8ae94c43f57985fd6b427827a7de72",
          "url": "https://github.com/powdr-labs/powdr/commit/ad409564caae4aa407abf10170783ce16ce27534"
        },
        "date": 1733942044675,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6998,
            "range": "± 209",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 603,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1074,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29375,
            "range": "± 398",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24452,
            "range": "± 328",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24404,
            "range": "± 371",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17302,
            "range": "± 442",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 805295,
            "range": "± 10968",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2778590,
            "range": "± 55643",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9794996,
            "range": "± 139776",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35570157,
            "range": "± 307430",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142659754,
            "range": "± 3186196",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3209,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2468,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2462,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1382,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67321,
            "range": "± 1525",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 255987,
            "range": "± 4525",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1055260,
            "range": "± 18393",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4941345,
            "range": "± 89721",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 29060717,
            "range": "± 712489",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9033694852,
            "range": "± 133930372",
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
          "id": "6d9cc8e04db2315f5fbd925f50cfe07cd98669b2",
          "message": "[WIP] RISCV executor trace",
          "timestamp": "2024-12-11T18:17:41Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2207/commits/6d9cc8e04db2315f5fbd925f50cfe07cd98669b2"
        },
        "date": 1733942650865,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7194,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 596,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1131,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30571,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25637,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25573,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18127,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 814935,
            "range": "± 1124",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2802361,
            "range": "± 5842",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9780969,
            "range": "± 19490",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35801562,
            "range": "± 168836",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141244369,
            "range": "± 302433",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3167,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2482,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2480,
            "range": "± 8",
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
            "value": 66844,
            "range": "± 159",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252529,
            "range": "± 206",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032169,
            "range": "± 1628",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4901396,
            "range": "± 20503",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27877507,
            "range": "± 201322",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8671273351,
            "range": "± 97202018",
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
          "id": "69af6341073d270452080ce7f0ba3fd23a9963bb",
          "message": "witgen inference.",
          "timestamp": "2024-12-11T18:17:41Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/69af6341073d270452080ce7f0ba3fd23a9963bb"
        },
        "date": 1733995822030,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6917,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 614,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1101,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29551,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24616,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24620,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17279,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 805580,
            "range": "± 1012",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2783182,
            "range": "± 3134",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9719888,
            "range": "± 22101",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35599344,
            "range": "± 64018",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141104628,
            "range": "± 184200",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3151,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2521,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2470,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1366,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65645,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251733,
            "range": "± 346",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030497,
            "range": "± 2767",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4875465,
            "range": "± 4608",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27632027,
            "range": "± 65445",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8177063124,
            "range": "± 27432217",
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
          "id": "46860a1f4f991c4ad389459f11efff0ab4d9d832",
          "message": "Prepare to call jit from block machine.",
          "timestamp": "2024-12-11T18:17:41Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2098/commits/46860a1f4f991c4ad389459f11efff0ab4d9d832"
        },
        "date": 1733998530067,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6866,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 596,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1078,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28894,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24170,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24142,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17007,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 783597,
            "range": "± 664",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2713263,
            "range": "± 2822",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9501362,
            "range": "± 16521",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34884143,
            "range": "± 139643",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138420641,
            "range": "± 212977",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3160,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2705,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 5",
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
            "value": 65760,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253900,
            "range": "± 214",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035954,
            "range": "± 1246",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4884568,
            "range": "± 5492",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27790169,
            "range": "± 39804",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8169453817,
            "range": "± 33910312",
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
          "id": "d31f8153b235b86e7a03b38fd87390d242349ff1",
          "message": "[WIP] RISCV executor trace",
          "timestamp": "2024-12-11T18:17:41Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2207/commits/d31f8153b235b86e7a03b38fd87390d242349ff1"
        },
        "date": 1734001968326,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6895,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 595,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1059,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29232,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24413,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24365,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17248,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 806450,
            "range": "± 1623",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2779030,
            "range": "± 2758",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9673169,
            "range": "± 14363",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35593489,
            "range": "± 51836",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141719696,
            "range": "± 540508",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3123,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2459,
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
            "value": 1359,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66949,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253434,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035530,
            "range": "± 1001",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4899309,
            "range": "± 3021",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27692985,
            "range": "± 19948",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8033335081,
            "range": "± 23421614",
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
          "id": "f7bc144beb6c8beb86be928d517fe2bdb7a78d39",
          "message": "witgen inference.",
          "timestamp": "2024-12-11T18:17:41Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/f7bc144beb6c8beb86be928d517fe2bdb7a78d39"
        },
        "date": 1734004408134,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7052,
            "range": "± 8",
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
            "value": 1096,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29448,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24561,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24682,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17503,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798625,
            "range": "± 639",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2766389,
            "range": "± 3746",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9646177,
            "range": "± 10281",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35200180,
            "range": "± 76937",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139644365,
            "range": "± 105016",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3120,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2453,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2468,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66274,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253909,
            "range": "± 192",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031949,
            "range": "± 937",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4883644,
            "range": "± 10000",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27696536,
            "range": "± 84435",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8154845769,
            "range": "± 22896319",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "2aa7f8388cbea891bf23e3e09ce302f430710847",
          "message": "Prepare to call jit from block machine. (#2098)\n\nThis PR performs preliminary preparations in the block machine so that\nit will be able to JIT-compile and evaluate lookups into this machine\ngiven a certain combination of \"known inputs\".\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-12T11:58:23Z",
          "url": "https://github.com/powdr-labs/powdr/commit/2aa7f8388cbea891bf23e3e09ce302f430710847"
        },
        "date": 1734005695112,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6954,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 635,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1077,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30025,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24453,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24389,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17261,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786252,
            "range": "± 691",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2713160,
            "range": "± 2356",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9492574,
            "range": "± 8823",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34762954,
            "range": "± 31870",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137977371,
            "range": "± 148726",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3163,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2456,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1366,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65892,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253837,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031623,
            "range": "± 1316",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4872427,
            "range": "± 5238",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27667761,
            "range": "± 27121",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8193290453,
            "range": "± 27294110",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "2aa7f8388cbea891bf23e3e09ce302f430710847",
          "message": "Prepare to call jit from block machine. (#2098)\n\nThis PR performs preliminary preparations in the block machine so that\nit will be able to JIT-compile and evaluate lookups into this machine\ngiven a certain combination of \"known inputs\".\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-12T11:58:23Z",
          "tree_id": "5e00e7b9c3a52999f54a6ac426c6a3daffb9d1c1",
          "url": "https://github.com/powdr-labs/powdr/commit/2aa7f8388cbea891bf23e3e09ce302f430710847"
        },
        "date": 1734007625433,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6789,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 2",
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
            "value": 29062,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24287,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24204,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17107,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785120,
            "range": "± 555",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2712988,
            "range": "± 2566",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9492630,
            "range": "± 12779",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34886056,
            "range": "± 55976",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138711991,
            "range": "± 160191",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3119,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2442,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1358,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65195,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250062,
            "range": "± 280",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1023842,
            "range": "± 930",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4848946,
            "range": "± 4721",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27567436,
            "range": "± 17229",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8109770885,
            "range": "± 27600533",
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
          "id": "3e848f5f483b9716a79b6ef89775092ef9dfb866",
          "message": "pilopt: equal-constrained witness columns removal",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2224/commits/3e848f5f483b9716a79b6ef89775092ef9dfb866"
        },
        "date": 1734007932646,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7008,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 655,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1065,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29211,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24400,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24347,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17284,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 787884,
            "range": "± 1439",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2719141,
            "range": "± 6216",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9511514,
            "range": "± 34085",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35272963,
            "range": "± 251578",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140407781,
            "range": "± 1033307",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3174,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2498,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2467,
            "range": "± 2",
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
            "value": 65615,
            "range": "± 148",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 249836,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1022327,
            "range": "± 1537",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4919028,
            "range": "± 26226",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27984550,
            "range": "± 143662",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8811184416,
            "range": "± 58928414",
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
          "id": "cc815d9e081d798d250587338ff7bfd1851309c7",
          "message": "[WIP] RISCV executor trace",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2207/commits/cc815d9e081d798d250587338ff7bfd1851309c7"
        },
        "date": 1734008368359,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6985,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1087,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28839,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24085,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24159,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16948,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790230,
            "range": "± 826",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2746296,
            "range": "± 3837",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9537938,
            "range": "± 14173",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34996340,
            "range": "± 52793",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139181847,
            "range": "± 459116",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3126,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2464,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2448,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65581,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 249740,
            "range": "± 166",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1021879,
            "range": "± 1570",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4870343,
            "range": "± 13782",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27553066,
            "range": "± 58100",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8125455902,
            "range": "± 61342385",
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
          "id": "ea24dd3508a026b9358bf4e81d2e806610d3e385",
          "message": "pilopt: equal-constrained witness columns removal",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2224/commits/ea24dd3508a026b9358bf4e81d2e806610d3e385"
        },
        "date": 1734010508798,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6921,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 610,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1073,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29018,
            "range": "± 186",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24148,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24108,
            "range": "± 147",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17043,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 796447,
            "range": "± 1612",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2757460,
            "range": "± 11057",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9618508,
            "range": "± 29023",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35265157,
            "range": "± 83281",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139988826,
            "range": "± 529960",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3219,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2553,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
            "range": "± 11",
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
            "value": 65821,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252099,
            "range": "± 682",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027169,
            "range": "± 2371",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4881075,
            "range": "± 28536",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27598242,
            "range": "± 140629",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8300769876,
            "range": "± 85358542",
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
          "id": "c37f8cd50aaeaa1db4e8c397f18f006014007868",
          "message": "RISCV executor trace",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2207/commits/c37f8cd50aaeaa1db4e8c397f18f006014007868"
        },
        "date": 1734014455600,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6834,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 582,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1050,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29672,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24867,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24802,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17632,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 783837,
            "range": "± 699",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2708104,
            "range": "± 2367",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9471468,
            "range": "± 11044",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34698127,
            "range": "± 32654",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137669076,
            "range": "± 124571",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3134,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2457,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65403,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252679,
            "range": "± 245",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029518,
            "range": "± 1149",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4890639,
            "range": "± 2944",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27652801,
            "range": "± 19194",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8097841060,
            "range": "± 35161039",
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
          "id": "cd35a60c39ea54ea0256ec9a74b295e143ce6c6a",
          "message": "RISCV executor trace",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2207/commits/cd35a60c39ea54ea0256ec9a74b295e143ce6c6a"
        },
        "date": 1734015434759,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6871,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 596,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1050,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29489,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25257,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24604,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17385,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 779756,
            "range": "± 1605",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2687358,
            "range": "± 5968",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9395858,
            "range": "± 27337",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34573691,
            "range": "± 128075",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137599313,
            "range": "± 617104",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3173,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2511,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2475,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1367,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65913,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253123,
            "range": "± 274",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031214,
            "range": "± 1253",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4870205,
            "range": "± 9006",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27673238,
            "range": "± 176620",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8326813690,
            "range": "± 49475946",
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
          "id": "b715662dddcc895980b39a041b905adc9536ff18",
          "message": "Witgen inference.",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/b715662dddcc895980b39a041b905adc9536ff18"
        },
        "date": 1734017431887,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6849,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 626,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1053,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29224,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24551,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24488,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17268,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 779739,
            "range": "± 2345",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2688838,
            "range": "± 5186",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9531727,
            "range": "± 50455",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35095450,
            "range": "± 227974",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139441302,
            "range": "± 918787",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3121,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2447,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2453,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1369,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65628,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251933,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031589,
            "range": "± 935",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4866285,
            "range": "± 6519",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28082745,
            "range": "± 240885",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8692119747,
            "range": "± 32756843",
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
          "id": "c4dd3a3136e0f69e4d37e4a69b0d8a836e8ca65f",
          "message": "pilopt: optimize until fixpoint",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2225/commits/c4dd3a3136e0f69e4d37e4a69b0d8a836e8ca65f"
        },
        "date": 1734018214105,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6971,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1102,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30173,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25418,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25366,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17848,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 810367,
            "range": "± 1242",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2786109,
            "range": "± 4472",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9734705,
            "range": "± 23560",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35679320,
            "range": "± 91403",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141295508,
            "range": "± 574195",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3140,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2453,
            "range": "± 4",
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
            "value": 66406,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254842,
            "range": "± 266",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032092,
            "range": "± 1271",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885746,
            "range": "± 7197",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27826732,
            "range": "± 103205",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8223811077,
            "range": "± 56384987",
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
          "id": "a905288b7ff3480a3b059872dbd8008a401a6592",
          "message": "Witgen inference.",
          "timestamp": "2024-12-12T12:31:43Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2219/commits/a905288b7ff3480a3b059872dbd8008a401a6592"
        },
        "date": 1734019021330,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6912,
            "range": "± 13",
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
            "value": 1051,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29000,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24248,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24200,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17084,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 782143,
            "range": "± 865",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2709141,
            "range": "± 8957",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9474580,
            "range": "± 54411",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34937433,
            "range": "± 311454",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138326178,
            "range": "± 781053",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3134,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
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
            "value": 66667,
            "range": "± 221",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254839,
            "range": "± 236",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036916,
            "range": "± 1334",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4894519,
            "range": "± 9058",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27845345,
            "range": "± 300203",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8419171025,
            "range": "± 50306381",
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
          "id": "b345808bbd088d50995e339cd24a58d5ac607b2e",
          "message": "pilopt: optimize until fixpoint",
          "timestamp": "2024-12-12T15:56:50Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2225/commits/b345808bbd088d50995e339cd24a58d5ac607b2e"
        },
        "date": 1734020077204,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6939,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1047,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28970,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24176,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24329,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17070,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 787421,
            "range": "± 831",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2718763,
            "range": "± 4278",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9549568,
            "range": "± 42030",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35127097,
            "range": "± 115505",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140529393,
            "range": "± 1064183",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3115,
            "range": "± 7",
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
            "value": 2453,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1366,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65418,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251540,
            "range": "± 185",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1025032,
            "range": "± 1178",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4922886,
            "range": "± 50975",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27976759,
            "range": "± 86621",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8747562832,
            "range": "± 97079463",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "e3c4c858f017c57c4f856cb32a4dfbb5fd1e1fbf",
          "message": "Witgen inference. (#2219)\n\nThis PR adds a component that can derive assignments and other code on\nidentities and multiple rows. It keeps track of which cells in the trace\nare already known and which not. The way to access fixed rows is\nabstracted because it does not have a concept of an absolute row. While\nthis might work for block machines with cyclic fixed columns, it does\nnot work in the general case.\n\nWhat it does not do:\n- have a sequence of which identities to consider on which rows\n- a mechanism that determines when it is finished\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-12T16:12:30Z",
          "url": "https://github.com/powdr-labs/powdr/commit/e3c4c858f017c57c4f856cb32a4dfbb5fd1e1fbf"
        },
        "date": 1734020909618,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6982,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 616,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1117,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29379,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24478,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24472,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17361,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 781592,
            "range": "± 1249",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2711997,
            "range": "± 2533",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9434230,
            "range": "± 10680",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34522977,
            "range": "± 31921",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137923927,
            "range": "± 261190",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3235,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2486,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66038,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253974,
            "range": "± 227",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033596,
            "range": "± 2537",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4884898,
            "range": "± 2925",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27616096,
            "range": "± 25685",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8112722518,
            "range": "± 11272260",
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
          "id": "77422e74e0a4547d648650b3afbb01dcdf6e8246",
          "message": "Various preparatory changes.",
          "timestamp": "2024-12-12T15:56:50Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2228/commits/77422e74e0a4547d648650b3afbb01dcdf6e8246"
        },
        "date": 1734022745364,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6825,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1058,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28840,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24147,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24119,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17098,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786622,
            "range": "± 1164",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2719506,
            "range": "± 3660",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9501693,
            "range": "± 18375",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34943136,
            "range": "± 130325",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138945886,
            "range": "± 314091",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3131,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2520,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2456,
            "range": "± 8",
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
            "value": 66180,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250407,
            "range": "± 342",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024528,
            "range": "± 1312",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4862337,
            "range": "± 4055",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27835567,
            "range": "± 91972",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8491061644,
            "range": "± 30661551",
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
          "id": "fb57219329fc7ad12a11f46a611867889ea923ee",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-12T15:56:50Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/fb57219329fc7ad12a11f46a611867889ea923ee"
        },
        "date": 1734022795270,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7027,
            "range": "± 156",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 616,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1162,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 31126,
            "range": "± 1284",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25572,
            "range": "± 608",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25719,
            "range": "± 608",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17842,
            "range": "± 342",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 809007,
            "range": "± 23508",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2810107,
            "range": "± 56420",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9800161,
            "range": "± 178534",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35716328,
            "range": "± 610699",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142736342,
            "range": "± 4231942",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3283,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2535,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2535,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1418,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 70546,
            "range": "± 1799",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 260478,
            "range": "± 6190",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1073726,
            "range": "± 29084",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 5074779,
            "range": "± 117493",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 29008742,
            "range": "± 697676",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9102290565,
            "range": "± 120472998",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "e3c4c858f017c57c4f856cb32a4dfbb5fd1e1fbf",
          "message": "Witgen inference. (#2219)\n\nThis PR adds a component that can derive assignments and other code on\nidentities and multiple rows. It keeps track of which cells in the trace\nare already known and which not. The way to access fixed rows is\nabstracted because it does not have a concept of an absolute row. While\nthis might work for block machines with cyclic fixed columns, it does\nnot work in the general case.\n\nWhat it does not do:\n- have a sequence of which identities to consider on which rows\n- a mechanism that determines when it is finished\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-12T16:12:30Z",
          "tree_id": "85815ba7f8495ef97aafbeaabb8559abd90b751a",
          "url": "https://github.com/powdr-labs/powdr/commit/e3c4c858f017c57c4f856cb32a4dfbb5fd1e1fbf"
        },
        "date": 1734022820592,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6924,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 593,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1059,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29171,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24352,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24282,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17121,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 780871,
            "range": "± 1393",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2694976,
            "range": "± 3877",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9443917,
            "range": "± 14990",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34546057,
            "range": "± 60401",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137830402,
            "range": "± 423781",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3119,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2497,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2441,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65506,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251709,
            "range": "± 1002",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028864,
            "range": "± 1200",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4882991,
            "range": "± 7026",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27670028,
            "range": "± 118051",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8178148268,
            "range": "± 163591247",
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
          "id": "35d4cbcabeb8440d6f4238b5a81fd01d2d8de093",
          "message": "Various preparatory changes.",
          "timestamp": "2024-12-12T16:44:26Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2228/commits/35d4cbcabeb8440d6f4238b5a81fd01d2d8de093"
        },
        "date": 1734024375238,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6923,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 586,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1053,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30215,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24672,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24620,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17498,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 777356,
            "range": "± 1033",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2682308,
            "range": "± 3620",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9391003,
            "range": "± 12050",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34528194,
            "range": "± 36170",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137325121,
            "range": "± 322116",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3178,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2456,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 5",
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
            "value": 65706,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251734,
            "range": "± 361",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035541,
            "range": "± 1837",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4873685,
            "range": "± 4805",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27616510,
            "range": "± 21911",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8353628034,
            "range": "± 37930372",
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
          "id": "c494ae2b56dbc4374721ae75c183d166e8e25536",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-12T16:44:26Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/c494ae2b56dbc4374721ae75c183d166e8e25536"
        },
        "date": 1734025644113,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6800,
            "range": "± 4",
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
            "value": 1048,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28815,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24033,
            "range": "± 146",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23999,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16971,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 782322,
            "range": "± 1293",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2706127,
            "range": "± 15466",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9495413,
            "range": "± 40509",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34986051,
            "range": "± 182896",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140696029,
            "range": "± 975939",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3162,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2445,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 15",
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
            "value": 65892,
            "range": "± 312",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252229,
            "range": "± 2786",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028453,
            "range": "± 2284",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4884247,
            "range": "± 9079",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28118344,
            "range": "± 145272",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8787454790,
            "range": "± 70414587",
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
          "id": "088bb1a60e8714643d581218782e05082c9523dd",
          "message": "Effects to rust",
          "timestamp": "2024-12-12T16:44:26Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/088bb1a60e8714643d581218782e05082c9523dd"
        },
        "date": 1734027129316,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6814,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 609,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1105,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28682,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24062,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24021,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17022,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 796610,
            "range": "± 750",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2750975,
            "range": "± 2034",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9619548,
            "range": "± 7976",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35333732,
            "range": "± 71904",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139925326,
            "range": "± 136582",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3128,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2462,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66337,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254772,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035774,
            "range": "± 1078",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4878968,
            "range": "± 3123",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27682054,
            "range": "± 13798",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 7992885419,
            "range": "± 31366982",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "12aca0e136cb39db408e47f9e67f5e50251e265c",
          "message": "Various preparatory changes. (#2228)",
          "timestamp": "2024-12-12T17:56:54Z",
          "url": "https://github.com/powdr-labs/powdr/commit/12aca0e136cb39db408e47f9e67f5e50251e265c"
        },
        "date": 1734027148590,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6886,
            "range": "± 5",
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
            "value": 1097,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29172,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24341,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24305,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17105,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 812659,
            "range": "± 875",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2809811,
            "range": "± 2993",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9829302,
            "range": "± 18193",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35800927,
            "range": "± 41757",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142773645,
            "range": "± 134400",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3132,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2449,
            "range": "± 5",
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
            "value": 1364,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65804,
            "range": "± 158",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253863,
            "range": "± 346",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030350,
            "range": "± 1108",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4875712,
            "range": "± 4139",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27660389,
            "range": "± 28449",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8091549626,
            "range": "± 38000076",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "12aca0e136cb39db408e47f9e67f5e50251e265c",
          "message": "Various preparatory changes. (#2228)",
          "timestamp": "2024-12-12T17:56:54Z",
          "tree_id": "991bf1433e5e9bffcaa7f7fd27e46029afbef1d5",
          "url": "https://github.com/powdr-labs/powdr/commit/12aca0e136cb39db408e47f9e67f5e50251e265c"
        },
        "date": 1734029182306,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6797,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 584,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1061,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29446,
            "range": "± 348",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24552,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24511,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17392,
            "range": "± 128",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785295,
            "range": "± 1704",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2711835,
            "range": "± 5768",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9554931,
            "range": "± 35419",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34897386,
            "range": "± 197568",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138584166,
            "range": "± 357186",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3321,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2656,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2607,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1377,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66127,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252356,
            "range": "± 789",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028663,
            "range": "± 1324",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4871730,
            "range": "± 9791",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27799849,
            "range": "± 69270",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8550277661,
            "range": "± 49476958",
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
          "id": "6fe0a003c25a8179a5ed1de89168f0f451c36439",
          "message": "Effects to rust",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/6fe0a003c25a8179a5ed1de89168f0f451c36439"
        },
        "date": 1734031960073,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7006,
            "range": "± 23",
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
            "value": 1063,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29023,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24331,
            "range": "± 341",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24269,
            "range": "± 198",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17093,
            "range": "± 319",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 807218,
            "range": "± 11649",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2738632,
            "range": "± 8167",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9597706,
            "range": "± 69077",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35460848,
            "range": "± 384258",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139993664,
            "range": "± 1185006",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3129,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2451,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2452,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1361,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67690,
            "range": "± 140",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254128,
            "range": "± 271",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035499,
            "range": "± 1584",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885491,
            "range": "± 27342",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27677325,
            "range": "± 360221",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8571815920,
            "range": "± 220048554",
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
          "id": "13b8d15c21d5601e7c91011b76d416278c939380",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/13b8d15c21d5601e7c91011b76d416278c939380"
        },
        "date": 1734032584550,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6861,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1041,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29174,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24347,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24287,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17094,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785728,
            "range": "± 1554",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2705441,
            "range": "± 2849",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9487577,
            "range": "± 11228",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34723445,
            "range": "± 27857",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137818062,
            "range": "± 114529",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3130,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2479,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2450,
            "range": "± 7",
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
            "value": 65816,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251558,
            "range": "± 232",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029531,
            "range": "± 1341",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4881445,
            "range": "± 5199",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27649670,
            "range": "± 18120",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8101131763,
            "range": "± 35861768",
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
          "id": "852ab8518556267ac2fd64edc2965ccf3a08b015",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/852ab8518556267ac2fd64edc2965ccf3a08b015"
        },
        "date": 1734033663084,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6999,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 611,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1066,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28995,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24335,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24270,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17189,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791553,
            "range": "± 1084",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2740601,
            "range": "± 3096",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9570255,
            "range": "± 11541",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35079752,
            "range": "± 49443",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139335025,
            "range": "± 134472",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3151,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2518,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2469,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1377,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65947,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252505,
            "range": "± 212",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030249,
            "range": "± 1115",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4884197,
            "range": "± 4471",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27603700,
            "range": "± 24064",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8164302769,
            "range": "± 37550580",
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
          "id": "9ac32a24d120230bac9fe3290d6aef4b2299ce05",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/9ac32a24d120230bac9fe3290d6aef4b2299ce05"
        },
        "date": 1734036395614,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7062,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 615,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1095,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30397,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25616,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25505,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18449,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 806742,
            "range": "± 1708",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2784971,
            "range": "± 4248",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9805681,
            "range": "± 15964",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35687875,
            "range": "± 41609",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141353298,
            "range": "± 173573",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3126,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2456,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2440,
            "range": "± 6",
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
            "value": 65534,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251703,
            "range": "± 351",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027053,
            "range": "± 1384",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4861225,
            "range": "± 3887",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27647927,
            "range": "± 19708",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8066607807,
            "range": "± 19177371",
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
          "id": "5aa3e08e836ba24ff25ffb1db86d9eaa03fd41bc",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/5aa3e08e836ba24ff25ffb1db86d9eaa03fd41bc"
        },
        "date": 1734040532747,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6874,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 625,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1053,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28806,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24094,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23971,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16983,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 787655,
            "range": "± 1411",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2717450,
            "range": "± 7956",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9497626,
            "range": "± 25522",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35094710,
            "range": "± 150044",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139384378,
            "range": "± 954180",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3136,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2470,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2450,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1378,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65629,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252142,
            "range": "± 180",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029447,
            "range": "± 1149",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4888914,
            "range": "± 4878",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27865314,
            "range": "± 149437",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8326947787,
            "range": "± 121841275",
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
          "id": "7ca71d70f18b2781610b6d88d3e5c10269345dd3",
          "message": "Effects to rust",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/7ca71d70f18b2781610b6d88d3e5c10269345dd3"
        },
        "date": 1734051049323,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6956,
            "range": "± 12",
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
            "value": 1099,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29365,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24577,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24515,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17305,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 817106,
            "range": "± 2004",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2820135,
            "range": "± 4960",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9946755,
            "range": "± 59487",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36689423,
            "range": "± 262448",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 145020703,
            "range": "± 1098557",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3206,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2547,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2500,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1379,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65301,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 249998,
            "range": "± 181",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031994,
            "range": "± 964",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4878924,
            "range": "± 34105",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28363902,
            "range": "± 314631",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8680646783,
            "range": "± 43729079",
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
          "id": "4d127f0a1208e0c67fd69d1fe5ffd295d224a90f",
          "message": "Use padded bitvec.",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2230/commits/4d127f0a1208e0c67fd69d1fe5ffd295d224a90f"
        },
        "date": 1734051973614,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7000,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 605,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1119,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29080,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24407,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24328,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17271,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 801092,
            "range": "± 2220",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2760433,
            "range": "± 3814",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9690700,
            "range": "± 34025",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35520693,
            "range": "± 152858",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141839511,
            "range": "± 1348448",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3134,
            "range": "± 8",
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
            "value": 2451,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1367,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65621,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251944,
            "range": "± 191",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028719,
            "range": "± 898",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885680,
            "range": "± 10207",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28106914,
            "range": "± 151382",
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
          "id": "62e24d556058a21b95c8236c47627202ac40c39a",
          "message": "Use padded bitvec.",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2230/commits/62e24d556058a21b95c8236c47627202ac40c39a"
        },
        "date": 1734085818991,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6946,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 613,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1077,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28894,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24179,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24091,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17077,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794942,
            "range": "± 973",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2723773,
            "range": "± 3481",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9462844,
            "range": "± 12646",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34764534,
            "range": "± 51838",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138357837,
            "range": "± 174867",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3132,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2460,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2482,
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
            "value": 65045,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252264,
            "range": "± 256",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027237,
            "range": "± 1214",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4862417,
            "range": "± 3552",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27620582,
            "range": "± 46874",
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
          "id": "16ab4ae5dd067b112285922b4d6af56eb62d524e",
          "message": "Use padded bitvec.",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2230/commits/16ab4ae5dd067b112285922b4d6af56eb62d524e"
        },
        "date": 1734089422443,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6853,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1046,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29019,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24307,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24267,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17170,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786806,
            "range": "± 946",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2718568,
            "range": "± 3804",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9483692,
            "range": "± 36167",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34644609,
            "range": "± 112560",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138129885,
            "range": "± 352662",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3212,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2469,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2558,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1363,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67247,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251102,
            "range": "± 227",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1022445,
            "range": "± 920",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4855065,
            "range": "± 4343",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27675294,
            "range": "± 19044",
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
          "id": "2e94cd96fdf372225eec8fc0a1fc7b43d6713568",
          "message": "Effects to rust",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/2e94cd96fdf372225eec8fc0a1fc7b43d6713568"
        },
        "date": 1734090406066,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6838,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 594,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1091,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29267,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24377,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24360,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17210,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800053,
            "range": "± 2562",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2762905,
            "range": "± 3675",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9686856,
            "range": "± 13770",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35581836,
            "range": "± 57648",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140979720,
            "range": "± 167662",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3123,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2474,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2443,
            "range": "± 6",
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
            "value": 65411,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251923,
            "range": "± 434",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031806,
            "range": "± 12915",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4863265,
            "range": "± 5703",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27540663,
            "range": "± 36026",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8126754285,
            "range": "± 30357616",
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
          "id": "605be04fe94de043a9dd035b0810abdc942a9089",
          "message": "Remove constant intermediates",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2163/commits/605be04fe94de043a9dd035b0810abdc942a9089"
        },
        "date": 1734095763016,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6859,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 588,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1059,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28796,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24045,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23980,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16955,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 793077,
            "range": "± 1003",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2744987,
            "range": "± 2933",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9578927,
            "range": "± 16799",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35139991,
            "range": "± 97056",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140806587,
            "range": "± 801394",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3120,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2455,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2456,
            "range": "± 5",
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
            "value": 66029,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252683,
            "range": "± 212",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1034369,
            "range": "± 1753",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4894298,
            "range": "± 6457",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28019557,
            "range": "± 100424",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8588650000,
            "range": "± 66145767",
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
          "id": "48de0e832e14da0e5dd5377bc75c6eea55c1bcc6",
          "message": "Remove constant intermediates",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2163/commits/48de0e832e14da0e5dd5377bc75c6eea55c1bcc6"
        },
        "date": 1734099200978,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7036,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 602,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1117,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29040,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24295,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24336,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17091,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 782821,
            "range": "± 1568",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2703167,
            "range": "± 5469",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9486703,
            "range": "± 25206",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34855670,
            "range": "± 136798",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138453765,
            "range": "± 491739",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3200,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2458,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2460,
            "range": "± 6",
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
            "value": 65633,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252421,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1042814,
            "range": "± 1257",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885339,
            "range": "± 4170",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27876044,
            "range": "± 210044",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8271288461,
            "range": "± 38124442",
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
          "id": "5b3e45a1d9d186218b4dd5fab12d532988ba0ca1",
          "message": "Effects to rust",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/5b3e45a1d9d186218b4dd5fab12d532988ba0ca1"
        },
        "date": 1734100078097,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6802,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 622,
            "range": "± 1",
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
            "value": 29032,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24182,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24141,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17084,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 782428,
            "range": "± 1110",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2700478,
            "range": "± 4555",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9422709,
            "range": "± 15661",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34605928,
            "range": "± 111735",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137205911,
            "range": "± 165821",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3184,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2450,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2443,
            "range": "± 5",
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
            "value": 65672,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254514,
            "range": "± 255",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033287,
            "range": "± 1378",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4856386,
            "range": "± 2876",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27659303,
            "range": "± 53257",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8280915258,
            "range": "± 16886347",
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
          "id": "e369b9230c5865841c3428877125346f91f4b31a",
          "message": "Effects to rust",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/e369b9230c5865841c3428877125346f91f4b31a"
        },
        "date": 1734101557131,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6866,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 602,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1055,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28947,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24177,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24290,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17103,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786779,
            "range": "± 2401",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2723147,
            "range": "± 6126",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9579132,
            "range": "± 41933",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35416743,
            "range": "± 287045",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140299301,
            "range": "± 920906",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3228,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2556,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2528,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1353,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65687,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252575,
            "range": "± 189",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027680,
            "range": "± 1065",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885617,
            "range": "± 23980",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28050560,
            "range": "± 257822",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8577520704,
            "range": "± 97941857",
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
          "id": "13b7a3929450b81f480d64d8a4eeb6e174a3472c",
          "message": "Remove reset instruction",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/1718/commits/13b7a3929450b81f480d64d8a4eeb6e174a3472c"
        },
        "date": 1734104446031,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7074,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 690,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1081,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29224,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24529,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24355,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17309,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 779025,
            "range": "± 1042",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2692523,
            "range": "± 3332",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9374815,
            "range": "± 11121",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34403611,
            "range": "± 53841",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 136675499,
            "range": "± 129932",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3279,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2449,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2446,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1359,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65698,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254707,
            "range": "± 175",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029811,
            "range": "± 1344",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4872373,
            "range": "± 2528",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27608916,
            "range": "± 28392",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8121202517,
            "range": "± 37028591",
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
          "id": "62377e99f9ed4634c537c44155132ffb0eea72d9",
          "message": "Prepare block machine processor",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2231/commits/62377e99f9ed4634c537c44155132ffb0eea72d9"
        },
        "date": 1734106084548,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7225,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 626,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1102,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29613,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24768,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24703,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17408,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 811013,
            "range": "± 2871",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2804608,
            "range": "± 8486",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9805771,
            "range": "± 70646",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35835534,
            "range": "± 232140",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142654377,
            "range": "± 884714",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3195,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2521,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2476,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1384,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66278,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254356,
            "range": "± 1088",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1038944,
            "range": "± 4936",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4917520,
            "range": "± 13502",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28161624,
            "range": "± 183171",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8856098067,
            "range": "± 60486521",
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
          "id": "db17f9c7ac704ac9ab29b784ea7ed3add45f953b",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/db17f9c7ac704ac9ab29b784ea7ed3add45f953b"
        },
        "date": 1734106105043,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6804,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 585,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1066,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29058,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24219,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24199,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17168,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794430,
            "range": "± 1853",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2736553,
            "range": "± 4990",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9597378,
            "range": "± 35453",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35040266,
            "range": "± 76490",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138868110,
            "range": "± 501529",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3140,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2466,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2453,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1375,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65872,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251919,
            "range": "± 204",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028392,
            "range": "± 982",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4897565,
            "range": "± 4332",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27640106,
            "range": "± 92950",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8172434296,
            "range": "± 80199211",
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
          "id": "3365e53e7d9d4ff38384e3aba47acc05c32c4b75",
          "message": "Remove reset instruction",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/1718/commits/3365e53e7d9d4ff38384e3aba47acc05c32c4b75"
        },
        "date": 1734112083882,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7009,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 610,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1076,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29948,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25040,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25063,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17688,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 787574,
            "range": "± 1574",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2713843,
            "range": "± 3484",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9503879,
            "range": "± 19675",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34880993,
            "range": "± 140438",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138449389,
            "range": "± 126394",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3125,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2464,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65500,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251747,
            "range": "± 330",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027488,
            "range": "± 1115",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4873102,
            "range": "± 2356",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27623674,
            "range": "± 16417",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8166958444,
            "range": "± 29086000",
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
          "id": "3bb99a22b99cf8d89ef41fa44b81d4bce87f7f6d",
          "message": "(WIP) make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/3bb99a22b99cf8d89ef41fa44b81d4bce87f7f6d"
        },
        "date": 1734119373068,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6839,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 586,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1094,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29124,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24364,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24353,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17248,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785479,
            "range": "± 918",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2717505,
            "range": "± 4076",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9582240,
            "range": "± 24802",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34954402,
            "range": "± 105315",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138948281,
            "range": "± 299844",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3126,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2456,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2475,
            "range": "± 11",
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
            "value": 65020,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250914,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1023447,
            "range": "± 1249",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4844192,
            "range": "± 3434",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27592154,
            "range": "± 79934",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8263644981,
            "range": "± 35101058",
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
          "id": "bc886586be4b10727cd6b1ba2ba8033fe24ee6a9",
          "message": "Remove reset instruction",
          "timestamp": "2024-12-12T18:30:21Z",
          "url": "https://github.com/powdr-labs/powdr/pull/1718/commits/bc886586be4b10727cd6b1ba2ba8033fe24ee6a9"
        },
        "date": 1734164021937,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6831,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1101,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29145,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24233,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24231,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17200,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 787800,
            "range": "± 779",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2710336,
            "range": "± 3067",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9382283,
            "range": "± 13004",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34650846,
            "range": "± 76807",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138974588,
            "range": "± 326689",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3152,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2460,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2457,
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
            "value": 65866,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253440,
            "range": "± 197",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031984,
            "range": "± 1270",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4895391,
            "range": "± 3876",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27700597,
            "range": "± 38825",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8168331407,
            "range": "± 21036553",
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
          "id": "3ffbd9ac3d99862a3cdf1d3d00cc23e38bca9662",
          "message": "Use padded bitvec.",
          "timestamp": "2024-12-15T22:24:48Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2230/commits/3ffbd9ac3d99862a3cdf1d3d00cc23e38bca9662"
        },
        "date": 1734344524200,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7183,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 684,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1155,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29395,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24608,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24510,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17283,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 831074,
            "range": "± 776",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2878224,
            "range": "± 3407",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 10026736,
            "range": "± 16520",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36554194,
            "range": "± 154796",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 143374311,
            "range": "± 226820",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3161,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2498,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2475,
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
            "value": 65635,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252632,
            "range": "± 148",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027060,
            "range": "± 1004",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4867059,
            "range": "± 2554",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27582955,
            "range": "± 86585",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8357144342,
            "range": "± 44057049",
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
          "id": "c75700d8296fdebfc81a118d6bc4c73517baeea5",
          "message": "Prepare block machine processor",
          "timestamp": "2024-12-15T22:24:48Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2231/commits/c75700d8296fdebfc81a118d6bc4c73517baeea5"
        },
        "date": 1734345502166,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6951,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 607,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1096,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29150,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24343,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24209,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17171,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 795299,
            "range": "± 1080",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2747149,
            "range": "± 4313",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9654320,
            "range": "± 26580",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35168622,
            "range": "± 134047",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139661584,
            "range": "± 307596",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3128,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2450,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2458,
            "range": "± 6",
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
            "value": 65728,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253129,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030747,
            "range": "± 1512",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4868974,
            "range": "± 4298",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27721138,
            "range": "± 83483",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8150431618,
            "range": "± 26134435",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "e5f93920104a3bcbb504f784a8f0d2624a551e77",
          "message": "Use padded bitvec. (#2230)",
          "timestamp": "2024-12-16T10:49:30Z",
          "url": "https://github.com/powdr-labs/powdr/commit/e5f93920104a3bcbb504f784a8f0d2624a551e77"
        },
        "date": 1734347150846,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7344,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 603,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1070,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29638,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24686,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24719,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17469,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791946,
            "range": "± 2384",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2731358,
            "range": "± 8045",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9559669,
            "range": "± 64092",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34886381,
            "range": "± 251723",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139038506,
            "range": "± 1246305",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3158,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2502,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2465,
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
            "value": 65685,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252582,
            "range": "± 267",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031836,
            "range": "± 2330",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4899260,
            "range": "± 32674",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28073094,
            "range": "± 164239",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8627108200,
            "range": "± 228310971",
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
          "id": "7ef386c909b4d84714e5d15e78147eaa7a9b13c8",
          "message": "executor fixes",
          "timestamp": "2024-12-15T22:24:48Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2238/commits/7ef386c909b4d84714e5d15e78147eaa7a9b13c8"
        },
        "date": 1734348784493,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6938,
            "range": "± 11",
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
            "value": 1086,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28808,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24124,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24086,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17007,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 784718,
            "range": "± 879",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2698581,
            "range": "± 2451",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9465932,
            "range": "± 11041",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34609027,
            "range": "± 29198",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138121685,
            "range": "± 284185",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3136,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2451,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2446,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1364,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67783,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 259288,
            "range": "± 172",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1056500,
            "range": "± 1072",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4963513,
            "range": "± 2699",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28005111,
            "range": "± 13910",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8370757426,
            "range": "± 231160763",
            "unit": "ns/iter"
          }
        ]
      },
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
          "distinct": false,
          "id": "e5f93920104a3bcbb504f784a8f0d2624a551e77",
          "message": "Use padded bitvec. (#2230)",
          "timestamp": "2024-12-16T10:49:30Z",
          "tree_id": "6db302c3d2fd9014e8b4015398f6290fd9e5d524",
          "url": "https://github.com/powdr-labs/powdr/commit/e5f93920104a3bcbb504f784a8f0d2624a551e77"
        },
        "date": 1734348925266,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6904,
            "range": "± 8",
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
            "value": 1069,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29332,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24515,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24426,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17269,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 803208,
            "range": "± 1380",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2772349,
            "range": "± 3794",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9703568,
            "range": "± 22964",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35521630,
            "range": "± 98762",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141455033,
            "range": "± 544764",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3145,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2521,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2537,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65881,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253404,
            "range": "± 275",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035369,
            "range": "± 1624",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4900293,
            "range": "± 3756",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27827796,
            "range": "± 53720",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8499844073,
            "range": "± 70304505",
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
          "id": "7cda443e11d21014404ff3b3d868fc87054799e0",
          "message": "RISCV executor trace",
          "timestamp": "2024-12-16T11:19:39Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2207/commits/7cda443e11d21014404ff3b3d868fc87054799e0"
        },
        "date": 1734350209899,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6922,
            "range": "± 18",
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
            "value": 1052,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29094,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24208,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24154,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17097,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791858,
            "range": "± 1174",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2737077,
            "range": "± 3101",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9531269,
            "range": "± 18559",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35016240,
            "range": "± 118655",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139174815,
            "range": "± 398548",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3187,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2510,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2465,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1373,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65623,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253343,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036416,
            "range": "± 1024",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4890367,
            "range": "± 12592",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27617252,
            "range": "± 154350",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8349171422,
            "range": "± 40705727",
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
          "id": "5075bd154a015e4384cf5c35f9a37e4b399fc934",
          "message": "Prepare block machine processor",
          "timestamp": "2024-12-16T11:19:39Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2231/commits/5075bd154a015e4384cf5c35f9a37e4b399fc934"
        },
        "date": 1734350324755,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7055,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 580,
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
            "value": 30829,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25796,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25700,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18268,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 809668,
            "range": "± 2105",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2797620,
            "range": "± 4815",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9746530,
            "range": "± 29329",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35780000,
            "range": "± 171839",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141800337,
            "range": "± 277068",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3121,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
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
            "value": 1360,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65550,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250444,
            "range": "± 237",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024472,
            "range": "± 770",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4878259,
            "range": "± 6836",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27698614,
            "range": "± 146903",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8342990069,
            "range": "± 61665149",
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
          "id": "25ff483ae2bdf466bfed278af63bef26e77764f5",
          "message": "(WIP) make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-16T11:19:39Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/25ff483ae2bdf466bfed278af63bef26e77764f5"
        },
        "date": 1734352253146,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6896,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 618,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1066,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29267,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24469,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24365,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17227,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 799206,
            "range": "± 659",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2766503,
            "range": "± 2677",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9691575,
            "range": "± 20677",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35367784,
            "range": "± 99376",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139894844,
            "range": "± 661885",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3169,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2509,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2456,
            "range": "± 2",
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
            "value": 67850,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 255849,
            "range": "± 197",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036143,
            "range": "± 1141",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4860327,
            "range": "± 3922",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27543525,
            "range": "± 18674",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8166587340,
            "range": "± 45606664",
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
          "id": "c96cf506296be090d134118707568cb405096486",
          "message": "Prepare block machine processor",
          "timestamp": "2024-12-16T11:19:39Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2231/commits/c96cf506296be090d134118707568cb405096486"
        },
        "date": 1734354210728,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6867,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 580,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1064,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29050,
            "range": "± 443",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24213,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24196,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17097,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790173,
            "range": "± 1173",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2730642,
            "range": "± 5423",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9566850,
            "range": "± 15792",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34914547,
            "range": "± 42377",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138937418,
            "range": "± 331387",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3159,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2492,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2480,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1376,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67360,
            "range": "± 91",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 255648,
            "range": "± 199",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1039356,
            "range": "± 1670",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4915037,
            "range": "± 12196",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27656848,
            "range": "± 55084",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8357423572,
            "range": "± 71957768",
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
          "id": "04cc70a252f8fd13d59cfdb921ab51be75fe82d8",
          "message": "(WIP) make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-16T11:19:39Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/04cc70a252f8fd13d59cfdb921ab51be75fe82d8"
        },
        "date": 1734355021068,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6904,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 613,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1096,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29173,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24467,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24474,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17178,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 795925,
            "range": "± 3597",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2789028,
            "range": "± 4527",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9778167,
            "range": "± 13801",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35775309,
            "range": "± 36348",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140859656,
            "range": "± 128251",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3240,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2493,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2512,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66073,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253607,
            "range": "± 161",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036662,
            "range": "± 1144",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4883546,
            "range": "± 2684",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27642021,
            "range": "± 21733",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8173880930,
            "range": "± 24761471",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Leandro Pacheco",
            "username": "pacheco",
            "email": "contact@leandropacheco.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "19a945759eddbdea69acc64b8577b44246b345cd",
          "message": "RISCV executor trace (#2207)\n\nRefactor the RISCV executor to produce a list of calls into submachines,\nin preparation of integrating with jit witgen.",
          "timestamp": "2024-12-16T13:47:29Z",
          "url": "https://github.com/powdr-labs/powdr/commit/19a945759eddbdea69acc64b8577b44246b345cd"
        },
        "date": 1734357798671,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6910,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1060,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29554,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24690,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24573,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17390,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800872,
            "range": "± 1189",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2760422,
            "range": "± 4207",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9679198,
            "range": "± 20568",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35329865,
            "range": "± 34357",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139867247,
            "range": "± 230909",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3246,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2510,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2473,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1361,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66518,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 265397,
            "range": "± 248",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1044253,
            "range": "± 1158",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4896453,
            "range": "± 3032",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27773329,
            "range": "± 37185",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8167696757,
            "range": "± 51149019",
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
          "id": "d007b8f14588f569ea20e7eb2aebddac3d051499",
          "message": "Prepare block machine processor (#2231)\n\nA few preparations for #2226:\n- Extracted a `test_utils` module\n- Introduced a new `Variable` type, which can refer to either a cell or\na \"parameter\" (either input or output of a machine call). I think in the\nfuture, we could have more variants (e.g. scalar publics). `Variable` is\nnow used instead of `Cell` in `WitgenInference`.\n- `WitgenInference::process_identity` now also returns whether any\nprogress has been made.\n- Renamed `lookup` -> `machine_call` when rendering `Effect`s",
          "timestamp": "2024-12-16T13:53:10Z",
          "url": "https://github.com/powdr-labs/powdr/commit/d007b8f14588f569ea20e7eb2aebddac3d051499"
        },
        "date": 1734358149542,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6838,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1060,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29705,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24657,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24683,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17425,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 797501,
            "range": "± 1675",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2735642,
            "range": "± 5030",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9616960,
            "range": "± 35036",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35262360,
            "range": "± 141221",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139651426,
            "range": "± 379748",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3155,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2484,
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
            "value": 1359,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67959,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254193,
            "range": "± 399",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036665,
            "range": "± 1098",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4894300,
            "range": "± 4224",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27810261,
            "range": "± 105820",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8438058327,
            "range": "± 74809948",
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
          "id": "2b87cc83a46a433fcfa42c94c05fa60567e09a30",
          "message": "(WIP) make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-16T11:19:39Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/2b87cc83a46a433fcfa42c94c05fa60567e09a30"
        },
        "date": 1734359431143,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7048,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 584,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1051,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29414,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24764,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24602,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17580,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 805860,
            "range": "± 767",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2791741,
            "range": "± 5625",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9719146,
            "range": "± 20180",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35676657,
            "range": "± 74353",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141784340,
            "range": "± 237857",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3139,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2473,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1383,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65391,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250994,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1022734,
            "range": "± 2725",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4874589,
            "range": "± 15368",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27760206,
            "range": "± 48483",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8270582340,
            "range": "± 45890566",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "contact@leandropacheco.com",
            "name": "Leandro Pacheco",
            "username": "pacheco"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "19a945759eddbdea69acc64b8577b44246b345cd",
          "message": "RISCV executor trace (#2207)\n\nRefactor the RISCV executor to produce a list of calls into submachines,\nin preparation of integrating with jit witgen.",
          "timestamp": "2024-12-16T13:47:29Z",
          "tree_id": "6b357a7cd7b6a39ea2f97b344261e738012ea510",
          "url": "https://github.com/powdr-labs/powdr/commit/19a945759eddbdea69acc64b8577b44246b345cd"
        },
        "date": 1734359625048,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7118,
            "range": "± 11",
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
            "value": 1072,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29651,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24688,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25054,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17413,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792474,
            "range": "± 1078",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2742348,
            "range": "± 6370",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9572962,
            "range": "± 24457",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35017832,
            "range": "± 61364",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139186496,
            "range": "± 570462",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3134,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2746,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2450,
            "range": "± 3",
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
            "value": 66273,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252832,
            "range": "± 288",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030289,
            "range": "± 818",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4877573,
            "range": "± 4870",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27773596,
            "range": "± 89605",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8493913343,
            "range": "± 103252813",
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
          "id": "8aafe83d9de2119ef8f2b817e2a3b6377aa7dc6a",
          "message": "free input data",
          "timestamp": "2024-12-16T14:17:57Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2239/commits/8aafe83d9de2119ef8f2b817e2a3b6377aa7dc6a"
        },
        "date": 1734359789402,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6879,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 595,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1048,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29237,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24470,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24436,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17254,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791905,
            "range": "± 837",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2734701,
            "range": "± 3423",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9537380,
            "range": "± 12117",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34980050,
            "range": "± 49504",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138972011,
            "range": "± 160845",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3165,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2504,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2484,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66901,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254860,
            "range": "± 207",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1038468,
            "range": "± 1179",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4896144,
            "range": "± 3993",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27661206,
            "range": "± 23387",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8144092817,
            "range": "± 48585889",
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
          "id": "b1dd99eedd8572b0e33617fdf21bc73125b675a9",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-16T14:17:57Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/b1dd99eedd8572b0e33617fdf21bc73125b675a9"
        },
        "date": 1734359975784,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7001,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 597,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1050,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29359,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24452,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24450,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17276,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 803330,
            "range": "± 3230",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2773587,
            "range": "± 5610",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9689934,
            "range": "± 13876",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35579814,
            "range": "± 60560",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140941835,
            "range": "± 563744",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3111,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2524,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65634,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250599,
            "range": "± 260",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029001,
            "range": "± 1646",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4868079,
            "range": "± 7796",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27717382,
            "range": "± 165613",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8475362877,
            "range": "± 44060746",
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
          "id": "d007b8f14588f569ea20e7eb2aebddac3d051499",
          "message": "Prepare block machine processor (#2231)\n\nA few preparations for #2226:\n- Extracted a `test_utils` module\n- Introduced a new `Variable` type, which can refer to either a cell or\na \"parameter\" (either input or output of a machine call). I think in the\nfuture, we could have more variants (e.g. scalar publics). `Variable` is\nnow used instead of `Cell` in `WitgenInference`.\n- `WitgenInference::process_identity` now also returns whether any\nprogress has been made.\n- Renamed `lookup` -> `machine_call` when rendering `Effect`s",
          "timestamp": "2024-12-16T13:53:10Z",
          "tree_id": "3d58668c897c2fba0a259f011f0be33ece8e0c26",
          "url": "https://github.com/powdr-labs/powdr/commit/d007b8f14588f569ea20e7eb2aebddac3d051499"
        },
        "date": 1734361388347,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6987,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 591,
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
            "value": 29448,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24405,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24417,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17309,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792768,
            "range": "± 1512",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2729821,
            "range": "± 4515",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9537799,
            "range": "± 14920",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34964168,
            "range": "± 133098",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139639228,
            "range": "± 680860",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3176,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2522,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2476,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1378,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65793,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251910,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031155,
            "range": "± 1748",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4902718,
            "range": "± 3942",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27674372,
            "range": "± 117287",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8357366459,
            "range": "± 71380549",
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
          "id": "338bd432deb517d6457c0863508098811dd16a4d",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/338bd432deb517d6457c0863508098811dd16a4d"
        },
        "date": 1734363338533,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6875,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 581,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1068,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28994,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24094,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24002,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16972,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 793410,
            "range": "± 1027",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2731666,
            "range": "± 3500",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9562742,
            "range": "± 26407",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35043555,
            "range": "± 416377",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140893327,
            "range": "± 966038",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3130,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2479,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2562,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1375,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65995,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251845,
            "range": "± 680",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029547,
            "range": "± 6445",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4879497,
            "range": "± 58846",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27702719,
            "range": "± 253748",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8831370184,
            "range": "± 281640271",
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
          "id": "f09ae213d07767a5ac4dd24859e5f04699c4bdc0",
          "message": "(WIP) make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/f09ae213d07767a5ac4dd24859e5f04699c4bdc0"
        },
        "date": 1734367179990,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7637,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 620,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1109,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 34160,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 28587,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 28534,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 20087,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 836860,
            "range": "± 2233",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2873263,
            "range": "± 4573",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 10082724,
            "range": "± 30014",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36801483,
            "range": "± 241321",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 144357779,
            "range": "± 767107",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3128,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2443,
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
            "value": 66075,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252904,
            "range": "± 184",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036719,
            "range": "± 1246",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4908816,
            "range": "± 3752",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27710633,
            "range": "± 19270",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8178344228,
            "range": "± 24603338",
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
          "id": "86f70859c4eb01414963a8b37a3063631c6be95c",
          "message": "make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/86f70859c4eb01414963a8b37a3063631c6be95c"
        },
        "date": 1734369854946,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6923,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 579,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1108,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29583,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24770,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24657,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17350,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798363,
            "range": "± 1472",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2762696,
            "range": "± 5182",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9715336,
            "range": "± 57185",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35846336,
            "range": "± 289678",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 143131989,
            "range": "± 1242888",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3128,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2448,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2447,
            "range": "± 6",
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
            "value": 65666,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252730,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1034705,
            "range": "± 2587",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4992521,
            "range": "± 79813",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30312526,
            "range": "± 361819",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8963636725,
            "range": "± 80829288",
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
          "id": "2910c59b819e81cc02fceffff5af2129f598b4e4",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/2910c59b819e81cc02fceffff5af2129f598b4e4"
        },
        "date": 1734371100196,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7009,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 598,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1075,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29575,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24616,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24537,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17366,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 808629,
            "range": "± 783",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2785637,
            "range": "± 4393",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9659115,
            "range": "± 8021",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35774749,
            "range": "± 45179",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140164058,
            "range": "± 103087",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3141,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2453,
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
            "value": 66087,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253469,
            "range": "± 595",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1039225,
            "range": "± 1196",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4892305,
            "range": "± 2935",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27656184,
            "range": "± 17358",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8072939427,
            "range": "± 37897385",
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
          "id": "ecbf7de382bef8476b3382b24f4ec2aca5893269",
          "message": "Effects to rust",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/ecbf7de382bef8476b3382b24f4ec2aca5893269"
        },
        "date": 1734371402420,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6750,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 621,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1072,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29157,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24308,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24237,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16920,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798595,
            "range": "± 1208",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2765995,
            "range": "± 4363",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9670395,
            "range": "± 10424",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35449255,
            "range": "± 85340",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140340694,
            "range": "± 229026",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3120,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2457,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2443,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1367,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65448,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251842,
            "range": "± 158",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031076,
            "range": "± 1418",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4876563,
            "range": "± 3940",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27649427,
            "range": "± 104609",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8429941210,
            "range": "± 22561979",
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
          "id": "d4881d0fc12b65b598755fa946fa4e4189afeea8",
          "message": "Effects to rust",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/d4881d0fc12b65b598755fa946fa4e4189afeea8"
        },
        "date": 1734372591758,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6969,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 597,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1086,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28953,
            "range": "± 318",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24347,
            "range": "± 289",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24408,
            "range": "± 266",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17101,
            "range": "± 219",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 819958,
            "range": "± 12604",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2861286,
            "range": "± 58889",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 10216103,
            "range": "± 246400",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36843064,
            "range": "± 830752",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 143652736,
            "range": "± 3613110",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3189,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2527,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2484,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1412,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 68824,
            "range": "± 1973",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 257108,
            "range": "± 5891",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1046588,
            "range": "± 17868",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4911650,
            "range": "± 56686",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27721522,
            "range": "± 483596",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8471916695,
            "range": "± 56524969",
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
          "id": "e32252feeaae8200f123c6cfaf7d32a04decf717",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/e32252feeaae8200f123c6cfaf7d32a04decf717"
        },
        "date": 1734374221562,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6958,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 598,
            "range": "± 0",
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
            "value": 28956,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24003,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24092,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16981,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794491,
            "range": "± 2679",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2783506,
            "range": "± 6618",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9700098,
            "range": "± 16500",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35553605,
            "range": "± 72235",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141122260,
            "range": "± 149053",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3120,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2460,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2443,
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
            "value": 65272,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251478,
            "range": "± 306",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1025889,
            "range": "± 1102",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4854197,
            "range": "± 2719",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27653903,
            "range": "± 20302",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8141363889,
            "range": "± 36647862",
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
          "id": "262dc2d2bb332882d101aa2d7c636e91bca008d7",
          "message": "Effects to rust",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/262dc2d2bb332882d101aa2d7c636e91bca008d7"
        },
        "date": 1734376704486,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6883,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 642,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1072,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28938,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24214,
            "range": "± 111",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24186,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17160,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 788434,
            "range": "± 865",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2686823,
            "range": "± 5793",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9457867,
            "range": "± 29833",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34489645,
            "range": "± 81328",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137449829,
            "range": "± 313006",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3122,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2458,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2458,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66710,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252905,
            "range": "± 245",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033478,
            "range": "± 1652",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4868572,
            "range": "± 6388",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27682269,
            "range": "± 154543",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8361888275,
            "range": "± 59325745",
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
          "id": "debacf88f9b0822dd86022e5b2691bc0c1a9b2c1",
          "message": "Support machine calls.",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2241/commits/debacf88f9b0822dd86022e5b2691bc0c1a9b2c1"
        },
        "date": 1734380811988,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6918,
            "range": "± 4",
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
            "value": 1066,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28755,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24047,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24021,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17016,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785405,
            "range": "± 884",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2713383,
            "range": "± 6300",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9479107,
            "range": "± 24061",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34973862,
            "range": "± 97455",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139363915,
            "range": "± 754491",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3182,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2532,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2462,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1379,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65856,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252679,
            "range": "± 167",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028834,
            "range": "± 884",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4869464,
            "range": "± 12464",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27722156,
            "range": "± 35179",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8306107645,
            "range": "± 73472711",
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
          "id": "f324eabecc243db442262641335aa3502d6aa2b1",
          "message": "make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/f324eabecc243db442262641335aa3502d6aa2b1"
        },
        "date": 1734384061189,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6969,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 604,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1111,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29754,
            "range": "± 591",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25419,
            "range": "± 559",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24841,
            "range": "± 536",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17426,
            "range": "± 338",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 829642,
            "range": "± 16026",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2879631,
            "range": "± 60788",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9924940,
            "range": "± 151082",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 37458403,
            "range": "± 735354",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 147957640,
            "range": "± 3225669",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3253,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2494,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2491,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67557,
            "range": "± 1052",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254860,
            "range": "± 2386",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1037624,
            "range": "± 6842",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4961831,
            "range": "± 72811",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28359219,
            "range": "± 386121",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9040695621,
            "range": "± 70012306",
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
          "id": "ebcc3f6380d11a9852d79b12ed68ce31d907cc46",
          "message": "make the RISCV executor look at the optimized PIL for link information",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2232/commits/ebcc3f6380d11a9852d79b12ed68ce31d907cc46"
        },
        "date": 1734385336507,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6945,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 593,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1064,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29137,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24268,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24292,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17201,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 799208,
            "range": "± 2163",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2747648,
            "range": "± 5465",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9664767,
            "range": "± 51100",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35310124,
            "range": "± 305360",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140633723,
            "range": "± 652407",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3134,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2450,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1364,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65962,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253078,
            "range": "± 322",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030609,
            "range": "± 1810",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885684,
            "range": "± 21020",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27837669,
            "range": "± 189211",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8567130302,
            "range": "± 189509891",
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
          "id": "9acf8c81cba767f7dc2a48a6340fb1a357fc4630",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/9acf8c81cba767f7dc2a48a6340fb1a357fc4630"
        },
        "date": 1734425413260,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7124,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 633,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1140,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29464,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24609,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24594,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17373,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 797091,
            "range": "± 1301",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2745053,
            "range": "± 2462",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9579985,
            "range": "± 10127",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35131746,
            "range": "± 41011",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139815920,
            "range": "± 669416",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3180,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2443,
            "range": "± 13",
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
            "value": 65688,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250864,
            "range": "± 259",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024379,
            "range": "± 859",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4865943,
            "range": "± 4643",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27761747,
            "range": "± 16794",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8097045858,
            "range": "± 19853986",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Leandro Pacheco",
            "username": "pacheco",
            "email": "contact@leandropacheco.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "e32108d44a4b7597b723c8728d7bf90229e09136",
          "message": "make the RISCV executor look at the optimized PIL for link information (#2232)\n\nThis PR modifies the RISCV executor to look into the optimized PIL for\nlookup/permutation definitions.\nIn particular, to figure out which selectors are assigned for each link,\nbut also with the intent of later integration with witgen (which needs\nthe identity ids for \"submachine calls\")",
          "timestamp": "2024-12-17T08:46:52Z",
          "url": "https://github.com/powdr-labs/powdr/commit/e32108d44a4b7597b723c8728d7bf90229e09136"
        },
        "date": 1734426195451,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6800,
            "range": "± 23",
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
            "value": 1078,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29408,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24598,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24491,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17330,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 779640,
            "range": "± 1563",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2685834,
            "range": "± 3376",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9390795,
            "range": "± 8653",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34491107,
            "range": "± 99411",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 136803243,
            "range": "± 164978",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3127,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2449,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2508,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1356,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65911,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252641,
            "range": "± 845",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035035,
            "range": "± 1610",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4868966,
            "range": "± 3077",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27665209,
            "range": "± 19612",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8130478164,
            "range": "± 24868012",
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
          "id": "516ae1c0baef167f27216009f1ca28bc0cf9cbeb",
          "message": "[WIP] Block machine processor",
          "timestamp": "2024-12-16T14:23:59Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/516ae1c0baef167f27216009f1ca28bc0cf9cbeb"
        },
        "date": 1734427784246,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7019,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 595,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1082,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29189,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24396,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24382,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17181,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786912,
            "range": "± 757",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2710535,
            "range": "± 4058",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9475588,
            "range": "± 12486",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34887877,
            "range": "± 58664",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138401607,
            "range": "± 384944",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3133,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2460,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66195,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252890,
            "range": "± 284",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031157,
            "range": "± 1197",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4915849,
            "range": "± 2857",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27758288,
            "range": "± 14887",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8180045332,
            "range": "± 30495427",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "contact@leandropacheco.com",
            "name": "Leandro Pacheco",
            "username": "pacheco"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "e32108d44a4b7597b723c8728d7bf90229e09136",
          "message": "make the RISCV executor look at the optimized PIL for link information (#2232)\n\nThis PR modifies the RISCV executor to look into the optimized PIL for\nlookup/permutation definitions.\nIn particular, to figure out which selectors are assigned for each link,\nbut also with the intent of later integration with witgen (which needs\nthe identity ids for \"submachine calls\")",
          "timestamp": "2024-12-17T08:46:52Z",
          "tree_id": "68fecd393e2a5710ab9b51b76dc9c3ff6f9cb794",
          "url": "https://github.com/powdr-labs/powdr/commit/e32108d44a4b7597b723c8728d7bf90229e09136"
        },
        "date": 1734427974297,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6994,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 646,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1102,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29497,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24793,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24758,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17621,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 803247,
            "range": "± 1101",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2751184,
            "range": "± 4503",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9616901,
            "range": "± 23734",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35233563,
            "range": "± 83105",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140243496,
            "range": "± 988024",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3192,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2469,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
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
            "value": 65661,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253328,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026421,
            "range": "± 1270",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4866870,
            "range": "± 3697",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27648807,
            "range": "± 209147",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8252091584,
            "range": "± 72115001",
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
          "id": "b8c9333d305bc74ff82e3218d26beec8515279c1",
          "message": "Lift some tests from nightly to pr-tests",
          "timestamp": "2024-12-17T09:17:13Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2216/commits/b8c9333d305bc74ff82e3218d26beec8515279c1"
        },
        "date": 1734428325843,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6808,
            "range": "± 11",
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
            "value": 1061,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28940,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24555,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24488,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17270,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791811,
            "range": "± 896",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2719896,
            "range": "± 3160",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9527570,
            "range": "± 12426",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34868905,
            "range": "± 87230",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139370069,
            "range": "± 451465",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3150,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2462,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 4",
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
            "value": 65516,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251696,
            "range": "± 289",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028558,
            "range": "± 1430",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4870481,
            "range": "± 3982",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27652580,
            "range": "± 28082",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8140865485,
            "range": "± 42964095",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Leo",
            "username": "leonardoalt",
            "email": "leo@powdrlabs.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "a5df8bc035bfcd77c8833fef3eb99f9068ff6bb8",
          "message": "Lift some tests from nightly to pr-tests (#2216)",
          "timestamp": "2024-12-17T09:50:58Z",
          "url": "https://github.com/powdr-labs/powdr/commit/a5df8bc035bfcd77c8833fef3eb99f9068ff6bb8"
        },
        "date": 1734430054589,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7008,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 605,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1094,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29113,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24349,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24303,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17212,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798398,
            "range": "± 3328",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2749188,
            "range": "± 10280",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9687125,
            "range": "± 50019",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35459695,
            "range": "± 210298",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141666600,
            "range": "± 660927",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3159,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2497,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2463,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65414,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251092,
            "range": "± 163",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027406,
            "range": "± 1698",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4886854,
            "range": "± 22308",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28162933,
            "range": "± 190909",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8804812572,
            "range": "± 68408360",
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
          "id": "71ee29c580fa9c2c42cb945bce0ca8388cd6e826",
          "message": "Effects to rust",
          "timestamp": "2024-12-17T09:17:13Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2229/commits/71ee29c580fa9c2c42cb945bce0ca8388cd6e826"
        },
        "date": 1734431137834,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6939,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 599,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1087,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29114,
            "range": "± 256",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24312,
            "range": "± 153",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24250,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17191,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 804946,
            "range": "± 8465",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2772349,
            "range": "± 6793",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9778172,
            "range": "± 80641",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35998130,
            "range": "± 574822",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140955676,
            "range": "± 1228530",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3224,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2532,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2473,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65638,
            "range": "± 227",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252446,
            "range": "± 243",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031681,
            "range": "± 3946",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4860542,
            "range": "± 13538",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27576719,
            "range": "± 44716",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8159764122,
            "range": "± 35690889",
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
          "id": "555ac0d2e19f27efac7cd69367b2080da46490b9",
          "message": "Remove reset instruction",
          "timestamp": "2024-12-17T09:17:13Z",
          "url": "https://github.com/powdr-labs/powdr/pull/1718/commits/555ac0d2e19f27efac7cd69367b2080da46490b9"
        },
        "date": 1734431232923,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6954,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 631,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1083,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29130,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24323,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24285,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17065,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786043,
            "range": "± 934",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2695696,
            "range": "± 3423",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9447359,
            "range": "± 20844",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34729086,
            "range": "± 89467",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138764908,
            "range": "± 364842",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3189,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2460,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2464,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1365,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66120,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253867,
            "range": "± 182",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036428,
            "range": "± 2339",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4888908,
            "range": "± 5680",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27648835,
            "range": "± 108482",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8234720194,
            "range": "± 52169378",
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
          "id": "71bd804d7cd2d5a4788fb83b70af33e9ae2a6a87",
          "message": "Block machine processor",
          "timestamp": "2024-12-17T09:17:13Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/71bd804d7cd2d5a4788fb83b70af33e9ae2a6a87"
        },
        "date": 1734431573552,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6800,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 626,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1129,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28975,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24308,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24251,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17261,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794422,
            "range": "± 939",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2737287,
            "range": "± 3130",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9595441,
            "range": "± 10855",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35247383,
            "range": "± 51084",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138785140,
            "range": "± 147325",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3124,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2466,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2445,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1373,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65784,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252242,
            "range": "± 227",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033084,
            "range": "± 1038",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4870607,
            "range": "± 3143",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27629682,
            "range": "± 25182",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8254207650,
            "range": "± 42600270",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "leo@powdrlabs.com",
            "name": "Leo",
            "username": "leonardoalt"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "a5df8bc035bfcd77c8833fef3eb99f9068ff6bb8",
          "message": "Lift some tests from nightly to pr-tests (#2216)",
          "timestamp": "2024-12-17T09:50:58Z",
          "tree_id": "af205775030009746e8d8cf77698b681f202df16",
          "url": "https://github.com/powdr-labs/powdr/commit/a5df8bc035bfcd77c8833fef3eb99f9068ff6bb8"
        },
        "date": 1734431744203,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6933,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 591,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1062,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29351,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24566,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24536,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17254,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798927,
            "range": "± 2426",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2745919,
            "range": "± 4252",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9751892,
            "range": "± 48298",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35104303,
            "range": "± 255396",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 143428392,
            "range": "± 2035286",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3209,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2534,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2506,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1373,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66804,
            "range": "± 94",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250947,
            "range": "± 187",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027998,
            "range": "± 1315",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4872904,
            "range": "± 25159",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28053154,
            "range": "± 354508",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8662625109,
            "range": "± 219759721",
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
          "id": "9e9fb9050ba587b8060daf4290157693edf944cd",
          "message": "Remove constant intermediates (#2163)\n\nSee [added\ntest](https://github.com/powdr-labs/powdr/blob/48de0e832e14da0e5dd5377bc75c6eea55c1bcc6/pilopt/tests/optimizer.rs#L34)\n\nThis propagates knowledge about constant intermediates forward in source\norder. We could run this backwards as well but I think in practice\nintermediates are defined before usage.\n\n---------\n\nCo-authored-by: chriseth <chris@ethereum.org>",
          "timestamp": "2024-12-17T10:33:59Z",
          "url": "https://github.com/powdr-labs/powdr/commit/9e9fb9050ba587b8060daf4290157693edf944cd"
        },
        "date": 1734432629030,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6842,
            "range": "± 15",
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
            "value": 1069,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28589,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 23917,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23880,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16917,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 787057,
            "range": "± 1650",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2709487,
            "range": "± 3985",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9460791,
            "range": "± 10095",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34800877,
            "range": "± 66121",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138497044,
            "range": "± 214086",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3117,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2467,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2445,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65646,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253233,
            "range": "± 187",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033350,
            "range": "± 1256",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4874999,
            "range": "± 8212",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27693726,
            "range": "± 79993",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8257629555,
            "range": "± 109883134",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "a68a20c9a4f05d997280ddb128fbfffe1ff55bb0",
          "message": "Effects to rust (#2229)\n\nFormats a vector of \"effects\" coming from the witgen solver into rust\ncode, compiles and loads it.\n\nSubmachine calls and receiving arguments will be done in another PR.\n\nThis code assumes `known` to be a padded bit vector ( #2230 ).\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-17T10:41:04Z",
          "url": "https://github.com/powdr-labs/powdr/commit/a68a20c9a4f05d997280ddb128fbfffe1ff55bb0"
        },
        "date": 1734433036500,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6938,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 585,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1101,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29246,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24503,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24490,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17341,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 828195,
            "range": "± 1456",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2807984,
            "range": "± 8577",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9598088,
            "range": "± 21103",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35097337,
            "range": "± 104356",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139585034,
            "range": "± 921998",
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
            "value": 2466,
            "range": "± 4",
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
            "value": 1371,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65526,
            "range": "± 126",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253196,
            "range": "± 280",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024185,
            "range": "± 1122",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4896706,
            "range": "± 4117",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27812174,
            "range": "± 153971",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8398085507,
            "range": "± 67397471",
            "unit": "ns/iter"
          }
        ]
      },
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
          "distinct": false,
          "id": "a68a20c9a4f05d997280ddb128fbfffe1ff55bb0",
          "message": "Effects to rust (#2229)\n\nFormats a vector of \"effects\" coming from the witgen solver into rust\ncode, compiles and loads it.\n\nSubmachine calls and receiving arguments will be done in another PR.\n\nThis code assumes `known` to be a padded bit vector ( #2230 ).\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-17T10:41:04Z",
          "tree_id": "7ed939b6d91ae50af226c33c9edd95769a675579",
          "url": "https://github.com/powdr-labs/powdr/commit/a68a20c9a4f05d997280ddb128fbfffe1ff55bb0"
        },
        "date": 1734436231986,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7024,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 638,
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
            "value": 28815,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24354,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24210,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17154,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 824466,
            "range": "± 1220",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2849190,
            "range": "± 3535",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9880039,
            "range": "± 27344",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36077083,
            "range": "± 206583",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142621886,
            "range": "± 131811",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3115,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2479,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2441,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1367,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65777,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251222,
            "range": "± 217",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030834,
            "range": "± 1463",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4874278,
            "range": "± 3339",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27689618,
            "range": "± 36744",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8168043356,
            "range": "± 30233187",
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
          "id": "d1e349bbb529afbce4b1c665ede096893872380f",
          "message": "Support machine calls.",
          "timestamp": "2024-12-17T11:14:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2241/commits/d1e349bbb529afbce4b1c665ede096893872380f"
        },
        "date": 1734437444488,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6948,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 621,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1081,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30524,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25100,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25313,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17903,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 808045,
            "range": "± 1008",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2771497,
            "range": "± 3130",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9676490,
            "range": "± 17942",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35505930,
            "range": "± 78875",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140432144,
            "range": "± 322072",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3115,
            "range": "± 3",
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
            "value": 2442,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65574,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250599,
            "range": "± 191",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027187,
            "range": "± 763",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4843405,
            "range": "± 5155",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27623290,
            "range": "± 56612",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8328333790,
            "range": "± 48734051",
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
          "id": "1c581c395d83f9cf18d1a91c3df9866580e910c5",
          "message": "pilopt: optimize until fixpoint",
          "timestamp": "2024-12-17T11:14:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2225/commits/1c581c395d83f9cf18d1a91c3df9866580e910c5"
        },
        "date": 1734439007686,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6956,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1093,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29286,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25154,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24385,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17327,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 808485,
            "range": "± 982",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2762234,
            "range": "± 4193",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9666394,
            "range": "± 14611",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35274516,
            "range": "± 105766",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139187356,
            "range": "± 109737",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3138,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2518,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65623,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252947,
            "range": "± 267",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032520,
            "range": "± 1332",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4866381,
            "range": "± 6052",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27563800,
            "range": "± 26548",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8197606600,
            "range": "± 37508561",
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
          "id": "95f8b2f84f1af8fa26c14b93b07fddea4cd9cc2b",
          "message": "Block machine processor",
          "timestamp": "2024-12-17T11:14:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2226/commits/95f8b2f84f1af8fa26c14b93b07fddea4cd9cc2b"
        },
        "date": 1734441186451,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6895,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 593,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1071,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29504,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24801,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24710,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17531,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 795971,
            "range": "± 738",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2748373,
            "range": "± 3801",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9627674,
            "range": "± 18909",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35120144,
            "range": "± 83433",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140149708,
            "range": "± 451319",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3114,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2444,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1359,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66209,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252263,
            "range": "± 298",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029228,
            "range": "± 1892",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4862931,
            "range": "± 4544",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27693930,
            "range": "± 24963",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8286646319,
            "range": "± 29297249",
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
          "id": "deba7da699c0c55acd16c7e41840f643b0b1ecb4",
          "message": "[WIP] Call JIT",
          "timestamp": "2024-12-17T11:14:46Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2242/commits/deba7da699c0c55acd16c7e41840f643b0b1ecb4"
        },
        "date": 1734443058312,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6956,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 588,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1073,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29257,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24313,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24300,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17234,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 804034,
            "range": "± 1349",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2759541,
            "range": "± 2889",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9625129,
            "range": "± 11349",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35189916,
            "range": "± 41011",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139713223,
            "range": "± 134123",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3161,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2469,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2478,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1374,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67060,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 255968,
            "range": "± 407",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1044051,
            "range": "± 1065",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4909810,
            "range": "± 4627",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27738447,
            "range": "± 30736",
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
          "id": "e2131f501ffe3a80be40c4df938889ef5ac57263",
          "message": "Block machine processor (#2226)\n\nThis PR adds a basic block machine processor. Currently, we assume a\nrectangular block shape and just iterate over all rows and identities of\nthe block until no more progress is made. This is sufficient to generate\ncode for Poseidon.",
          "timestamp": "2024-12-17T13:31:03Z",
          "url": "https://github.com/powdr-labs/powdr/commit/e2131f501ffe3a80be40c4df938889ef5ac57263"
        },
        "date": 1734443221815,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6923,
            "range": "± 7",
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
            "value": 1062,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28808,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24151,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24129,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17038,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 799148,
            "range": "± 705",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2751917,
            "range": "± 3129",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9631499,
            "range": "± 12449",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35279261,
            "range": "± 56973",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139883303,
            "range": "± 364061",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3142,
            "range": "± 4",
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
            "value": 2455,
            "range": "± 4",
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
            "value": 65311,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250638,
            "range": "± 170",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024176,
            "range": "± 1008",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4862372,
            "range": "± 3487",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27593128,
            "range": "± 15074",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8130810304,
            "range": "± 30902101",
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
          "id": "0c2ae615a30b81f63dcb312ba319978e4cd22eb9",
          "message": "prevent instr fail from being optimized away",
          "timestamp": "2024-12-17T13:37:11Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2243/commits/0c2ae615a30b81f63dcb312ba319978e4cd22eb9"
        },
        "date": 1734443716091,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6883,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1135,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28912,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24194,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24180,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17106,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 805092,
            "range": "± 1023",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2755797,
            "range": "± 2629",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9652377,
            "range": "± 10312",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35191274,
            "range": "± 41680",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139853075,
            "range": "± 235322",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3154,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2495,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2459,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1357,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65443,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253498,
            "range": "± 340",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029829,
            "range": "± 1140",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4867225,
            "range": "± 4621",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27652397,
            "range": "± 18682",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8195908740,
            "range": "± 19186856",
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
          "id": "e2131f501ffe3a80be40c4df938889ef5ac57263",
          "message": "Block machine processor (#2226)\n\nThis PR adds a basic block machine processor. Currently, we assume a\nrectangular block shape and just iterate over all rows and identities of\nthe block until no more progress is made. This is sufficient to generate\ncode for Poseidon.",
          "timestamp": "2024-12-17T13:31:03Z",
          "tree_id": "e22d64917cef34a0dc722a4d21e59316a8ca3bf8",
          "url": "https://github.com/powdr-labs/powdr/commit/e2131f501ffe3a80be40c4df938889ef5ac57263"
        },
        "date": 1734445186106,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6946,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 579,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1073,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28929,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24183,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24143,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17115,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798467,
            "range": "± 1677",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2746686,
            "range": "± 6275",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9682452,
            "range": "± 43678",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35667379,
            "range": "± 245130",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140926815,
            "range": "± 901472",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3113,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2457,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2440,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1376,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66085,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253423,
            "range": "± 1590",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032556,
            "range": "± 2042",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4937625,
            "range": "± 29743",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28136219,
            "range": "± 179887",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8776289344,
            "range": "± 34938910",
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
          "id": "9ce0eea4fa53c175ed71adb26adeb7ccd61828aa",
          "message": "Introduce concept of assignment.",
          "timestamp": "2024-12-17T14:03:36Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2244/commits/9ce0eea4fa53c175ed71adb26adeb7ccd61828aa"
        },
        "date": 1734445523803,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7161,
            "range": "± 14",
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
            "value": 1103,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29462,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24609,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24572,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17322,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 797827,
            "range": "± 1563",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2752973,
            "range": "± 2782",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9707006,
            "range": "± 24474",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35275171,
            "range": "± 108297",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139950602,
            "range": "± 713567",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3272,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2619,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2513,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1386,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65248,
            "range": "± 124",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250345,
            "range": "± 270",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1022240,
            "range": "± 729",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4884514,
            "range": "± 20078",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27889820,
            "range": "± 107677",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8655121900,
            "range": "± 45517457",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "Leandro Pacheco",
            "username": "pacheco",
            "email": "contact@leandropacheco.com"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "38d26f4494d419f20c508fe6a8feafb31a8b3aa5",
          "message": "prevent instr fail from being optimized away (#2243)",
          "timestamp": "2024-12-17T14:12:04Z",
          "url": "https://github.com/powdr-labs/powdr/commit/38d26f4494d419f20c508fe6a8feafb31a8b3aa5"
        },
        "date": 1734445696181,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6876,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 632,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1075,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28814,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24373,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24189,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17084,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794026,
            "range": "± 1129",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2722163,
            "range": "± 3666",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9580810,
            "range": "± 32937",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35204811,
            "range": "± 276724",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140121587,
            "range": "± 880306",
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
            "value": 2457,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2443,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1370,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65459,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252719,
            "range": "± 212",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032378,
            "range": "± 971",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4883863,
            "range": "± 8079",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27926757,
            "range": "± 278663",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8572443847,
            "range": "± 72923775",
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
          "id": "85689817f5457ad625d1e3c184e7b61a1cb04b07",
          "message": "pilopt: optimize until fixpoint",
          "timestamp": "2024-12-17T14:03:36Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2225/commits/85689817f5457ad625d1e3c184e7b61a1cb04b07"
        },
        "date": 1734446135120,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7073,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 589,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1099,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30318,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25341,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25290,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17897,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 804206,
            "range": "± 596",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2779979,
            "range": "± 4773",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9704144,
            "range": "± 18468",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35518788,
            "range": "± 88164",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140508165,
            "range": "± 251957",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3128,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2459,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2449,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66060,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252030,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1034048,
            "range": "± 843",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4879012,
            "range": "± 4417",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27669842,
            "range": "± 61507",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8403911548,
            "range": "± 35876351",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "contact@leandropacheco.com",
            "name": "Leandro Pacheco",
            "username": "pacheco"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "38d26f4494d419f20c508fe6a8feafb31a8b3aa5",
          "message": "prevent instr fail from being optimized away (#2243)",
          "timestamp": "2024-12-17T14:12:04Z",
          "tree_id": "19d11965756b9fb0ca16a6ea22bc807d906ba93b",
          "url": "https://github.com/powdr-labs/powdr/commit/38d26f4494d419f20c508fe6a8feafb31a8b3aa5"
        },
        "date": 1734447718866,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6976,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 590,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1072,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29065,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24344,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24266,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17172,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798497,
            "range": "± 3436",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2755329,
            "range": "± 3333",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9663896,
            "range": "± 27606",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35257468,
            "range": "± 88184",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141085280,
            "range": "± 1011892",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3142,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2460,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2464,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1377,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66535,
            "range": "± 378",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252740,
            "range": "± 546",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1037121,
            "range": "± 6352",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4893276,
            "range": "± 8808",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27693113,
            "range": "± 91704",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8274155467,
            "range": "± 30038390",
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
          "id": "8c1c85265a09b158ffafae1e5e9b6d444c5ca40b",
          "message": "[WIP] Call JIT",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2242/commits/8c1c85265a09b158ffafae1e5e9b6d444c5ca40b"
        },
        "date": 1734452327313,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 8127,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 753,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1325,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 36723,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 30009,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 30118,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 20874,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 908829,
            "range": "± 1335",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 3135857,
            "range": "± 9224",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 10941471,
            "range": "± 32618",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 39688993,
            "range": "± 59883",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 154167569,
            "range": "± 1363829",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3465,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2686,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2643,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1382,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 75268,
            "range": "± 535",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 283826,
            "range": "± 670",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1130792,
            "range": "± 2535",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 5204424,
            "range": "± 5321",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28765827,
            "range": "± 30129",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8435334055,
            "range": "± 39987121",
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
          "id": "aedcc63a587cfada7636e68f16240743a2c47a39",
          "message": "Support machine calls.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2241/commits/aedcc63a587cfada7636e68f16240743a2c47a39"
        },
        "date": 1734452445723,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6837,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 575,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1076,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29236,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24576,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24516,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17484,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794545,
            "range": "± 1424",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2730833,
            "range": "± 5217",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9553333,
            "range": "± 26897",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34949370,
            "range": "± 200456",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139077744,
            "range": "± 802253",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3151,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2471,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2465,
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
            "value": 65055,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251776,
            "range": "± 285",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1023531,
            "range": "± 1162",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885683,
            "range": "± 39993",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27739221,
            "range": "± 199701",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8396782665,
            "range": "± 59138274",
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
          "id": "bc8439b1a04b6e9ea7fab64fe0d4fccd2c58cb74",
          "message": "[WIP] Call JIT",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2242/commits/bc8439b1a04b6e9ea7fab64fe0d4fccd2c58cb74"
        },
        "date": 1734454619679,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6870,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 594,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1082,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29600,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24738,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24738,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17442,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 796586,
            "range": "± 1558",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2744983,
            "range": "± 4075",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9649020,
            "range": "± 44590",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35307563,
            "range": "± 122307",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140042069,
            "range": "± 450775",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3143,
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
            "value": 2470,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1380,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65445,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252538,
            "range": "± 206",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035876,
            "range": "± 861",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4905630,
            "range": "± 25427",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28134384,
            "range": "± 91718",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8856902602,
            "range": "± 52110595",
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
          "id": "f57a36731120e93e90b8565734eb080a60d7f6f3",
          "message": "Profile jit compilation separately.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2248/commits/f57a36731120e93e90b8565734eb080a60d7f6f3"
        },
        "date": 1734454807541,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6951,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 597,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1094,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29245,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24435,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24401,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17291,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800761,
            "range": "± 1045",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2762733,
            "range": "± 2843",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9666711,
            "range": "± 8870",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35285903,
            "range": "± 35949",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140175711,
            "range": "± 101084",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3146,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2481,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2461,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65728,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252328,
            "range": "± 286",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027042,
            "range": "± 1324",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4863283,
            "range": "± 3734",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27654316,
            "range": "± 24814",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8294160367,
            "range": "± 39904086",
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
          "id": "5131ad616561bc7954c390fe8ee38ef7a905b15d",
          "message": "Use truncate instead of pop.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2247/commits/5131ad616561bc7954c390fe8ee38ef7a905b15d"
        },
        "date": 1734454808624,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6945,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 617,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1086,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29092,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24305,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24411,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17243,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794310,
            "range": "± 965",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2744828,
            "range": "± 3309",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9561099,
            "range": "± 23083",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35073671,
            "range": "± 113112",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140759277,
            "range": "± 731134",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3247,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2463,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1379,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65934,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253002,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033452,
            "range": "± 1298",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4895830,
            "range": "± 30724",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27888741,
            "range": "± 114923",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8733733287,
            "range": "± 61812624",
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
          "id": "b917170c7d5e49bb31ea0bce664c9b94bc893c17",
          "message": "[WIP] Call JIT",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2242/commits/b917170c7d5e49bb31ea0bce664c9b94bc893c17"
        },
        "date": 1734456827372,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6853,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1066,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28942,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24070,
            "range": "± 608",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24011,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17049,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 777638,
            "range": "± 1670",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2691189,
            "range": "± 4450",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9435261,
            "range": "± 24594",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34619484,
            "range": "± 170334",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137563938,
            "range": "± 589690",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3159,
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
            "value": 2453,
            "range": "± 4",
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
            "value": 65682,
            "range": "± 224",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252035,
            "range": "± 223",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026365,
            "range": "± 1066",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4848257,
            "range": "± 237917",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27638179,
            "range": "± 98695",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8514830289,
            "range": "± 48358802",
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
          "id": "40478ecd072719100ae5449e69f08ef4729b8d10",
          "message": "Fix bit operations.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2249/commits/40478ecd072719100ae5449e69f08ef4729b8d10"
        },
        "date": 1734457018224,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6904,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 603,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1106,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29374,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24576,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24617,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17250,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800156,
            "range": "± 1114",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2761318,
            "range": "± 2729",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9764788,
            "range": "± 14600",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36041149,
            "range": "± 70352",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140771946,
            "range": "± 1329178",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3129,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2474,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2493,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65735,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253230,
            "range": "± 215",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029735,
            "range": "± 2767",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4875208,
            "range": "± 3645",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27638418,
            "range": "± 31454",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8375709973,
            "range": "± 76160927",
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
          "id": "31205a4b2e15a40ef843ff15a19035b63bbd5fd3",
          "message": "default getenv",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2250/commits/31205a4b2e15a40ef843ff15a19035b63bbd5fd3"
        },
        "date": 1734457363875,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6993,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1089,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29424,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24607,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24575,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17406,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800585,
            "range": "± 994",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2762167,
            "range": "± 3185",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9675553,
            "range": "± 22175",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35450711,
            "range": "± 35447",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139966341,
            "range": "± 236368",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3226,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2539,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2470,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1378,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65406,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 249994,
            "range": "± 235",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024028,
            "range": "± 760",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4838629,
            "range": "± 2916",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27650171,
            "range": "± 57391",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8475479272,
            "range": "± 21893768",
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
          "id": "33e55fa8c7b56db1881d1e36149d85a0c64795a7",
          "message": "Reserved space for prover data in memory layout",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2251/commits/33e55fa8c7b56db1881d1e36149d85a0c64795a7"
        },
        "date": 1734478536152,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6780,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 586,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1037,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29314,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24467,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24349,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17234,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786015,
            "range": "± 963",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2720556,
            "range": "± 4719",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9495485,
            "range": "± 21605",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35083856,
            "range": "± 245725",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139416483,
            "range": "± 681007",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3133,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2468,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1369,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66328,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251778,
            "range": "± 215",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1025425,
            "range": "± 888",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4910813,
            "range": "± 17443",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28153117,
            "range": "± 220436",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8618192669,
            "range": "± 87810171",
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
          "id": "34bcc61fc432fc9f7c197760d440c2fa9a6f0e63",
          "message": "default getenv",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2250/commits/34bcc61fc432fc9f7c197760d440c2fa9a6f0e63"
        },
        "date": 1734510103488,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6933,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 585,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1059,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 31032,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 26132,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 26032,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18575,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 832076,
            "range": "± 1084",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2810246,
            "range": "± 3101",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9828203,
            "range": "± 10354",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35940631,
            "range": "± 31966",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141993104,
            "range": "± 148904",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3119,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2453,
            "range": "± 3",
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
            "value": 1358,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65788,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252885,
            "range": "± 205",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029245,
            "range": "± 1390",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4887600,
            "range": "± 3647",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27689263,
            "range": "± 25549",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8224045124,
            "range": "± 40603320",
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
          "id": "95007d76533036f122485b9cfe2f61fb64134399",
          "message": "Implement Neg for FieldElement.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2252/commits/95007d76533036f122485b9cfe2f61fb64134399"
        },
        "date": 1734513212405,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6927,
            "range": "± 11",
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
            "value": 1099,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30027,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25078,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25040,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17670,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 802778,
            "range": "± 1316",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2784863,
            "range": "± 9316",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9765549,
            "range": "± 27586",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35499865,
            "range": "± 58050",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141064639,
            "range": "± 621484",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3123,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2449,
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
            "value": 65839,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251665,
            "range": "± 205",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031089,
            "range": "± 859",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4883636,
            "range": "± 5596",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27660512,
            "range": "± 78416",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8353712887,
            "range": "± 22349615",
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
          "id": "1a129e4ad977523815501679a57ce6ac33106ace",
          "message": "Specialized code for goldilocks.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2253/commits/1a129e4ad977523815501679a57ce6ac33106ace"
        },
        "date": 1734519205625,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6781,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 578,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1067,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28794,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24059,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24013,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16981,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790358,
            "range": "± 1028",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2726162,
            "range": "± 2602",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9542976,
            "range": "± 10477",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35046803,
            "range": "± 47757",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138896630,
            "range": "± 149607",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3134,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2459,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2444,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66421,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254009,
            "range": "± 265",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1036477,
            "range": "± 1695",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4902245,
            "range": "± 2999",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27746932,
            "range": "± 20253",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8327903661,
            "range": "± 73285110",
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
          "id": "1009078b596e3009f2894bebc793132cfe956a00",
          "message": "Specialized code for goldilocks.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2253/commits/1009078b596e3009f2894bebc793132cfe956a00"
        },
        "date": 1734520337059,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6885,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 572,
            "range": "± 0",
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
            "value": 28970,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24200,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24118,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17089,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798747,
            "range": "± 2248",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2756446,
            "range": "± 3527",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9649371,
            "range": "± 28487",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35547422,
            "range": "± 247216",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140277091,
            "range": "± 420404",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3181,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2500,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2466,
            "range": "± 4",
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
            "value": 66658,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254552,
            "range": "± 236",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033091,
            "range": "± 1033",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4865960,
            "range": "± 7099",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28106322,
            "range": "± 167175",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8436447493,
            "range": "± 113498849",
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
          "id": "d93ef1cb9e3765ad0cbf56f409aece411b2df161",
          "message": "Implement Neg for FieldElement.",
          "timestamp": "2024-12-17T14:46:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2252/commits/d93ef1cb9e3765ad0cbf56f409aece411b2df161"
        },
        "date": 1734524132609,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6874,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 581,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1061,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29210,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24339,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24351,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17225,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 814117,
            "range": "± 1139",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2810311,
            "range": "± 3439",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9809821,
            "range": "± 14093",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35658022,
            "range": "± 50450",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141157893,
            "range": "± 110256",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3127,
            "range": "± 13",
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
            "value": 2445,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65583,
            "range": "± 124",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251453,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029738,
            "range": "± 1081",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4869854,
            "range": "± 4518",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27647297,
            "range": "± 22921",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8318964371,
            "range": "± 48167881",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "taikoon",
            "username": "taikoonwang",
            "email": "taikoonwang@proton.me"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "1a9c99d2320d02d72a61f1c31b72d1b55fa08a14",
          "message": "chore: remove duplicate words (#2227)",
          "timestamp": "2024-12-18T12:14:36Z",
          "url": "https://github.com/powdr-labs/powdr/commit/1a9c99d2320d02d72a61f1c31b72d1b55fa08a14"
        },
        "date": 1734525036899,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7036,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 582,
            "range": "± 1",
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
            "value": 29547,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24658,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24618,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17361,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 810950,
            "range": "± 1249",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2791620,
            "range": "± 3652",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9740345,
            "range": "± 10067",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35779823,
            "range": "± 105872",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141465448,
            "range": "± 225770",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3128,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2458,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2455,
            "range": "± 6",
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
            "value": 66115,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252545,
            "range": "± 332",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032305,
            "range": "± 1286",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4880330,
            "range": "± 3168",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27619523,
            "range": "± 18458",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8211468401,
            "range": "± 34429900",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "a3d9f38682d428da59ce0b970c4e33499f0653bf",
          "message": "Implement Neg for FieldElement. (#2252)",
          "timestamp": "2024-12-18T12:44:18Z",
          "url": "https://github.com/powdr-labs/powdr/commit/a3d9f38682d428da59ce0b970c4e33499f0653bf"
        },
        "date": 1734526849214,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6765,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1071,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28817,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24134,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24045,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17060,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 796135,
            "range": "± 3366",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2751168,
            "range": "± 6599",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9653320,
            "range": "± 35926",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35719832,
            "range": "± 197565",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141573031,
            "range": "± 719724",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3436,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2652,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2604,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1364,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66577,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 253982,
            "range": "± 296",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035174,
            "range": "± 3179",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4881534,
            "range": "± 5740",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28182301,
            "range": "± 154989",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8785711085,
            "range": "± 47682831",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "leo@powdrlabs.com",
            "name": "Leo",
            "username": "leonardoalt"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "c8dcd07b5977c2116489d14721beeba445f92d50",
          "message": "default getenv (#2250)",
          "timestamp": "2024-12-18T12:24:53Z",
          "tree_id": "64e3281c5072c016ff5571d9af55bf54aea322cc",
          "url": "https://github.com/powdr-labs/powdr/commit/c8dcd07b5977c2116489d14721beeba445f92d50"
        },
        "date": 1734527864306,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6853,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 602,
            "range": "± 0",
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
            "value": 28942,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24232,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24178,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17144,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791068,
            "range": "± 1400",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2721722,
            "range": "± 3339",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9526840,
            "range": "± 12678",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34964874,
            "range": "± 51894",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138824446,
            "range": "± 415672",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3130,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2457,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2460,
            "range": "± 2",
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
            "value": 67874,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 260544,
            "range": "± 259",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1055897,
            "range": "± 1474",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4938631,
            "range": "± 7751",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27821411,
            "range": "± 46305",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8204662260,
            "range": "± 23259416",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "388979952819bccbbed4a82bc7b928e295a04222",
          "message": "Profile jit compilation separately. (#2248)",
          "timestamp": "2024-12-18T13:06:27Z",
          "url": "https://github.com/powdr-labs/powdr/commit/388979952819bccbbed4a82bc7b928e295a04222"
        },
        "date": 1734528156111,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7003,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 605,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1071,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29644,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24966,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24853,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17479,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800477,
            "range": "± 1588",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2747418,
            "range": "± 3689",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9598183,
            "range": "± 14212",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35174124,
            "range": "± 46876",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139712219,
            "range": "± 399605",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3206,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2539,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2503,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1380,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66088,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252540,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1033795,
            "range": "± 1338",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4895852,
            "range": "± 3558",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27742945,
            "range": "± 33190",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8222083084,
            "range": "± 49658347",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "bdfa4e1102c4d5d9a360f24646fd1f9af31b3d4b",
          "message": "Use truncate instead of pop. (#2247)",
          "timestamp": "2024-12-18T13:07:28Z",
          "url": "https://github.com/powdr-labs/powdr/commit/bdfa4e1102c4d5d9a360f24646fd1f9af31b3d4b"
        },
        "date": 1734528212627,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6916,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1065,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29129,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24326,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24292,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17279,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794940,
            "range": "± 1099",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2750416,
            "range": "± 3694",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9607910,
            "range": "± 20312",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35111545,
            "range": "± 111713",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139583336,
            "range": "± 1372217",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3129,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2458,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1377,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65774,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252592,
            "range": "± 162",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028901,
            "range": "± 1368",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4881194,
            "range": "± 6347",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27861561,
            "range": "± 167324",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8408593770,
            "range": "± 96607724",
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
          "id": "465f698b6ab10eae3b12528cb01b5647d19644dd",
          "message": "Specialized code for goldilocks.",
          "timestamp": "2024-12-18T13:11:12Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2253/commits/465f698b6ab10eae3b12528cb01b5647d19644dd"
        },
        "date": 1734528585540,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6927,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 581,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1064,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29399,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24484,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24466,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17344,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 796499,
            "range": "± 1095",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2748766,
            "range": "± 3359",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9598616,
            "range": "± 18854",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35186541,
            "range": "± 138533",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139737456,
            "range": "± 784874",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3136,
            "range": "± 3",
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
            "value": 2460,
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
            "value": 66081,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254267,
            "range": "± 252",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032931,
            "range": "± 1004",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4883034,
            "range": "± 20324",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27796167,
            "range": "± 112778",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8403545459,
            "range": "± 99172625",
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
          "id": "4bed6536888c2cb396da04c36b07c0b8c1995b2b",
          "message": "Fix bit operations.",
          "timestamp": "2024-12-18T13:11:12Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2249/commits/4bed6536888c2cb396da04c36b07c0b8c1995b2b"
        },
        "date": 1734529289227,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6808,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 578,
            "range": "± 0",
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
            "value": 29031,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24313,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24287,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17152,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 793053,
            "range": "± 929",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2734863,
            "range": "± 1918",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9585114,
            "range": "± 15458",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35186568,
            "range": "± 37389",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139430069,
            "range": "± 208206",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3168,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2451,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2445,
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
            "value": 66253,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254722,
            "range": "± 514",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1044463,
            "range": "± 852",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4925097,
            "range": "± 4475",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27776058,
            "range": "± 17805",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8241636098,
            "range": "± 26012073",
            "unit": "ns/iter"
          }
        ]
      },
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
          "distinct": false,
          "id": "a3d9f38682d428da59ce0b970c4e33499f0653bf",
          "message": "Implement Neg for FieldElement. (#2252)",
          "timestamp": "2024-12-18T12:44:18Z",
          "tree_id": "02791f682bd32104c90f0f106d0620df3b6d331f",
          "url": "https://github.com/powdr-labs/powdr/commit/a3d9f38682d428da59ce0b970c4e33499f0653bf"
        },
        "date": 1734529328960,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6899,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 579,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1082,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28830,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24203,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24034,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17063,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786552,
            "range": "± 1071",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2709488,
            "range": "± 6699",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9469349,
            "range": "± 14421",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34726801,
            "range": "± 33207",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138425651,
            "range": "± 192326",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3130,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2462,
            "range": "± 5",
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
            "value": 1377,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65393,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251141,
            "range": "± 264",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027734,
            "range": "± 1332",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4877359,
            "range": "± 11192",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27922981,
            "range": "± 789712",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8234134992,
            "range": "± 129982005",
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
          "id": "a529b138dcdee32f5b9100b1676428b07c4919c3",
          "message": "Rename `_start` to `_powdr_start`",
          "timestamp": "2024-12-18T13:11:12Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2254/commits/a529b138dcdee32f5b9100b1676428b07c4919c3"
        },
        "date": 1734529653661,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6931,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1049,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28939,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24226,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24213,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17127,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800508,
            "range": "± 1250",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2754309,
            "range": "± 3238",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9666045,
            "range": "± 32546",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35771747,
            "range": "± 237325",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142525931,
            "range": "± 948970",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3131,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2458,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2448,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1375,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65147,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 249939,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028186,
            "range": "± 1148",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4891582,
            "range": "± 11090",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28085177,
            "range": "± 167356",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8893478405,
            "range": "± 75043314",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "bdfa4e1102c4d5d9a360f24646fd1f9af31b3d4b",
          "message": "Use truncate instead of pop. (#2247)",
          "timestamp": "2024-12-18T13:07:28Z",
          "tree_id": "bb35741ddb232810fe3a571ef5aaa7df63ed262a",
          "url": "https://github.com/powdr-labs/powdr/commit/bdfa4e1102c4d5d9a360f24646fd1f9af31b3d4b"
        },
        "date": 1734530754679,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6868,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1059,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29352,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24615,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24529,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17366,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 788618,
            "range": "± 610",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2713189,
            "range": "± 2174",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9486543,
            "range": "± 11822",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34826738,
            "range": "± 44569",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138477587,
            "range": "± 150527",
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
            "value": 2458,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2445,
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
            "value": 65411,
            "range": "± 134",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251695,
            "range": "± 229",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028802,
            "range": "± 1420",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4880407,
            "range": "± 4122",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27658723,
            "range": "± 58995",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8159567346,
            "range": "± 32515244",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "d98b7ebb58f0f2b88f3f9e68d562b63d57cda185",
          "message": "Fix bit operations. (#2249)\n\nThis fixes a bug in witjitgen where the bit operations used for masking\nin bit-decomposition would use field elements as types for the masks.\nThe problem is that we sometimes mask using masks outside the field and\nthe proper type for masks is FieldElement::Integer.",
          "timestamp": "2024-12-18T13:56:29Z",
          "url": "https://github.com/powdr-labs/powdr/commit/d98b7ebb58f0f2b88f3f9e68d562b63d57cda185"
        },
        "date": 1734531214235,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6803,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 582,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1082,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28972,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24197,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24257,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17000,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 801936,
            "range": "± 1343",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2769100,
            "range": "± 4441",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9671697,
            "range": "± 30093",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35493772,
            "range": "± 129773",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140754293,
            "range": "± 545415",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3142,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2535,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2455,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1363,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65916,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252215,
            "range": "± 163",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031757,
            "range": "± 1424",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4922906,
            "range": "± 14476",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27741677,
            "range": "± 77027",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8592284832,
            "range": "± 43980460",
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
          "id": "95c96bb6acbc2561be00b21f186c301431ac57a7",
          "message": "Rename `_start` to `_powdr_start` (#2254)\n\nShould fix #2220 \ncc @gballet",
          "timestamp": "2024-12-18T14:21:00Z",
          "url": "https://github.com/powdr-labs/powdr/commit/95c96bb6acbc2561be00b21f186c301431ac57a7"
        },
        "date": 1734532647141,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 8006,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1145,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 32857,
            "range": "± 693",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 26803,
            "range": "± 413",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 26489,
            "range": "± 350",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18660,
            "range": "± 297",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 852113,
            "range": "± 11501",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2960161,
            "range": "± 46716",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 10178943,
            "range": "± 153154",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 37798830,
            "range": "± 442351",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 148337247,
            "range": "± 2775788",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3312,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2690,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2583,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1424,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67287,
            "range": "± 1262",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 264100,
            "range": "± 4574",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1089465,
            "range": "± 25232",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 5043163,
            "range": "± 83948",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28912243,
            "range": "± 482587",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8391000701,
            "range": "± 72237963",
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
          "id": "3e2268f339f8e7ec0b0fe4e3e20c5fac05561eed",
          "message": "Reserved space for prover data in memory layout",
          "timestamp": "2024-12-18T13:34:13Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2251/commits/3e2268f339f8e7ec0b0fe4e3e20c5fac05561eed"
        },
        "date": 1734532800482,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6953,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 594,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1052,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29000,
            "range": "± 187",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24139,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24108,
            "range": "± 376",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17016,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 779459,
            "range": "± 6904",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2677536,
            "range": "± 18373",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9359318,
            "range": "± 32287",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34387909,
            "range": "± 574922",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 136416234,
            "range": "± 531808",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3174,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2467,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2455,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65269,
            "range": "± 256",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250456,
            "range": "± 1164",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1030746,
            "range": "± 6538",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885279,
            "range": "± 26301",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27826012,
            "range": "± 612828",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8247584614,
            "range": "± 54781119",
            "unit": "ns/iter"
          }
        ]
      },
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
          "distinct": false,
          "id": "d98b7ebb58f0f2b88f3f9e68d562b63d57cda185",
          "message": "Fix bit operations. (#2249)\n\nThis fixes a bug in witjitgen where the bit operations used for masking\nin bit-decomposition would use field elements as types for the masks.\nThe problem is that we sometimes mask using masks outside the field and\nthe proper type for masks is FieldElement::Integer.",
          "timestamp": "2024-12-18T13:56:29Z",
          "tree_id": "65110b3cb1f0e3999943c446c6cea195a43315a2",
          "url": "https://github.com/powdr-labs/powdr/commit/d98b7ebb58f0f2b88f3f9e68d562b63d57cda185"
        },
        "date": 1734533007757,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7035,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 653,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1142,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29495,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24763,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24616,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17418,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798293,
            "range": "± 1439",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2755918,
            "range": "± 3706",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9638297,
            "range": "± 22508",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35510744,
            "range": "± 261209",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141195594,
            "range": "± 539587",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3183,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2467,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2450,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65477,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251676,
            "range": "± 186",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1026676,
            "range": "± 1565",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4889638,
            "range": "± 13237",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28013427,
            "range": "± 121043",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8566008584,
            "range": "± 90470830",
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
          "id": "95c96bb6acbc2561be00b21f186c301431ac57a7",
          "message": "Rename `_start` to `_powdr_start` (#2254)\n\nShould fix #2220 \ncc @gballet",
          "timestamp": "2024-12-18T14:21:00Z",
          "tree_id": "d61d01e11ca03de1b8a6bbf29d9e74c99a670df8",
          "url": "https://github.com/powdr-labs/powdr/commit/95c96bb6acbc2561be00b21f186c301431ac57a7"
        },
        "date": 1734534859143,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6933,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 592,
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
            "value": 29307,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24620,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24431,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17443,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 815931,
            "range": "± 860",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2821780,
            "range": "± 3175",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9847489,
            "range": "± 15439",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35944762,
            "range": "± 97546",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142285471,
            "range": "± 288263",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3123,
            "range": "± 16",
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
            "value": 2465,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1377,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66365,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 259080,
            "range": "± 209",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1040192,
            "range": "± 1202",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4884776,
            "range": "± 5041",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27765394,
            "range": "± 20076",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8137614151,
            "range": "± 30564144",
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
          "id": "7dfb0094855e46d2334d8fbb6f8fc3813854b518",
          "message": "Specialized code for goldilocks.",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2253/commits/7dfb0094855e46d2334d8fbb6f8fc3813854b518"
        },
        "date": 1734536400115,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6854,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 589,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1077,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29749,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24839,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24515,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17388,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 810550,
            "range": "± 1282",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2789963,
            "range": "± 4767",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9807934,
            "range": "± 53833",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36039866,
            "range": "± 268622",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 144075325,
            "range": "± 901053",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3310,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2451,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2447,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65393,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252961,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1031606,
            "range": "± 1225",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4900156,
            "range": "± 28406",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28451691,
            "range": "± 275336",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8973617782,
            "range": "± 79201613",
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
          "id": "b33f385c544c1b7482b60ef8e69b5eae04c504cd",
          "message": "can process",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2255/commits/b33f385c544c1b7482b60ef8e69b5eae04c504cd"
        },
        "date": 1734536594009,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6798,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1058,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28919,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24142,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24102,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17092,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790181,
            "range": "± 917",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2720308,
            "range": "± 3075",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9560911,
            "range": "± 29080",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34966279,
            "range": "± 41789",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139460694,
            "range": "± 173129",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3127,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2455,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2448,
            "range": "± 3",
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
            "value": 65391,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252122,
            "range": "± 229",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028606,
            "range": "± 1344",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4865232,
            "range": "± 3291",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27609021,
            "range": "± 24705",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8323456940,
            "range": "± 41189029",
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
          "id": "d808875338702d8562e0aa45d314ddebf38f9e6f",
          "message": "can process",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2255/commits/d808875338702d8562e0aa45d314ddebf38f9e6f"
        },
        "date": 1734538243592,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6888,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 623,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1064,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29554,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24431,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24383,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17322,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794552,
            "range": "± 1523",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2739448,
            "range": "± 5403",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9680156,
            "range": "± 33161",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35294776,
            "range": "± 148813",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139996567,
            "range": "± 466232",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3189,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2468,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2455,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1358,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65727,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 250651,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1020634,
            "range": "± 1018",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4858392,
            "range": "± 7002",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27917301,
            "range": "± 146561",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8704595226,
            "range": "± 59380843",
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
          "id": "2a5d64c761f25b3bd22040c7caeed1f1f7131d3d",
          "message": "Introduce concept of assignment (2)",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2257/commits/2a5d64c761f25b3bd22040c7caeed1f1f7131d3d"
        },
        "date": 1734538345614,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6869,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 575,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1068,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28929,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24174,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24139,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17068,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 802645,
            "range": "± 862",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2770416,
            "range": "± 3653",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9715440,
            "range": "± 12290",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35691716,
            "range": "± 53811",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140870138,
            "range": "± 246293",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3131,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2452,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2449,
            "range": "± 3",
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
            "value": 65818,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251898,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029801,
            "range": "± 949",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4878339,
            "range": "± 5110",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27657253,
            "range": "± 64896",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8238825384,
            "range": "± 32893859",
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
          "id": "70e59d95486951d8546eb0161c47dbbdd07b81ab",
          "message": "[WIP] Call JIT",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2242/commits/70e59d95486951d8546eb0161c47dbbdd07b81ab"
        },
        "date": 1734540252060,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6834,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 582,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1072,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29033,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24187,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24090,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17015,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 788702,
            "range": "± 2151",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2705105,
            "range": "± 6405",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9463047,
            "range": "± 43273",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35151226,
            "range": "± 399426",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138658337,
            "range": "± 1347195",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3121,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2529,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2642,
            "range": "± 5",
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
            "value": 65525,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251450,
            "range": "± 208",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1027808,
            "range": "± 1757",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4924327,
            "range": "± 60563",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28025052,
            "range": "± 346031",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8872585640,
            "range": "± 177109544",
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
          "id": "29da03f45078d509cf08a8b1897cfe4c08afa24f",
          "message": "Add latch to phantom bus interaction",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2258/commits/29da03f45078d509cf08a8b1897cfe4c08afa24f"
        },
        "date": 1734541178993,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6808,
            "range": "± 6",
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
            "value": 1080,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28809,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24010,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24017,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17164,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792901,
            "range": "± 1229",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2728719,
            "range": "± 5217",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9581334,
            "range": "± 23012",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35014773,
            "range": "± 38071",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138974231,
            "range": "± 205084",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3124,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2453,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2494,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1364,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65423,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252167,
            "range": "± 395",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1028030,
            "range": "± 936",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4885767,
            "range": "± 3578",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27685649,
            "range": "± 123989",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9020587101,
            "range": "± 206448739",
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
          "id": "8dd0a00c1fd15c5748a759ed08385f9557feff02",
          "message": "[WIP] Call JIT",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2242/commits/8dd0a00c1fd15c5748a759ed08385f9557feff02"
        },
        "date": 1734543909028,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6942,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 580,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1082,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29611,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24573,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24541,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17415,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794976,
            "range": "± 1518",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2757076,
            "range": "± 6412",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9606470,
            "range": "± 16600",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35093514,
            "range": "± 35170",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140000480,
            "range": "± 534714",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3598,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2838,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2838,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1677,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100370,
            "range": "± 166",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 374711,
            "range": "± 329",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1444088,
            "range": "± 2030",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6152384,
            "range": "± 10497",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30581747,
            "range": "± 95181",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8473185537,
            "range": "± 44423377",
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
          "id": "918e075e356a769610e6e88549f508bba46bc1a8",
          "message": "Introduce concept of assignment.",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2244/commits/918e075e356a769610e6e88549f508bba46bc1a8"
        },
        "date": 1734548893515,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6837,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 579,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1068,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29564,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24410,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24408,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17233,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 804369,
            "range": "± 2405",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2763620,
            "range": "± 6846",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9695919,
            "range": "± 45096",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35434103,
            "range": "± 149277",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141307571,
            "range": "± 902967",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3121,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2455,
            "range": "± 7",
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
            "value": 1360,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65471,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252375,
            "range": "± 289",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1022899,
            "range": "± 1354",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4872300,
            "range": "± 24402",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27622526,
            "range": "± 91866",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8376865382,
            "range": "± 150319188",
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
          "id": "07015859c1af7853443220d57c5bfd2edc361662",
          "message": "\"can process fully\"",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2255/commits/07015859c1af7853443220d57c5bfd2edc361662"
        },
        "date": 1734549562845,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6846,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 580,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1054,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28859,
            "range": "± 232",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24144,
            "range": "± 277",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24058,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17091,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 786421,
            "range": "± 1755",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2719057,
            "range": "± 6263",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9573284,
            "range": "± 114031",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35372404,
            "range": "± 273648",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139898448,
            "range": "± 421652",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3132,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2504,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2451,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1361,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 67104,
            "range": "± 409",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 257561,
            "range": "± 1636",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1039516,
            "range": "± 2658",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4976461,
            "range": "± 45055",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28488352,
            "range": "± 168763",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8838233312,
            "range": "± 71948870",
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
          "id": "68eae4bad2d4c55e76f0d60645398184a95e0f34",
          "message": "Reserved space for prover data in memory layout",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2251/commits/68eae4bad2d4c55e76f0d60645398184a95e0f34"
        },
        "date": 1734549695029,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7022,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1054,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29319,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24809,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24515,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17458,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800054,
            "range": "± 1256",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2756648,
            "range": "± 4006",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9664354,
            "range": "± 28329",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35566932,
            "range": "± 199144",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141696521,
            "range": "± 689822",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3140,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2466,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2453,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1371,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65554,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254483,
            "range": "± 163",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035413,
            "range": "± 1677",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4944208,
            "range": "± 36579",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28433980,
            "range": "± 432642",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8838035865,
            "range": "± 53960551",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "18f6da0c476cf27614679d5b6889d36fe79d2699",
          "message": "Specialized code for goldilocks. (#2253)\n\nThis is mostly a reduced copy of the goldilocks implementation we\nalready have with the main difference that the division tries to perform\ninteger division first if it can be done without remainder.",
          "timestamp": "2024-12-18T19:09:51Z",
          "url": "https://github.com/powdr-labs/powdr/commit/18f6da0c476cf27614679d5b6889d36fe79d2699"
        },
        "date": 1734549964230,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6810,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 595,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1065,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28805,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24117,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24102,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17040,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 797027,
            "range": "± 1243",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2749752,
            "range": "± 5463",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9590934,
            "range": "± 26350",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35052167,
            "range": "± 144526",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140204498,
            "range": "± 904362",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3148,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2461,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2457,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1380,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65522,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251631,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1029739,
            "range": "± 892",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4871711,
            "range": "± 13271",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27895099,
            "range": "± 153376",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8503204111,
            "range": "± 134671824",
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
          "id": "47632c3b0c7e6d6736e7afe7fc686dbd3900b451",
          "message": "Support machine calls.",
          "timestamp": "2024-12-18T14:51:47Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2241/commits/47632c3b0c7e6d6736e7afe7fc686dbd3900b451"
        },
        "date": 1734550191330,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7084,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 599,
            "range": "± 2",
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
            "value": 29499,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24656,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24561,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17360,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791667,
            "range": "± 978",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2732084,
            "range": "± 2246",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9526232,
            "range": "± 16465",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34830839,
            "range": "± 67385",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138800032,
            "range": "± 228613",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3126,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2450,
            "range": "± 3",
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
            "value": 1363,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65470,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 252763,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1022903,
            "range": "± 720",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4852724,
            "range": "± 2199",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27572973,
            "range": "± 19187",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8121994354,
            "range": "± 30857182",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "1b58d1e18a1eaeb63e6db29494cda9e908703234",
          "message": "Introduce concept of assignment. (#2244)",
          "timestamp": "2024-12-18T19:21:48Z",
          "url": "https://github.com/powdr-labs/powdr/commit/1b58d1e18a1eaeb63e6db29494cda9e908703234"
        },
        "date": 1734550648367,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6880,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 585,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1090,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29277,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24547,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24549,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17363,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792549,
            "range": "± 1215",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2723350,
            "range": "± 4767",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9580767,
            "range": "± 16144",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34954802,
            "range": "± 118949",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138911182,
            "range": "± 177029",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3208,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2531,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2489,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1362,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65213,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251300,
            "range": "± 242",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1023357,
            "range": "± 1270",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4863027,
            "range": "± 28304",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27511439,
            "range": "± 58673",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8216514352,
            "range": "± 66537099",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "89a01a4e8b1c26864a9e571217eabdede5bafe5d",
          "message": "\"can process fully\" (#2255)\n\nAdd a function to the Machine trait that can be used to determine if a\nsubmachine call with certain known inputs and certain range constraints\ncan be fully processed by the called machine.\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-18T19:33:37Z",
          "url": "https://github.com/powdr-labs/powdr/commit/89a01a4e8b1c26864a9e571217eabdede5bafe5d"
        },
        "date": 1734551433656,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6871,
            "range": "± 26",
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
            "value": 1197,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28892,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24239,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24169,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17070,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800831,
            "range": "± 4561",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2750205,
            "range": "± 23177",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9713356,
            "range": "± 95789",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35707263,
            "range": "± 388189",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142327294,
            "range": "± 1288030",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3135,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2454,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2463,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1380,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 65483,
            "range": "± 577",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 251855,
            "range": "± 623",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1024653,
            "range": "± 11604",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4870911,
            "range": "± 34833",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28288761,
            "range": "± 282936",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9008191836,
            "range": "± 48172709",
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
          "id": "2e3a64c30b3334b5b6bc2046bfcdb1041f1be2ba",
          "message": "Call JIT",
          "timestamp": "2024-12-18T19:51:19Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2242/commits/2e3a64c30b3334b5b6bc2046bfcdb1041f1be2ba"
        },
        "date": 1734552678493,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7098,
            "range": "± 191",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 591,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1104,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30279,
            "range": "± 1070",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25230,
            "range": "± 495",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25053,
            "range": "± 450",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17618,
            "range": "± 290",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 817441,
            "range": "± 9078",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2794316,
            "range": "± 30517",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9821240,
            "range": "± 83083",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35687038,
            "range": "± 379271",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141416078,
            "range": "± 967672",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3622,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2877,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2866,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1682,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100402,
            "range": "± 631",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 371795,
            "range": "± 2039",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1429050,
            "range": "± 5833",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6133656,
            "range": "± 34101",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30706083,
            "range": "± 277074",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8322848144,
            "range": "± 49159314",
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
          "id": "46ef4d7abf009f7c3eaa578a87cb15516f106f1a",
          "message": "Support machine calls.",
          "timestamp": "2024-12-18T19:51:19Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2241/commits/46ef4d7abf009f7c3eaa578a87cb15516f106f1a"
        },
        "date": 1734552707254,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6792,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 583,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1074,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29125,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24236,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24238,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17187,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 789670,
            "range": "± 2258",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2730292,
            "range": "± 5674",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9521361,
            "range": "± 32241",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34940214,
            "range": "± 137902",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138482895,
            "range": "± 669591",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3137,
            "range": "± 4",
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
            "value": 2457,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1373,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66043,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254580,
            "range": "± 200",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1035414,
            "range": "± 872",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4872363,
            "range": "± 10206",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27708444,
            "range": "± 21982",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8097717750,
            "range": "± 36551644",
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
          "id": "aebb997585d059459e6f696465f236027c298836",
          "message": "Add benchmark for JITgen",
          "timestamp": "2024-12-18T19:51:19Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2260/commits/aebb997585d059459e6f696465f236027c298836"
        },
        "date": 1734553117057,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6920,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1093,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29282,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24433,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24484,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17653,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790660,
            "range": "± 746",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2725239,
            "range": "± 2888",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9529718,
            "range": "± 15306",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35024462,
            "range": "± 80536",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138373559,
            "range": "± 165155",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3142,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2459,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2485,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1372,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 66807,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 257113,
            "range": "± 146",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1057840,
            "range": "± 1138",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 5019144,
            "range": "± 3489",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 27882794,
            "range": "± 135552",
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
          "id": "886cb818ab5de6a3d9d0a01490ef14e8376e5c15",
          "message": "Not-concrete-is-unknown eval",
          "timestamp": "2024-12-18T20:05:18Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2261/commits/886cb818ab5de6a3d9d0a01490ef14e8376e5c15"
        },
        "date": 1734553510947,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6863,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1062,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29408,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24568,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24532,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17209,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790969,
            "range": "± 1148",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2716792,
            "range": "± 3783",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9561215,
            "range": "± 20560",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35197185,
            "range": "± 84733",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139442025,
            "range": "± 358549",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3666,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2866,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2861,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1675,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100649,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 370985,
            "range": "± 220",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1424297,
            "range": "± 1388",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6084671,
            "range": "± 7878",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30487225,
            "range": "± 108464",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8660030950,
            "range": "± 37204487",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "89a01a4e8b1c26864a9e571217eabdede5bafe5d",
          "message": "\"can process fully\" (#2255)\n\nAdd a function to the Machine trait that can be used to determine if a\nsubmachine call with certain known inputs and certain range constraints\ncan be fully processed by the called machine.\n\n---------\n\nCo-authored-by: Georg Wiese <georgwiese@gmail.com>",
          "timestamp": "2024-12-18T19:33:37Z",
          "tree_id": "69e8696eb6be9129adf55b36e14f54ad2b1edcc7",
          "url": "https://github.com/powdr-labs/powdr/commit/89a01a4e8b1c26864a9e571217eabdede5bafe5d"
        },
        "date": 1734553632923,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6895,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 576,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1061,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29038,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24374,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24282,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17250,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791949,
            "range": "± 1003",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2735063,
            "range": "± 6806",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9549451,
            "range": "± 25160",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35110476,
            "range": "± 208279",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139211533,
            "range": "± 461145",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3133,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2447,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2449,
            "range": "± 7",
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
            "value": 66128,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 254839,
            "range": "± 1103",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1032638,
            "range": "± 1087",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 4873224,
            "range": "± 4323",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 28077808,
            "range": "± 230983",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8567786964,
            "range": "± 80240524",
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
          "id": "a737ed851f42fe75bdd88b386869d41b2afd5d26",
          "message": "Call JIT (#2242)\n\nThis PR puts together the pieces to run compile-time witgen for block\nmachines. There are still many cases where it doesn't work yet, in which\ncase it falls back to run-time solving. These cases should be fixed in\nfuture PRs.\n\nIt also fixes two bugs:\n- When multiplying two affine expression, the case where one of them is\nzero is now handled properly.\n- `WitgenInference` now handles intermediate columns.\n\nNote that this PR could slow down witgen by attempting to compile code\nonce per incoming connection and input / output combination, in block\nmachines. I think this should be negligible though and it gives us that\nmuch of the new pipeline is already running in the tests and elsewhere.\n\n# Benchmark results\n\nI tested the code with different opt levels on a benchmark that computes\nca. $2^{16}$ Poseidon hashes.\n\n## Baseline\n\n```\n == Witgen profile (393220 events)\n   93.0% (   30.8s): Secondary machine 0: main_poseidon (BlockMachine)\n    4.1% (    1.4s): witgen (outer code)\n    2.3% ( 750.8ms): Main machine (Dynamic)\n    0.6% ( 204.4ms): FixedLookup\n    0.0% (   3.2µs): range constraint multiplicity witgen\n  ---------------------------\n    ==> Total: 33.109672458s\n```\n\n## JIT (opt level 1)\n\n```\n == Witgen profile (393222 events)\n   52.3% (    7.7s): JIT-compilation\n   32.0% (    4.7s): Secondary machine 0: main_poseidon (BlockMachine)\n    9.2% (    1.3s): witgen (outer code)\n    5.1% ( 748.3ms): Main machine (Dynamic)\n    1.4% ( 213.5ms): FixedLookup\n    0.0% ( 417.0ns): range constraint multiplicity witgen\n  ---------------------------\n    ==> Total: 14.729149333s\n```\n\n## JIT (opt level 3)\n\n```\n== Witgen profile (393222 events)\n   94.6% (  107.9s): JIT-compilation\n    3.4% (    3.9s): Secondary machine 0: main_poseidon (BlockMachine)\n    1.1% (    1.3s): witgen (outer code)\n    0.7% ( 746.5ms): Main machine (Dynamic)\n    0.2% ( 204.1ms): FixedLookup\n    0.0% ( 542.0ns): range constraint multiplicity witgen\n  ---------------------------\n    ==> Total: 114.036571291s\n```",
          "timestamp": "2024-12-18T20:27:49Z",
          "url": "https://github.com/powdr-labs/powdr/commit/a737ed851f42fe75bdd88b386869d41b2afd5d26"
        },
        "date": 1734554688837,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6971,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 576,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1091,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29066,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24251,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24171,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17071,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 811809,
            "range": "± 2080",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2790714,
            "range": "± 8755",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9835578,
            "range": "± 44070",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36117357,
            "range": "± 233182",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 144627593,
            "range": "± 1026509",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3705,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2871,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2867,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1670,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99770,
            "range": "± 250",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 374743,
            "range": "± 1505",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1443770,
            "range": "± 5126",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6199065,
            "range": "± 46483",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31251191,
            "range": "± 222122",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9189673850,
            "range": "± 48079525",
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
          "id": "a737ed851f42fe75bdd88b386869d41b2afd5d26",
          "message": "Call JIT (#2242)\n\nThis PR puts together the pieces to run compile-time witgen for block\nmachines. There are still many cases where it doesn't work yet, in which\ncase it falls back to run-time solving. These cases should be fixed in\nfuture PRs.\n\nIt also fixes two bugs:\n- When multiplying two affine expression, the case where one of them is\nzero is now handled properly.\n- `WitgenInference` now handles intermediate columns.\n\nNote that this PR could slow down witgen by attempting to compile code\nonce per incoming connection and input / output combination, in block\nmachines. I think this should be negligible though and it gives us that\nmuch of the new pipeline is already running in the tests and elsewhere.\n\n# Benchmark results\n\nI tested the code with different opt levels on a benchmark that computes\nca. $2^{16}$ Poseidon hashes.\n\n## Baseline\n\n```\n == Witgen profile (393220 events)\n   93.0% (   30.8s): Secondary machine 0: main_poseidon (BlockMachine)\n    4.1% (    1.4s): witgen (outer code)\n    2.3% ( 750.8ms): Main machine (Dynamic)\n    0.6% ( 204.4ms): FixedLookup\n    0.0% (   3.2µs): range constraint multiplicity witgen\n  ---------------------------\n    ==> Total: 33.109672458s\n```\n\n## JIT (opt level 1)\n\n```\n == Witgen profile (393222 events)\n   52.3% (    7.7s): JIT-compilation\n   32.0% (    4.7s): Secondary machine 0: main_poseidon (BlockMachine)\n    9.2% (    1.3s): witgen (outer code)\n    5.1% ( 748.3ms): Main machine (Dynamic)\n    1.4% ( 213.5ms): FixedLookup\n    0.0% ( 417.0ns): range constraint multiplicity witgen\n  ---------------------------\n    ==> Total: 14.729149333s\n```\n\n## JIT (opt level 3)\n\n```\n== Witgen profile (393222 events)\n   94.6% (  107.9s): JIT-compilation\n    3.4% (    3.9s): Secondary machine 0: main_poseidon (BlockMachine)\n    1.1% (    1.3s): witgen (outer code)\n    0.7% ( 746.5ms): Main machine (Dynamic)\n    0.2% ( 204.1ms): FixedLookup\n    0.0% ( 542.0ns): range constraint multiplicity witgen\n  ---------------------------\n    ==> Total: 114.036571291s\n```",
          "timestamp": "2024-12-18T20:27:49Z",
          "tree_id": "d8aec592d76bd341c83f04dee5f01ba6be0d9cf1",
          "url": "https://github.com/powdr-labs/powdr/commit/a737ed851f42fe75bdd88b386869d41b2afd5d26"
        },
        "date": 1734556530592,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6877,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1099,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29521,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24568,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24488,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17350,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 800376,
            "range": "± 1751",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2769226,
            "range": "± 6267",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9667007,
            "range": "± 41046",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35369118,
            "range": "± 184297",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140388147,
            "range": "± 586433",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3613,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2875,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2876,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1703,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99659,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 371799,
            "range": "± 330",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1429847,
            "range": "± 2103",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6100775,
            "range": "± 21360",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30569526,
            "range": "± 188786",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8449156872,
            "range": "± 66432993",
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
          "id": "2b317c10fd97fea025e4bc290936ec9c399fa3af",
          "message": "Add benchmark for JITgen",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2260/commits/2b317c10fd97fea025e4bc290936ec9c399fa3af"
        },
        "date": 1734557428981,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6881,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 576,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1081,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29196,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24489,
            "range": "± 169",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24383,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17237,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 807846,
            "range": "± 3779",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2778261,
            "range": "± 32788",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9873572,
            "range": "± 52657",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36457975,
            "range": "± 255568",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 145265013,
            "range": "± 1161923",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3599,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2860,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2844,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1682,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100547,
            "range": "± 265",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 370298,
            "range": "± 8824",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1437737,
            "range": "± 2169",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6178552,
            "range": "± 47354",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31355765,
            "range": "± 271299",
            "unit": "ns/iter"
          },
          {
            "name": "jit-witgen-benchmark/jit_witgen_benchmark",
            "value": 55247830787,
            "range": "± 654589745",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9148444221,
            "range": "± 71373075",
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
          "id": "caf1b3892097ab463146321b4aa510a8fa6a9152",
          "message": "[Prototype] Simple Cache for JIT compilation",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2262/commits/caf1b3892097ab463146321b4aa510a8fa6a9152"
        },
        "date": 1734561720294,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6963,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 577,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1072,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29821,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25380,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25056,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17652,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 799279,
            "range": "± 694",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2756437,
            "range": "± 3549",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9639180,
            "range": "± 16690",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35315258,
            "range": "± 41400",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140537119,
            "range": "± 183713",
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
          "id": "07dc059646443b1d45abe10ab0ee6229e64346cb",
          "message": "Sponge construction for keccak syscall",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2263/commits/07dc059646443b1d45abe10ab0ee6229e64346cb"
        },
        "date": 1734568163998,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7136,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 597,
            "range": "± 1",
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
            "value": 29106,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24357,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24344,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17128,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 807951,
            "range": "± 2121",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2800784,
            "range": "± 3708",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9814499,
            "range": "± 53594",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36735884,
            "range": "± 427287",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 144952106,
            "range": "± 673974",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3590,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2848,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2851,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1674,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100140,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 373743,
            "range": "± 308",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1447040,
            "range": "± 3431",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6284501,
            "range": "± 67168",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31134508,
            "range": "± 488272",
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
          "id": "db788a67e8c362fcd8c0674831547d0e9ae1d2f1",
          "message": "use can_process",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2256/commits/db788a67e8c362fcd8c0674831547d0e9ae1d2f1"
        },
        "date": 1734603216974,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7009,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 600,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1113,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30096,
            "range": "± 190",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25218,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25070,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17846,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 792813,
            "range": "± 1383",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2732568,
            "range": "± 6279",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9577645,
            "range": "± 25712",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35076337,
            "range": "± 178702",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138841534,
            "range": "± 257913",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3588,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2833,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2835,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1682,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 101136,
            "range": "± 278",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 378121,
            "range": "± 610",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1461021,
            "range": "± 2808",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6197070,
            "range": "± 20236",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30689855,
            "range": "± 90696",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8909782243,
            "range": "± 65757805",
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
          "id": "64fab4376084c56cf6777a7d4f446c0575a90696",
          "message": "Sponge construction for keccak syscall",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2263/commits/64fab4376084c56cf6777a7d4f446c0575a90696"
        },
        "date": 1734608511992,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6946,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 626,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1066,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29197,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24441,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24388,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17204,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 797636,
            "range": "± 2025",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2753329,
            "range": "± 6169",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9602623,
            "range": "± 40271",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35161301,
            "range": "± 184977",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140004245,
            "range": "± 793110",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3569,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2837,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2829,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1679,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99821,
            "range": "± 288",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 372504,
            "range": "± 359",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1435571,
            "range": "± 2022",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6094441,
            "range": "± 29413",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30651519,
            "range": "± 240536",
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
          "id": "83ce3f3ee622885f8bac1adf0545d1393b1c50f8",
          "message": "(WIP) remove reset instr fix",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2265/commits/83ce3f3ee622885f8bac1adf0545d1393b1c50f8"
        },
        "date": 1734609254485,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6882,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 587,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1140,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28757,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 23928,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23980,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16914,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 778710,
            "range": "± 921",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2694381,
            "range": "± 2029",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9392117,
            "range": "± 13510",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34337243,
            "range": "± 110688",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137294769,
            "range": "± 204984",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3616,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2860,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2857,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1690,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99092,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 371032,
            "range": "± 369",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1432171,
            "range": "± 2383",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6119358,
            "range": "± 10973",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30266097,
            "range": "± 164139",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8355466157,
            "range": "± 60361077",
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
          "id": "0bdbbd2c4ea7274abeb1f07197d8f4bbe2c702dc",
          "message": "Update stwo dependency to the latest version",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2264/commits/0bdbbd2c4ea7274abeb1f07197d8f4bbe2c702dc"
        },
        "date": 1734614411446,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6867,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 594,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1078,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29111,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24372,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24343,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17193,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 771056,
            "range": "± 965",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2666556,
            "range": "± 2827",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9310530,
            "range": "± 14193",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34153751,
            "range": "± 111309",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 136591713,
            "range": "± 638994",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 2735,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2165,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2126,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1261,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 92048,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 344544,
            "range": "± 371",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1375077,
            "range": "± 1426",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6031259,
            "range": "± 20281",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31538334,
            "range": "± 112702",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8629259168,
            "range": "± 84553531",
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
          "id": "fec80a842d06ed143a8ac3146677d7a49be229b8",
          "message": "Remove reset instruction",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/1718/commits/fec80a842d06ed143a8ac3146677d7a49be229b8"
        },
        "date": 1734616116826,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6762,
            "range": "± 4",
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
            "value": 1035,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28452,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 23832,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23767,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16896,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 789267,
            "range": "± 1999",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2712525,
            "range": "± 5820",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9545553,
            "range": "± 41000",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34858521,
            "range": "± 187860",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138957498,
            "range": "± 797639",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3616,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2861,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2857,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1687,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 101608,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 379007,
            "range": "± 530",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1471783,
            "range": "± 2155",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6266018,
            "range": "± 33652",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31101738,
            "range": "± 222366",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8645313056,
            "range": "± 52059619",
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
          "id": "7eeeab2dca35b9005dcbbc3aa3a5b7eda0a8f654",
          "message": "Update stwo dependency to the latest version",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2264/commits/7eeeab2dca35b9005dcbbc3aa3a5b7eda0a8f654"
        },
        "date": 1734616476591,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6860,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 607,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1095,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29312,
            "range": "± 273",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24534,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24417,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17344,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790276,
            "range": "± 2373",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2728158,
            "range": "± 6754",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9538212,
            "range": "± 25458",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34981602,
            "range": "± 114304",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139339715,
            "range": "± 542144",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 2705,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2168,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2148,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1273,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 91817,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 343898,
            "range": "± 291",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1351522,
            "range": "± 3583",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6005667,
            "range": "± 18419",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31780788,
            "range": "± 218566",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8823148608,
            "range": "± 65520536",
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
          "id": "1daf745b6d9edd8635607fb2552a164907248d2b",
          "message": "Not-concrete-is-unknown eval",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2261/commits/1daf745b6d9edd8635607fb2552a164907248d2b"
        },
        "date": 1734617174883,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6945,
            "range": "± 7",
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
            "value": 1107,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28826,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24207,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24172,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17112,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 802201,
            "range": "± 744",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2769766,
            "range": "± 2897",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9698385,
            "range": "± 17893",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35571886,
            "range": "± 60282",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139723836,
            "range": "± 169104",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3619,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2856,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2893,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1685,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99822,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 372331,
            "range": "± 728",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1429714,
            "range": "± 1496",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6096622,
            "range": "± 7961",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30506353,
            "range": "± 72278",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8419662973,
            "range": "± 11342422",
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
          "id": "a74bfe91c1b96223b2661ee8ce7e6e7c4f07c62d",
          "message": "Extract effect and sub-types into its own module.",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2266/commits/a74bfe91c1b96223b2661ee8ce7e6e7c4f07c62d"
        },
        "date": 1734617451176,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6902,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 578,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1090,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29371,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24584,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24480,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17366,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 799247,
            "range": "± 1176",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2762814,
            "range": "± 7295",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9711630,
            "range": "± 16075",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35313284,
            "range": "± 203853",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139982413,
            "range": "± 372737",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3603,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2867,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2851,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1688,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99518,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 369312,
            "range": "± 289",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1432157,
            "range": "± 2540",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6096421,
            "range": "± 8568",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30371304,
            "range": "± 105764",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8403870630,
            "range": "± 116945763",
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
          "id": "5f323c65ab4b715b0ad526c2cebc1d85ff9c0562",
          "message": "Prepare branching",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2259/commits/5f323c65ab4b715b0ad526c2cebc1d85ff9c0562"
        },
        "date": 1734617456384,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6859,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 572,
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
            "value": 30751,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24287,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24197,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17132,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 798761,
            "range": "± 1594",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2761893,
            "range": "± 6968",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9647210,
            "range": "± 33833",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35444382,
            "range": "± 151479",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 140965780,
            "range": "± 770701",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3617,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2887,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2837,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1673,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 98703,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 367427,
            "range": "± 344",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1423615,
            "range": "± 2027",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6145011,
            "range": "± 42681",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31093486,
            "range": "± 224293",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8979079922,
            "range": "± 49990898",
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
          "id": "bee112ec5488a8724cb01bc59f1a92420bfe0644",
          "message": "(ignore) opt 2",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2267/commits/bee112ec5488a8724cb01bc59f1a92420bfe0644"
        },
        "date": 1734617621964,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6948,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 604,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1090,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29477,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24613,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24564,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17332,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 788176,
            "range": "± 1252",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2721699,
            "range": "± 2277",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9498046,
            "range": "± 13402",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34835563,
            "range": "± 99109",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137713246,
            "range": "± 149021",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3593,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2856,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2859,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1684,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100239,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 375036,
            "range": "± 482",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1449102,
            "range": "± 2238",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6205397,
            "range": "± 10565",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30571399,
            "range": "± 104823",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8459279079,
            "range": "± 33159880",
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
          "id": "047cd5c85f9d37e223b75341e31afb059c96fdac",
          "message": "Sponge construction for keccak syscall",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2263/commits/047cd5c85f9d37e223b75341e31afb059c96fdac"
        },
        "date": 1734618164913,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7040,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 585,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1079,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30461,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25433,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25830,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17996,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 785481,
            "range": "± 1082",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2701567,
            "range": "± 2980",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9514851,
            "range": "± 10703",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34794980,
            "range": "± 51961",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 137912639,
            "range": "± 129512",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3663,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2971,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2839,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1671,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99994,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 374776,
            "range": "± 261",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1437299,
            "range": "± 2262",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6129366,
            "range": "± 7733",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30627304,
            "range": "± 74979",
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
          "id": "595abb691febe467a579977137a732bb8480223d",
          "message": "Support machine calls.",
          "timestamp": "2024-12-18T20:59:30Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2241/commits/595abb691febe467a579977137a732bb8480223d"
        },
        "date": 1734618607890,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7064,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 578,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1078,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 30862,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 25646,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 25570,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 18075,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 805525,
            "range": "± 1183",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2777867,
            "range": "± 3606",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9768685,
            "range": "± 36728",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36291716,
            "range": "± 230672",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 143769122,
            "range": "± 927116",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3617,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2899,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2870,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1672,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99579,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 370438,
            "range": "± 494",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1428203,
            "range": "± 1952",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6187575,
            "range": "± 59741",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30760496,
            "range": "± 230590",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8652901781,
            "range": "± 62054565",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "309279ac8a0f100e0b401289641bb009f2664b67",
          "message": "Extract effect and sub-types into its own module. (#2266)",
          "timestamp": "2024-12-19T14:27:25Z",
          "url": "https://github.com/powdr-labs/powdr/commit/309279ac8a0f100e0b401289641bb009f2664b67"
        },
        "date": 1734619509091,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6973,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 608,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1089,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29066,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24367,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24245,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17211,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 790779,
            "range": "± 1353",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2726282,
            "range": "± 6395",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9624249,
            "range": "± 68587",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35655628,
            "range": "± 252281",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 141361036,
            "range": "± 740216",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3616,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2869,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2839,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1677,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99546,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 372898,
            "range": "± 329",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1430436,
            "range": "± 2089",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6182888,
            "range": "± 54711",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31123242,
            "range": "± 243558",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 9018827071,
            "range": "± 48361087",
            "unit": "ns/iter"
          }
        ]
      },
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
          "distinct": false,
          "id": "309279ac8a0f100e0b401289641bb009f2664b67",
          "message": "Extract effect and sub-types into its own module. (#2266)",
          "timestamp": "2024-12-19T14:27:25Z",
          "tree_id": "2f12c4853a5ea5d363bb8d5510d4dae28a5640e0",
          "url": "https://github.com/powdr-labs/powdr/commit/309279ac8a0f100e0b401289641bb009f2664b67"
        },
        "date": 1734621366630,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6794,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 586,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1079,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28737,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24019,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 23979,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 16922,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791696,
            "range": "± 1868",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2721847,
            "range": "± 4339",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9525738,
            "range": "± 40374",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34841704,
            "range": "± 150450",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139426697,
            "range": "± 1166536",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3592,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2856,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2865,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1689,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100726,
            "range": "± 171",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 373126,
            "range": "± 268",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1437414,
            "range": "± 3385",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6155487,
            "range": "± 17820",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31362601,
            "range": "± 230611",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8505983672,
            "range": "± 66638843",
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
          "id": "71beadee67989a87da1e9eaf327c8af726a83f0f",
          "message": "use can_process",
          "timestamp": "2024-12-19T14:59:19Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2256/commits/71beadee67989a87da1e9eaf327c8af726a83f0f"
        },
        "date": 1734624987706,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6985,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 581,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1081,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29182,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24502,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24333,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17225,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 795163,
            "range": "± 898",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2736424,
            "range": "± 3868",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9551010,
            "range": "± 12450",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35040940,
            "range": "± 165303",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138761474,
            "range": "± 286495",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3576,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2841,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2838,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1675,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99878,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 370334,
            "range": "± 224",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1428462,
            "range": "± 1798",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6093843,
            "range": "± 11322",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30650611,
            "range": "± 189222",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8763858339,
            "range": "± 119454684",
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
          "id": "92ffac3883ca2dc91e0cf2b498cb86725104f102",
          "message": "Support machine calls.",
          "timestamp": "2024-12-19T14:59:19Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2241/commits/92ffac3883ca2dc91e0cf2b498cb86725104f102"
        },
        "date": 1734625156008,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6875,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 603,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1074,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 28932,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24460,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24210,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17121,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 784865,
            "range": "± 756",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2709497,
            "range": "± 3150",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9476417,
            "range": "± 11018",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34831109,
            "range": "± 30799",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138299180,
            "range": "± 522318",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3591,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2841,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2845,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1672,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100396,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 375313,
            "range": "± 392",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1460401,
            "range": "± 2233",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6174353,
            "range": "± 9011",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30606498,
            "range": "± 83039",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8263449716,
            "range": "± 160575149",
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
          "id": "dd1f13a80c9f6372a0c74ab31dab3ec214fc4366",
          "message": "Not-concrete-is-unknown eval",
          "timestamp": "2024-12-19T14:59:19Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2261/commits/dd1f13a80c9f6372a0c74ab31dab3ec214fc4366"
        },
        "date": 1734625403914,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6865,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 572,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1054,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29099,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24260,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24209,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17314,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791434,
            "range": "± 1584",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2733178,
            "range": "± 5089",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9576815,
            "range": "± 34700",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34952492,
            "range": "± 108591",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139662676,
            "range": "± 382076",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3585,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2843,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2839,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1671,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99354,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 369457,
            "range": "± 530",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1427788,
            "range": "± 1786",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6097693,
            "range": "± 28857",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30823099,
            "range": "± 200649",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8562467697,
            "range": "± 114645131",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "a2a67a6264b631645bc8b0e8c90d655b3a69864f",
          "message": "Support machine calls. (#2241)\n\nDepends on #2244",
          "timestamp": "2024-12-19T16:35:19Z",
          "url": "https://github.com/powdr-labs/powdr/commit/a2a67a6264b631645bc8b0e8c90d655b3a69864f"
        },
        "date": 1734627072678,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6897,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 607,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1158,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29287,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24405,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24275,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17227,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 791029,
            "range": "± 689",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2729796,
            "range": "± 2841",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9554663,
            "range": "± 7865",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 34972444,
            "range": "± 31488",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139614021,
            "range": "± 96353",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3600,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2851,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2855,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1689,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99938,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 369453,
            "range": "± 343",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1428515,
            "range": "± 1681",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6087757,
            "range": "± 6154",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30324679,
            "range": "± 65541",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8047355219,
            "range": "± 37288372",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "name": "chriseth",
            "username": "chriseth",
            "email": "chris@ethereum.org"
          },
          "committer": {
            "name": "GitHub",
            "username": "web-flow",
            "email": "noreply@github.com"
          },
          "id": "d5c8a1ef876b539c3b23d117408e098a1cc4fd1c",
          "message": "Not-concrete-is-unknown eval (#2261)",
          "timestamp": "2024-12-19T16:38:20Z",
          "url": "https://github.com/powdr-labs/powdr/commit/d5c8a1ef876b539c3b23d117408e098a1cc4fd1c"
        },
        "date": 1734627317034,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6896,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 579,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1082,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29131,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24334,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24304,
            "range": "± 91",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17275,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 804403,
            "range": "± 1752",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2765693,
            "range": "± 6850",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9738451,
            "range": "± 44882",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35911052,
            "range": "± 260990",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 142295037,
            "range": "± 828479",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3613,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2877,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2891,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1702,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99747,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 373113,
            "range": "± 467",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1427105,
            "range": "± 2352",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6202769,
            "range": "± 78980",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 31112514,
            "range": "± 330741",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8997158132,
            "range": "± 41020700",
            "unit": "ns/iter"
          }
        ]
      },
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
          "distinct": false,
          "id": "a2a67a6264b631645bc8b0e8c90d655b3a69864f",
          "message": "Support machine calls. (#2241)\n\nDepends on #2244",
          "timestamp": "2024-12-19T16:35:19Z",
          "tree_id": "ad8967d28ce34cbd72a187a56460efadf735938e",
          "url": "https://github.com/powdr-labs/powdr/commit/a2a67a6264b631645bc8b0e8c90d655b3a69864f"
        },
        "date": 1734629046566,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6905,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 581,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1067,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29413,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24581,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24535,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17660,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 793870,
            "range": "± 1508",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2743078,
            "range": "± 9345",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9585050,
            "range": "± 30434",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35111054,
            "range": "± 105184",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 139413473,
            "range": "± 297245",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3640,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2874,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2856,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1682,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99343,
            "range": "± 179",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 370731,
            "range": "± 1239",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1431920,
            "range": "± 7475",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6132567,
            "range": "± 30589",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30556983,
            "range": "± 139410",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8421222939,
            "range": "± 32321010",
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
          "id": "950f990024310b0c9a763d904cb3d590c9b71fdd",
          "message": "improve executor handwritten witgen performance",
          "timestamp": "2024-12-19T17:12:00Z",
          "url": "https://github.com/powdr-labs/powdr/pull/2269/commits/950f990024310b0c9a763d904cb3d590c9b71fdd"
        },
        "date": 1734629947505,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 6832,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::reduce",
            "value": 605,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/std::math::ff::mul",
            "value": 1070,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 29174,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 24425,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 24380,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 17294,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 794961,
            "range": "± 1196",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2733700,
            "range": "± 4002",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 9541659,
            "range": "± 10321",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 35011646,
            "range": "± 108081",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 138928346,
            "range": "± 224287",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3757,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2913,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2868,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1674,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 100094,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 371711,
            "range": "± 466",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1435314,
            "range": "± 2477",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6117434,
            "range": "± 8187",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30622308,
            "range": "± 72896",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8165587016,
            "range": "± 29999428",
            "unit": "ns/iter"
          }
        ]
      },
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
          "id": "d5c8a1ef876b539c3b23d117408e098a1cc4fd1c",
          "message": "Not-concrete-is-unknown eval (#2261)",
          "timestamp": "2024-12-19T16:38:20Z",
          "tree_id": "f07cb4f01ad9d915b6df027bf140c3a6420bfea0",
          "url": "https://github.com/powdr-labs/powdr/commit/d5c8a1ef876b539c3b23d117408e098a1cc4fd1c"
        },
        "date": 1734630925634,
        "tool": "cargo",
        "benches": [
          {
            "name": "evaluator-benchmark/std::math::ff::inverse",
            "value": 7055,
            "range": "± 10",
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
            "value": 1097,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_879882356",
            "value": 34677,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1882356",
            "value": 29024,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_1187956",
            "value": 29094,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sqrt_56",
            "value": 20504,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_33",
            "value": 828151,
            "range": "± 1054",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_100",
            "value": 2873870,
            "range": "± 2888",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_300",
            "value": 10010341,
            "range": "± 14415",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_900",
            "value": 36493304,
            "range": "± 77837",
            "unit": "ns/iter"
          },
          {
            "name": "evaluator-benchmark/sort_2700",
            "value": 143895832,
            "range": "± 122572",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_879882356",
            "value": 3599,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1882356",
            "value": 2906,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_1187956",
            "value": 2849,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sqrt_56",
            "value": 1674,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_33",
            "value": 99957,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_100",
            "value": 369485,
            "range": "± 255",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_300",
            "value": 1428945,
            "range": "± 1671",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_900",
            "value": 6118121,
            "range": "± 9401",
            "unit": "ns/iter"
          },
          {
            "name": "jit-benchmark/sort_2700",
            "value": 30347365,
            "range": "± 72848",
            "unit": "ns/iter"
          },
          {
            "name": "executor-benchmark/keccak",
            "value": 8294008248,
            "range": "± 30698431",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}