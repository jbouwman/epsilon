# HTTP Module Benchmark Baselines

Baseline measurements established on 2026-01-29.

## Individual Benchmarks (Critical Path)

| Benchmark | Mean | Median | 95% CI | Ops/sec | Budget | Status |
|-----------|------|--------|--------|---------|--------|--------|
| HTTP/REQUEST/CREATE | 75ns | 0ps | [58ns, 93ns] | 13.33M | 5us | PASS |
| HTTP/RESPONSE/CREATE | 75ns | 0ps | [59ns, 92ns] | 13.33M | 5us | PASS |
| HTTP/URL/ENCODE | 2.61us | 3us | [2.55us, 2.66us] | 383.62K | 10us | PASS |
| HTTP/POOL/KEY | 197ns | 0ps | [174ns, 222ns] | 5.08M | 2us | PASS |
| HTTP/CIRCUIT/CHECK-CLOSED | 97ns | 0ps | [78ns, 116ns] | 10.31M | 2us | PASS |
| HTTP/REQUEST/PARSE-SIMPLE | 1.58us | 2us | [1.54us, 1.62us] | 633.63K | 20us | PASS |

## Parameterized Benchmarks

### Request Parsing by Body Size

| Body Size | Mean | 95% CI | Throughput |
|-----------|------|--------|------------|
| 0 bytes | 3.20us | [3.13us, 3.27us] | 59.6 MiB/s |
| 256 bytes | 3.45us | [3.38us, 3.53us] | 126.1 MiB/s |
| 1024 bytes | 4.41us | [4.29us, 4.53us] | 265.0 MiB/s |
| 4096 bytes | 9.17us | [8.76us, 9.88us] | 446.6 MiB/s |

Observations:
- Parsing time scales sub-linearly with body size
- Throughput improves with larger bodies (amortized header parsing)
- 4KB body is only 2.87x slower than empty body

### Response Serialization by Header Count

| Header Count | Mean | 95% CI | Throughput |
|--------------|------|--------|------------|
| 5 headers | 1.54us | [1.49us, 1.60us] | 3.25M elem/s |
| 10 headers | 2.50us | [2.44us, 2.56us] | 4.01M elem/s |
| 25 headers | 5.48us | [5.38us, 5.60us] | 4.56M elem/s |
| 50 headers | 10.44us | [10.27us, 10.61us] | 4.79M elem/s |

Observations:
- Header serialization scales linearly with count
- Per-header overhead is ~200ns
- Throughput increases slightly with more headers (amortization)

### URL Encoding by String Length

| Length | Mean | 95% CI | Throughput |
|--------|------|--------|------------|
| 10 chars | 170ns | [148ns, 194ns] | 56.1 MiB/s |
| 50 chars | 710ns | [670ns, 754ns] | 67.2 MiB/s |
| 100 chars | 1.28us | [1.25us, 1.32us] | 74.2 MiB/s |
| 500 chars | 6.35us | [6.17us, 6.61us] | 75.0 MiB/s |

Observations:
- Encoding scales linearly with string length
- Per-character overhead is ~12ns
- Throughput plateaus around 75 MiB/s

## Performance Budgets

All budgets passed. Current budgets are set with margin:

| Operation | Budget | Measured | Headroom |
|-----------|--------|----------|----------|
| Request create | 5us | 75ns | 66x |
| Response create | 5us | 75ns | 66x |
| URL encode | 10us | 2.61us | 3.8x |
| Pool key | 2us | 197ns | 10x |
| Circuit check | 2us | 97ns | 20x |
| Request parse | 20us | 1.58us | 12x |

## Notes

- Measurements taken on Linux x86_64
- SBCL Lisp runtime
- Single-threaded execution
- Results may vary with system load

## Running Benchmarks

```bash
# Full benchmark suite
./epsilon/epsilon benchmark epsilon.http

# Quick CI performance test with budget checking
./epsilon/epsilon benchmark --ci epsilon.http

# Or directly:
./epsilon/epsilon load --module epsilon.http --module epsilon.benchmark \
  epsilon/modules/http/benchmarks/http-benchmarks.lisp \
  --eval "(epsilon.http.benchmarks:run-all-benchmarks)"
```
