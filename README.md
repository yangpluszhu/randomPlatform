# randomPlatform <a href="https://github.com/yangpluszhu/randomPlatform"><img src="inst/app/www/logo.png" align="right" height="120" alt="randomPlatform logo" /></a>

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R >= 4.1.0](https://img.shields.io/badge/R-%3E%3D%204.1.0-blue.svg)](https://www.r-project.org/)
[![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)](https://github.com/yangpluszhu/randomPlatform)
<!-- badges: end -->

**Clinical Trial Randomization Platform** — Generate reproducible randomization schedules, intervention codes, allocation concealment envelopes, emergency unblinding envelopes, audit artifacts, and a local Shiny web interface for clinical trials.

## Features

- **4 randomization methods**: simple, block (permuted block with variable sizes), stratified, and stratified block
- **Dynamic minimization**: Pocock & Simon-style minimization with weighted covariate factors and configurable probability bias (`prob_best`)
- **Intervention coding**: automatic generation of blinded drug codes and random numbers with configurable prefixes, widths, and formatting
- **Allocation concealment envelopes**: B6-size PDF envelopes (cover page + insert page) per stratum, ready for printing and physical concealment
- **Emergency unblinding envelopes**: separate sealed PDF envelopes for SAE/emergency unblinding scenarios
- **SHA-256 integrity**: every generated artifact is hashed; the allocation table hash is embedded into the reproducibility bundle for tamper detection
- **Full reproducibility bundle**: `parameters.json` + `rng_state.rds` + `reproduce_randomization.R` + `session_info.txt`, enabling exact reproduction of any randomization schedule
- **Audit-ready Excel report**: project info, unblinded master, blinded site table, balance summary, and reproducibility metadata in a single `.xlsx` workbook
- **JSONL audit log**: append-only log capturing every operation with timestamps for regulatory traceability
- **UTF-8 BOM CSV**: Windows Excel–compatible CSV output for allocation tables
- **Shiny web interface**: a full-featured local GUI for point-and-click randomization, no R programming required

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("yangpluszhu/randomPlatform")
```

Or install from source after cloning:

```bash
git clone https://github.com/yangpluszhu/randomPlatform.git
cd randomPlatform
R CMD INSTALL .
```

## Quick Start

### Programmatic API

```r
library(randomPlatform)

# Basic block randomization for a two-arm trial
result <- rp_randomize(
  project_name   = "My Trial",
  protocol_no    = "P-2026-001",
  sponsor_name   = "PharmaCorp",
  interventions  = c(T = "Treatment", P = "Placebo"),
  method         = "block",
  seed           = 20260529,
  n_total        = 120,
  allocation_ratio = c(T = 2, P = 1),
  block_sizes    = c(6, 9, 12),
  generate_random_envelope   = TRUE,
  generate_emergency_envelope = TRUE,
  generate_report             = TRUE,
  output_dir      = "output"
)

# Inspect the allocation table
head(result$allocation_table)

# Verify reproducibility
rp_verify_reproducibility(
  original_hash    = result$reproducibility$table_hash,
  reproduced_table = result$allocation_table
)
```

### Stratified Block Randomization

```r
result <- rp_randomize(
  project_name  = "Stratified Trial",
  protocol_no   = "P-2026-002",
  sponsor_name  = "Sponsor",
  interventions = c(A = "Drug A", B = "Drug B"),
  method        = "stratified_block",
  seed          = 42,
  n_total       = 200,
  allocation_ratio = c(A = 1, B = 1),
  strata = list(
    center  = c("BJ01", "SH02", "GZ03"),
    stage   = c("II", "III")
  ),
  block_sizes    = c(4, 6, 8),
  generate_report = TRUE,
  output_dir     = "output_stratified"
)
```

### Dynamic Minimization

```r
# Initialize a minimization session
session <- rp_minimization_session(
  project_name  = "Minimization Trial",
  protocol_no   = "P-2026-003",
  sponsor_name  = "Sponsor",
  interventions = c(A = "Drug A", B = "Drug B"),
  allocation_ratio = c(A = 1, B = 1),
  factors       = c("center", "stage", "age_group"),
  weights       = c(center = 2, stage = 1, age_group = 1),
  prob_best     = 0.8,
  seed          = 999,
  output_dir    = "output_minimization"
)

# Assign participants one at a time
assignment <- rp_assign_next(
  session,
  subject_id = "SUBJ-001",
  covariates = c(center = "BJ01", stage = "III", age_group = ">=60")
)
```

### Shiny Web Interface

Launch the interactive GUI for point-and-click randomization:

```r
randomPlatform::rp_launch_app()
```

The app starts at `http://127.0.0.1:3838` and provides 8 panels:

| Panel | Function |
|-------|----------|
| Project | Project metadata (name, protocol, sponsor) |
| Interventions | Treatment group configuration |
| Randomization | Method selection, sample size, block sizes, stratification |
| Outputs | File generation options (envelopes, reports, reproducibility) |
| Results | Allocation table viewer, balance summary, file downloads |
| Minimization | Dynamic randomization session setup and assignment |
| Verification | Reproducibility verification |
| Settings | Language, font, encryption options |

## API Reference

| Function | Description |
|----------|-------------|
| `rp_randomize()` | Generate a complete randomization package (allocation table, envelopes, report, reproducibility bundle) |
| `rp_minimization_session()` | Initialize a Pocock–Simon minimization session |
| `rp_assign_next()` | Assign the next participant in a minimization session |
| `rp_verify_reproducibility()` | Verify a reproduced table matches the original via SHA-256 hash |
| `rp_launch_app()` | Launch the local Shiny web interface |

## Output Artifacts

`rp_randomize()` generates the following files in `output_dir`:

| File | Description |
|------|-------------|
| `randomization_report.xlsx` | Multi-sheet Excel workbook (project info, unblinded master, blinded table, balance, reproducibility) |
| `random_envelopes.pdf` | Allocation concealment envelopes (B6, one per stratum) |
| `emergency_unblinding_envelopes.pdf` | Emergency unblinding sealed envelopes |
| `allocation_table.csv` | UTF-8 BOM CSV for blinded site use |
| `parameters.json` | Full parameter snapshot for reproducibility |
| `rng_state.rds` | R RNG state after allocation |
| `reproduce_randomization.R` | Standalone R script to exactly reproduce the schedule |
| `session_info.txt` | R version, platform, package versions |
| `hashes_sha256.txt` | SHA-256 hashes of all generated files |
| `audit_log.jsonl` | Append-only JSONL audit trail |

## Randomization Methods

| Method | Key | Use Case |
|--------|-----|----------|
| Simple | `"simple"` | Small trials, minimal balance requirements |
| Block | `"block"` | Standard trials needing periodic balance |
| Stratified | `"stratified"` | Multi-center trials without block constraints |
| Stratified Block | `"stratified_block"` | Multi-center trials with balanced enrollment per stratum |
| Minimization | via `rp_minimization_session()` | Large multi-factor trials with many stratification levels |

## Dependencies

**Imports:** bslib, digest, DT, grDevices, jsonlite, openxlsx, shiny, shinyFiles, stats, utils, zip

**Suggests:** testthat (>= 3.0.0)

## System Requirements

- R >= 4.1.0
- For PDF envelope generation: a system with CJK font support (Windows: Microsoft YaHei / SimHei; macOS: PingFang SC; Linux: Noto Sans CJK SC)

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## Contributing

Issues and pull requests are welcome at [https://github.com/yangpluszhu/randomPlatform/issues](https://github.com/yangpluszhu/randomPlatform/issues).
