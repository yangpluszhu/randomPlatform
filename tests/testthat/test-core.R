test_that("default blind labels do not leak group identifiers", {
  interventions <- .rp_normalize_interventions(c(A = "Test", B = "Control"))
  expect_identical(interventions$blind_label, c("Treatment", "Treatment"))
})

test_that("same seed and same parameters reproduce identical tables", {
  dir <- tempfile("rp-core-")
  res1 <- rp_randomize(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    method = "block",
    seed = 123,
    n_total = 20,
    allocation_ratio = c(A = 1, B = 1),
    block_sizes = c(4, 6),
    generate_report = FALSE,
    generate_reproducibility = FALSE,
    output_dir = dir
  )
  res2 <- rp_randomize(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    method = "block",
    seed = 123,
    n_total = 20,
    allocation_ratio = c(A = 1, B = 1),
    block_sizes = c(4, 6),
    generate_report = FALSE,
    generate_reproducibility = FALSE,
    output_dir = dir
  )
  expect_identical(res1$allocation_table, res2$allocation_table)
})

test_that("block randomization errors on incompatible total and block sizes", {
  dir <- tempfile("rp-block-invalid-")

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "block",
      seed = 123,
      n_total = 5,
      allocation_ratio = c(A = 1, B = 1),
      block_sizes = c(4, 6),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "n_total|block"
  )
})

test_that("block randomization fills feasible totals exactly", {
  dir <- tempfile("rp-block-valid-")
  res <- rp_randomize(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    method = "block",
    seed = 321,
    n_total = 12,
    allocation_ratio = c(A = 1, B = 1),
    block_sizes = c(4, 6, 10),
    generate_report = FALSE,
    generate_reproducibility = FALSE,
    output_dir = dir
  )

  expect_equal(nrow(res$allocation_table), 12L)
  expect_false(anyNA(res$allocation_table$group_id))
})

test_that("block randomization errors on duplicate allocation_ratio names", {
  dir <- tempfile("rp-block-dup-ratio-")

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "block",
      seed = 123,
      n_total = 8,
      allocation_ratio = c(A = 1, A = 9, B = 1),
      block_sizes = c(4),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "allocation_ratio.*unique"
  )
})

test_that("stratified block supports center and envelope outputs", {
  dir <- tempfile("rp-env-")
  res <- rp_randomize(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    method = "stratified_block",
    seed = 456,
    n_total = 8,
    allocation_ratio = c(A = 1, B = 1),
    strata = list(center = c("C01", "C02")),
    center_var = "center",
    block_sizes = c(4),
    generate_report = TRUE,
    generate_random_envelope = TRUE,
    generate_emergency_envelope = TRUE,
    output_dir = dir
  )
  expect_true(file.exists(res$files$report))
  expect_true(file.exists(res$files$random_envelope))
  expect_true(file.exists(res$files$emergency_envelope))
  expect_equal(nrow(res$allocation_table), 8)
})

test_that("stratified block errors when total cannot satisfy per-stratum minimum blocks", {
  dir <- tempfile("rp-strat-block-invalid-")

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "stratified_block",
      seed = 654,
      n_total = 8,
      allocation_ratio = c(A = 1, B = 1),
      strata = list(center = c("C01", "C02", "C03")),
      block_sizes = c(4),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "n_total|stratified_block|block"
  )
})

test_that("stratified block uses reachable mixed block totals across strata", {
  dir <- tempfile("rp-strat-block-mixed-")
  res <- rp_randomize(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    method = "stratified_block",
    seed = 654,
    n_total = 12,
    allocation_ratio = c(A = 1, B = 1),
    strata = list(center = c("C01", "C02")),
    block_sizes = c(4, 8),
    generate_report = FALSE,
    generate_reproducibility = FALSE,
    output_dir = dir
  )

  stratum_counts <- sort(as.integer(table(res$allocation_table$stratum_id)))
  expect_equal(nrow(res$allocation_table), 12L)
  expect_identical(stratum_counts, c(4L, 8L))
})

test_that("stratified block errors on explicit zero-count strata", {
  dir <- tempfile("rp-strat-block-zero-")

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "stratified_block",
      seed = 654,
      n_total = 8,
      allocation_ratio = c(A = 1, B = 1),
      strata = list(center = c("C01", "C02")),
      stratum_n = c(stratum_01 = 0L, stratum_02 = 8L),
      block_sizes = c(4),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "zero-count strata|stratum_n"
  )
})

test_that("explicit stratum_n must sum to n_total", {
  dir <- tempfile("rp-stratum-n-total-")

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "stratified",
      seed = 654,
      n_total = 8,
      allocation_ratio = c(A = 1, B = 1),
      strata = list(center = c("C01", "C02")),
      stratum_n = c(stratum_01 = 2L, stratum_02 = 3L),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "stratum_n.*sum to n_total"
  )
})

test_that("explicit stratum_n rejects NA and negative values", {
  dir <- tempfile("rp-stratum-n-invalid-")

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "stratified",
      seed = 654,
      n_total = 8,
      allocation_ratio = c(A = 1, B = 1),
      strata = list(center = c("C01", "C02")),
      stratum_n = c(stratum_01 = NA_integer_, stratum_02 = 8L),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "stratum_n.*must not be NA"
  )

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "stratified",
      seed = 654,
      n_total = 8,
      allocation_ratio = c(A = 1, B = 1),
      strata = list(center = c("C01", "C02")),
      stratum_n = c(stratum_01 = -1L, stratum_02 = 9L),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "stratum_n.*non-negative"
  )

  expect_error(
    rp_randomize(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      method = "stratified",
      seed = 654,
      n_total = 8,
      allocation_ratio = c(A = 1, B = 1),
      strata = list(center = c("C01", "C02")),
      stratum_n = c(stratum_01 = 4L, stratum_01 = 4L),
      generate_report = FALSE,
      generate_reproducibility = FALSE,
      output_dir = dir
    ),
    "stratum_n.*unique"
  )
})

test_that("stratified outputs use numbered strata and keep factor columns", {
  dir <- tempfile("rp-strata-")
  res <- rp_randomize(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    method = "stratified_block",
    seed = 654,
    n_total = 16,
    allocation_ratio = c(A = 1, B = 1),
    strata = list(sex = c("F", "M"), age = c("Low", "High")),
    block_sizes = c(4),
    generate_report = TRUE,
    generate_random_envelope = TRUE,
    generate_emergency_envelope = TRUE,
    output_dir = dir
  )

  expect_setequal(unique(res$allocation_table$stratum_id), sprintf("stratum_%02d", 1:4))
  expect_true(all(grepl("^No\\. [0-9]{3}$", res$allocation_table$envelope_no)))
  expect_false(any(grepl("stratum_", res$allocation_table$envelope_no, fixed = TRUE)))
  expect_true(all(c("stratum_id", "sex", "age") %in% names(res$allocation_table)))
  expect_true(all(c("stratum_id", "sex", "age") %in% names(res$blinded_table)))

  combos <- unique(res$allocation_table[c("stratum_id", "sex", "age")])
  expect_equal(nrow(combos), 4)
  expect_true(all(table(combos$stratum_id) == 1))

  csv <- utils::read.csv(res$files$blinded_site_table, fileEncoding = "UTF-8-BOM", check.names = FALSE)
  expect_true(all(c("stratum_id", "sex", "age") %in% names(csv)))

  xlsx <- openxlsx::read.xlsx(res$files$report, sheet = "Unblinded Master")
  expect_true(all(c("stratum_id", "sex", "age") %in% names(xlsx)))

  expect_identical(.rp_envelope_strata_cols(res$allocation_table), c("sex", "age"))
  first_display <- .rp_stratum_display(res$allocation_table[1, , drop = FALSE], c("sex", "age"))
  expect_match(first_display, "^sex:.+;age:.+$")
  expect_false(grepl("stratum_", first_display, fixed = TRUE))
})

test_that("minimization session assigns one participant", {
  dir <- tempfile("rp-min-")
  sess <- rp_minimization_session(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    factors = c("center", "sex"),
    weights = c(center = 1, sex = 1),
    seed = 789,
    output_dir = dir
  )
  out <- rp_assign_next(sess, "S001", list(center = "C01", sex = "F"))
  expect_equal(nrow(out$session$assignments), 1)
  expect_true(file.exists(file.path(dir, "minimization_assignments.csv")))
})

test_that("minimization supports second and later assignments", {
  dir <- tempfile("rp-min-second-")
  sess <- rp_minimization_session(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    factors = c("center", "sex"),
    weights = c(center = 1, sex = 1),
    seed = 789,
    output_dir = dir
  )

  first <- rp_assign_next(sess, "S001", list(center = "C01", sex = "F"))
  second <- rp_assign_next(first$session, "S002", list(center = "C02", sex = "M"))

  expect_equal(nrow(second$session$assignments), 2)
  expect_identical(second$session$assignments$subject_id, c("S001", "S002"))
})

test_that("minimization preserves named weights supplied out of factor order", {
  dir <- tempfile("rp-min-weights-")
  sess <- rp_minimization_session(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    factors = c("center", "sex"),
    weights = c(sex = 2, center = 1),
    seed = 789,
    output_dir = dir
  )

  expect_identical(sess$weights, c(center = 1, sex = 2))
})

test_that("minimization errors on invalid allocation_ratio names", {
  dir <- tempfile("rp-min-bad-ratio-")

  expect_error(
    rp_minimization_session(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      allocation_ratio = c(1, 2),
      factors = c("center", "sex"),
      weights = c(center = 1, sex = 1),
      seed = 789,
      output_dir = dir
    ),
    "allocation_ratio.*named|group_id"
  )
})

test_that("minimization errors on duplicate allocation_ratio names", {
  dir <- tempfile("rp-min-dup-ratio-")

  expect_error(
    rp_minimization_session(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      allocation_ratio = c(A = 1, A = 9, B = 1),
      factors = c("center", "sex"),
      weights = c(center = 1, sex = 1),
      seed = 789,
      output_dir = dir
    ),
    "allocation_ratio.*unique"
  )
})

test_that("minimization errors when named weights do not match factors", {
  dir <- tempfile("rp-min-bad-weights-")

  expect_error(
    rp_minimization_session(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      factors = c("center", "sex"),
      weights = c(site = 1, sex = 2),
      seed = 789,
      output_dir = dir
    ),
    "weights.*factor|named"
  )
})

test_that("minimization errors on duplicate named weights", {
  dir <- tempfile("rp-min-dup-weights-")

  expect_error(
    rp_minimization_session(
      project_name = "Trial",
      protocol_no = "P-001",
      sponsor_name = "Sponsor",
      interventions = c(A = "Test", B = "Control"),
      factors = c("center", "sex"),
      weights = c(center = 1, center = 2, sex = 3),
      seed = 789,
      output_dir = dir
    ),
    "duplicate|weights"
  )
})

test_that("envelope rows use 申办单位 label", {
  row <- data.frame(envelope_no = "No. 001", stringsAsFactors = FALSE)
  rows <- .rp_envelope_common_rows(
    row = row,
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor Unit",
    center_col = NULL,
    stratum_display = NULL
  )

  expect_true(any(grepl("申办单位名称", rows$label, fixed = TRUE)))
  expect_false(any(grepl("申办方名称", rows$label, fixed = TRUE)))
})

test_that("reproducibility script embeds package functions and runs standalone", {
  dir <- tempfile("rp-repro-")
  res <- rp_randomize(
    project_name = "Trial",
    protocol_no = "P-001",
    sponsor_name = "Sponsor",
    interventions = c(A = "Test", B = "Control"),
    method = "stratified_block",
    seed = 20260529,
    n_total = 8,
    allocation_ratio = c(A = 1, B = 1),
    strata = list(center = c("C01", "C02")),
    stratum_n = c(stratum_01 = 4L, stratum_02 = 4L),
    center_var = "center",
    block_sizes = c(4),
    random_no_by_center = TRUE,
    code_by_group = TRUE,
    generate_report = FALSE,
    generate_random_envelope = FALSE,
    generate_emergency_envelope = FALSE,
    generate_reproducibility = TRUE,
    standalone_reproducibility_code = TRUE,
    output_dir = dir
  )

  hash_lines <- readLines(res$files$hashes, warn = FALSE, encoding = "UTF-8")
  copied_root <- tempfile("rp-repro-copy-")
  env <- new.env(parent = baseenv())

  dir.create(copied_root, recursive = TRUE, showWarnings = FALSE)
  expect_true(file.copy(res$files$reproducibility_dir, copied_root, recursive = TRUE))
  unlink(res$files$reproducibility_dir, recursive = TRUE, force = TRUE)

  script_path <- file.path(copied_root, "reproducibility", "reproduce_randomization.R")
  script <- paste(readLines(script_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(file.exists(script_path))
  expect_true(grepl("rp_randomize <- function", script, fixed = TRUE))
  expect_true(grepl(".rp_write_reproducibility_bundle <- function", script, fixed = TRUE))
  expect_true(grepl("rp_launch_app <- function", script, fixed = TRUE))
  expect_true(grepl("res <- do.call(rp_randomize", script, fixed = TRUE))
  expect_true(any(grepl("^reproduce_randomization\\.R:", hash_lines)))
  expect_true(any(grepl("^audit_log\\.jsonl:", hash_lines)))
  expect_message(sys.source(script_path, envir = env), "Reproducibility check passed")
  expect_true(exists("rp_randomize", envir = env, inherits = FALSE))
})

test_that("emergency insert sensitive note height grows to fit wrapped text", {
  legacy_required_height <- .rp_note_box_required_height(
    .rp_emergency_insert_sensitive_note_lines(),
    line_gap = 0.03,
    paragraph_gap = 0.007,
    wrap_width = 72
  )
  note_spec <- .rp_emergency_insert_sensitive_note_spec(y_after = 0.395)
  required_height <- .rp_note_box_required_height(
    note_spec$lines,
    line_gap = note_spec$line_gap,
    paragraph_gap = note_spec$paragraph_gap,
    wrap_width = note_spec$wrap_width
  )

  expect_gt(legacy_required_height, 0.11)
  expect_gte(note_spec$height, required_height)
  expect_gt(note_spec$y_top - note_spec$height, 0.05)
})
