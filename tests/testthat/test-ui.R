test_that("UI launcher and app objects are available", {
  expect_true(is.function(rp_launch_app))
  expect_true(identical(formals(rp_launch_app)$stop_on_session_end, TRUE))
  expect_s3_class(rp_app_ui(), "shiny.tag.list")
  expect_true(is.function(rp_app_server))
})

test_that("UI allocation keeps group names for direct sample sizes", {
  input <- list(
    group_count = 2,
    group_id_1 = "A",
    group_id_2 = "B",
    intervention_name_1 = "Drug",
    intervention_name_2 = "Control",
    blind_label = "Treatment",
    ratio_1 = 1,
    ratio_2 = 1,
    n_group_1 = 12,
    n_group_2 = 8,
    sample_input_mode = "n_per_group"
  )
  interventions <- rp_collect_interventions(input)
  alloc <- rp_collect_allocation(input, interventions)
  expect_identical(interventions$blind_label, c("Treatment", "Treatment"))
  expect_named(alloc$n_per_group, c("A", "B"))
  expect_identical(unname(alloc$n_per_group), c(12L, 8L))
})

test_that("ratio based sample sizes use largest remainder and keep names", {
  ratio <- c(A = 1, B = 1, C = 1)
  out <- rp_group_n_from_ratio(10, ratio)
  expect_named(out, c("A", "B", "C"))
  expect_equal(sum(out), 10)
  expect_identical(unname(out), c(4L, 3L, 3L))
})

test_that("UI call arguments keep named n_per_group", {
  input <- list(
    project_name = "Study",
    protocol_no = "P001",
    sponsor_name = "Sponsor",
    language = "zh-CN",
    output_dir = tempdir(),
    encrypt_sensitive_outputs = FALSE,
    password = "",
    group_count = 2,
    group_id_1 = "A",
    group_id_2 = "B",
    intervention_name_1 = "Drug",
    intervention_name_2 = "Control",
    blind_label = "Treatment",
    ratio_1 = 1,
    ratio_2 = 1,
    n_group_1 = 12,
    n_group_2 = 8,
    sample_input_mode = "n_per_group",
    method = "simple",
    seed = 20260528,
    strata_count = 0,
    center_var = "<none>",
    block_sizes = "4",
    block_size_probs = "",
    random_no_prefix = "R",
    random_no_width = 3,
    random_no_by_center = FALSE,
    code_prefix = "D",
    code_width = 4,
    code_random_digits = FALSE,
    code_range_min = 1000,
    code_range_max = 9999,
    code_by_group = FALSE,
    envelope_no_prefix = "No.",
    envelope_no_width = 3,
    generate_random_envelope = FALSE,
    random_envelope_file = "random_envelopes.pdf",
    generate_emergency_envelope = FALSE,
    emergency_envelope_file = "emergency_unblinding_envelopes.pdf",
    generate_report = TRUE,
    report_file = "randomization_report.xlsx",
    generate_reproducibility = TRUE,
    standalone_reproducibility_code = TRUE
  )
  args <- rp_build_call_args(input)
  expect_named(args$n_per_group, c("A", "B"))
  expect_null(args$n_total)
  expect_identical(args$interventions$blind_label, c("Treatment", "Treatment"))
})

test_that("standard UI no longer exposes minimization module", {
  html <- paste(as.character(rp_app_ui()), collapse = "\n")
  expect_false(grepl("minimization", html, fixed = TRUE))
})

test_that("UI uses 申办单位 wording", {
  html <- paste(as.character(rp_app_ui()), collapse = "\n")
  expect_true(grepl("申办单位名称", html, fixed = TRUE))
  expect_false(grepl("申办方名称", html, fixed = TRUE))
})

test_that("UI does not claim outputs are encrypted", {
  html <- paste(as.character(rp_app_ui()), collapse = "\n")
  expect_true(grepl("敏感输出密码必填检查（当前不加密文件）", html, fixed = TRUE))
  expect_true(grepl("密码（当前仅校验是否填写）", html, fixed = TRUE))
  expect_false(grepl("启用敏感输出文件加密", html, fixed = TRUE))
})

test_that("UI verification code no longer compares the table to itself", {
  server_text <- paste(readLines("D:/tmpProject/randomPlatform/R/app_server.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("rp_verify_reproducibility\\(result\\(\\)\\$allocation_table, result\\(\\)\\$allocation_table\\)", server_text))
  expect_true(grepl("sys.source", server_text, fixed = TRUE))
})
