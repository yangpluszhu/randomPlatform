.rp_match_method <- function(method, choices) {
  method <- match.arg(method, choices)
  method
}

.rp_required <- function(value, label) {
  if (missing(value) || is.null(value) || length(value) == 0 || all(is.na(value)) ||
      (is.character(value) && !nzchar(trimws(value[1])))) {
    stop(label, " is required.", call. = FALSE)
  }
  invisible(value)
}

.rp_normalize_interventions <- function(interventions) {
  if (is.data.frame(interventions)) {
    required <- c("group_id", "intervention_name")
    missing_cols <- setdiff(required, names(interventions))
    if (length(missing_cols) > 0) {
      stop("interventions data frame must contain: ", paste(required, collapse = ", "), call. = FALSE)
    }
    out <- interventions[, intersect(c("group_id", "intervention_name", "blind_label", "allocation_label"), names(interventions)), drop = FALSE]
  } else if (is.character(interventions) && !is.null(names(interventions))) {
    out <- data.frame(
      group_id = names(interventions),
      intervention_name = unname(interventions),
      stringsAsFactors = FALSE
    )
  } else {
    stop("interventions must be a named character vector or a data frame.", call. = FALSE)
  }

  out$group_id <- as.character(out$group_id)
  out$intervention_name <- as.character(out$intervention_name)
  if (!"blind_label" %in% names(out)) out$blind_label <- rep("Treatment", nrow(out))
  if (!"allocation_label" %in% names(out)) out$allocation_label <- out$intervention_name
  out$blind_label <- as.character(out$blind_label)
  out$allocation_label <- as.character(out$allocation_label)

  if (nrow(out) < 2 || nrow(out) > 5) {
    stop("The number of intervention groups must be between 2 and 5.", call. = FALSE)
  }
  if (anyDuplicated(out$group_id)) {
    stop("Intervention group_id values must be unique.", call. = FALSE)
  }
  if (any(!nzchar(trimws(out$group_id))) || any(!nzchar(trimws(out$intervention_name)))) {
    stop("Intervention group_id and intervention_name values cannot be empty.", call. = FALSE)
  }
  out
}

.rp_calc_group_n <- function(groups, n_total = NULL, n_per_group = NULL, allocation_ratio = NULL) {
  if (!is.null(n_per_group)) {
    n_per_group <- as.integer(n_per_group)
    if (is.null(names(n_per_group))) stop("n_per_group must be named.", call. = FALSE)
    if (!setequal(names(n_per_group), groups)) stop("n_per_group names must match intervention group_id values.", call. = FALSE)
    n_per_group <- n_per_group[groups]
    if (any(n_per_group < 0) || sum(n_per_group) <= 0) stop("n_per_group values must be non-negative and sum to a positive value.", call. = FALSE)
    return(n_per_group)
  }

  .rp_required(n_total, "n_total")
  .rp_required(allocation_ratio, "allocation_ratio")
  allocation_ratio_names <- names(allocation_ratio)
  allocation_ratio <- as.numeric(allocation_ratio)
  names(allocation_ratio) <- allocation_ratio_names
  if (is.null(names(allocation_ratio))) stop("allocation_ratio must be named.", call. = FALSE)
  if (!setequal(names(allocation_ratio), groups)) stop("allocation_ratio names must match intervention group_id values.", call. = FALSE)
  allocation_ratio <- allocation_ratio[groups]
  if (any(allocation_ratio <= 0)) stop("allocation_ratio values must be positive.", call. = FALSE)
  n_total <- as.integer(n_total)
  if (is.na(n_total) || n_total <= 0) stop("n_total must be a positive integer.", call. = FALSE)

  raw_n <- n_total * allocation_ratio / sum(allocation_ratio)
  base_n <- floor(raw_n)
  remainder <- n_total - sum(base_n)
  if (remainder > 0) {
    order_add <- order(raw_n - base_n, decreasing = TRUE)
    base_n[order_add[seq_len(remainder)]] <- base_n[order_add[seq_len(remainder)]] + 1L
  }
  stats::setNames(as.integer(base_n), groups)
}

.rp_parse_strata <- function(strata) {
  if (is.null(strata)) return(NULL)
  if (!is.list(strata) || is.null(names(strata)) || any(!nzchar(names(strata)))) {
    stop("strata must be a named list.", call. = FALSE)
  }
  if (length(strata) > 6) stop("A maximum of 6 stratification factors is supported.", call. = FALSE)
  strata <- lapply(strata, function(x) unique(as.character(x[nzchar(trimws(as.character(x)))])))
  if (any(lengths(strata) == 0)) stop("Each stratification factor must have at least one level.", call. = FALSE)
  strata
}

.rp_make_stratum_id <- function(strata_row) {
  parts <- vapply(names(strata_row), function(nm) {
    paste0(nm, as.character(strata_row[[nm]]))
  }, character(1))
  paste(parts, collapse = "_")
}

.rp_stratum_grid <- function(strata) {
  if (is.null(strata)) {
    return(data.frame(stratum_id = "S001", stringsAsFactors = FALSE))
  }
  grid <- do.call(expand.grid, c(strata, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
  grid$stratum_id <- paste0("stratum_", sprintf("%02d", seq_len(nrow(grid))))
  grid[, c("stratum_id", names(strata)), drop = FALSE]
}

.rp_alloc_simple <- function(n_per_group) {
  data.frame(
    sequence_no = seq_len(sum(n_per_group)),
    group_id = sample(rep(names(n_per_group), n_per_group)),
    block_id = NA_integer_,
    block_size = NA_integer_,
    stringsAsFactors = FALSE
  )
}

.rp_default_block_sizes <- function(allocation_ratio) {
  ratio_sum <- sum(allocation_ratio)
  if (length(allocation_ratio) == 2 && all(allocation_ratio == 1)) return(c(4L, 6L, 8L))
  if (length(allocation_ratio) == 3 && all(allocation_ratio == 1)) return(c(6L, 9L, 12L))
  as.integer(ratio_sum * c(2L, 3L, 4L))
}

.rp_is_reachable_total <- function(total, block_sizes) {
  total <- as.integer(total)
  block_sizes <- unique(as.integer(block_sizes[block_sizes > 0]))
  if (is.na(total) || total < 0 || length(block_sizes) == 0) return(FALSE)
  reachable <- rep(FALSE, total + 1L)
  reachable[1] <- TRUE
  for (i in seq_len(total + 1L)) {
    if (!reachable[i]) next
    current <- i - 1L
    next_totals <- current + block_sizes
    next_totals <- next_totals[next_totals <= total]
    reachable[next_totals + 1L] <- TRUE
  }
  reachable[total + 1L]
}

.rp_alloc_block <- function(n, allocation_ratio, block_sizes = NULL, block_size_probs = NULL) {
  groups <- names(allocation_ratio)
  allocation_ratio <- as.integer(allocation_ratio)
  ratio_sum <- sum(allocation_ratio)
  if (is.null(block_sizes) || length(block_sizes) == 0) block_sizes <- .rp_default_block_sizes(allocation_ratio)
  block_sizes <- as.integer(block_sizes)
  valid <- block_sizes[block_sizes %% ratio_sum == 0 & block_sizes > 0]
  if (length(valid) == 0) stop("block_sizes must contain values that are positive multiples of the allocation ratio sum.", call. = FALSE)
  if (!.rp_is_reachable_total(n, valid)) {
    stop("n_total is incompatible with the available block_sizes and allocation_ratio.", call. = FALSE)
  }
  if (!is.null(block_size_probs)) {
    block_size_probs <- as.numeric(block_size_probs)
    if (length(block_size_probs) != length(block_sizes)) stop("block_size_probs must have the same length as block_sizes.", call. = FALSE)
    block_size_probs <- block_size_probs[block_sizes %in% valid]
    block_size_probs <- block_size_probs / sum(block_size_probs)
  }

  out <- character()
  block_id <- integer()
  block_size_record <- integer()
  current_block <- 1L
  remaining <- n
  while (remaining > 0) {
    feasible <- valid[valid <= remaining & vapply(remaining - valid, .rp_is_reachable_total, logical(1), block_sizes = valid)]
    if (length(feasible) == 0) {
      stop("n_total is incompatible with the available block_sizes and allocation_ratio.", call. = FALSE)
    }
    probs <- NULL
    if (!is.null(block_size_probs)) {
      probs <- block_size_probs[match(feasible, valid)]
      probs <- probs / sum(probs)
    }
    bsize <- if (length(feasible) == 1L) feasible[1L] else sample(feasible, 1L, prob = probs)
    multiplier <- as.integer(bsize / ratio_sum)
    block <- rep(groups, times = allocation_ratio * multiplier)
    block <- sample(block)
    out <- c(out, block)
    block_id <- c(block_id, rep(current_block, length(block)))
    block_size_record <- c(block_size_record, rep(bsize, length(block)))
    current_block <- current_block + 1L
    remaining <- remaining - bsize
  }
  data.frame(
    sequence_no = seq_len(n),
    group_id = out,
    block_id = block_id,
    block_size = block_size_record,
    stringsAsFactors = FALSE
  )
}

.rp_distribute_n_across_strata <- function(n_total, grid, stratum_n = NULL) {
  if (!is.null(stratum_n)) {
    if (is.data.frame(stratum_n)) {
      if (!all(c("stratum_id", "n") %in% names(stratum_n))) stop("stratum_n data frame must contain stratum_id and n.", call. = FALSE)
      out <- stats::setNames(as.integer(stratum_n$n), stratum_n$stratum_id)
    } else {
      out_values <- as.integer(stratum_n)
      out <- stats::setNames(out_values, names(stratum_n))
    }
    if (is.null(names(out))) stop("stratum_n must be named by stratum_id.", call. = FALSE)
    if (anyDuplicated(names(out))) stop("stratum_n names must be unique.", call. = FALSE)
    if (!setequal(names(out), grid$stratum_id)) stop("stratum_n names must match generated stratum_id values.", call. = FALSE)
    if (any(is.na(out))) stop("stratum_n values must not be NA.", call. = FALSE)
    if (any(out < 0)) stop("stratum_n values must be non-negative.", call. = FALSE)
    if (sum(out) != as.integer(n_total)) stop("stratum_n must sum to n_total.", call. = FALSE)
    return(out[grid$stratum_id])
  }
  base_n <- rep(floor(n_total / nrow(grid)), nrow(grid))
  extra <- n_total - sum(base_n)
  if (extra > 0) base_n[seq_len(extra)] <- base_n[seq_len(extra)] + 1L
  stats::setNames(as.integer(base_n), grid$stratum_id)
}

.rp_align_strata_n_to_blocks <- function(strata_n, valid_blocks, n_total) {
  strata_n <- as.integer(strata_n)
  min_block <- min(valid_blocks)
  n_strata <- length(strata_n)
  if (n_strata * min_block > n_total) {
    stop("n_total is incompatible with stratified_block allocation under the available block_sizes.", call. = FALSE)
  }

  candidate_totals <- seq.int(min_block, n_total)
  candidate_totals <- candidate_totals[vapply(candidate_totals, .rp_is_reachable_total, logical(1), block_sizes = valid_blocks)]
  if (length(candidate_totals) == 0L) {
    stop("n_total is incompatible with stratified_block allocation under the available block_sizes.", call. = FALSE)
  }

  dp <- matrix(Inf, nrow = n_strata + 1L, ncol = n_total + 1L)
  choice <- matrix(NA_integer_, nrow = n_strata, ncol = n_total + 1L)
  dp[1L, 1L] <- 0
  min_candidate <- min(candidate_totals)
  max_candidate <- max(candidate_totals)

  for (i in seq_len(n_strata)) {
    for (sum_prev in 0:n_total) {
      prev_cost <- dp[i, sum_prev + 1L]
      if (!is.finite(prev_cost)) next
      remaining_slots <- n_strata - i
      for (candidate in candidate_totals) {
        sum_new <- sum_prev + candidate
        if (sum_new > n_total) next
        remaining_total <- n_total - sum_new
        if (remaining_slots == 0L) {
          if (remaining_total != 0L) next
        } else {
          if (remaining_total < remaining_slots * min_candidate) next
          if (remaining_total > remaining_slots * max_candidate) next
        }
        new_cost <- prev_cost + (candidate - strata_n[i])^2
        if (new_cost < dp[i + 1L, sum_new + 1L]) {
          dp[i + 1L, sum_new + 1L] <- new_cost
          choice[i, sum_new + 1L] <- candidate
        }
      }
    }
  }

  if (!is.finite(dp[n_strata + 1L, n_total + 1L])) {
    stop("n_total is incompatible with stratified_block allocation under the available block_sizes.", call. = FALSE)
  }

  aligned <- integer(n_strata)
  remaining <- n_total
  for (i in seq.int(n_strata, 1L)) {
    candidate <- choice[i, remaining + 1L]
    aligned[i] <- candidate
    remaining <- remaining - candidate
  }
  stats::setNames(aligned, names(strata_n))
}

.rp_alloc_stratified <- function(method, n_total, n_per_group, allocation_ratio, strata, stratum_n, block_sizes, block_size_probs) {
  grid <- .rp_stratum_grid(strata)
  strata_n <- .rp_distribute_n_across_strata(n_total, grid, stratum_n)

  if (identical(method, "stratified_block")) {
    ratio_sum <- sum(as.integer(allocation_ratio))
    valid_blocks <- block_sizes
    if (is.null(valid_blocks) || length(valid_blocks) == 0) valid_blocks <- .rp_default_block_sizes(allocation_ratio)
    valid_blocks <- as.integer(valid_blocks)
    valid_blocks <- valid_blocks[valid_blocks %% ratio_sum == 0 & valid_blocks > 0]
    if (length(valid_blocks) == 0L) {
      stop("block_sizes must contain values that are positive multiples of the allocation ratio sum.", call. = FALSE)
    }
    if (!is.null(stratum_n) && any(strata_n == 0L)) {
      stop("stratum_n for stratified_block cannot include zero-count strata.", call. = FALSE)
    }
    strata_n <- .rp_align_strata_n_to_blocks(strata_n, valid_blocks, n_total)
  }

  all_res <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    local_group_n <- .rp_calc_group_n(names(n_per_group), n_total = strata_n[i], allocation_ratio = allocation_ratio)
    local_ratio <- if (is.null(allocation_ratio)) n_per_group else allocation_ratio[names(n_per_group)]
    if (identical(method, "stratified_block")) {
      tmp <- .rp_alloc_block(strata_n[i], local_ratio, block_sizes, block_size_probs)
    } else {
      tmp <- .rp_alloc_simple(local_group_n)
    }
    tmp$stratum_id <- grid$stratum_id[i]
    for (nm in setdiff(names(grid), "stratum_id")) tmp[[nm]] <- grid[[nm]][i]
    all_res[[i]] <- tmp
  }
  res <- do.call(rbind, all_res)
  rownames(res) <- NULL
  res$sequence_no <- seq_len(nrow(res))
  res
}

.rp_make_numbers <- function(n, prefix, width) {
  paste0(prefix, sprintf(paste0("%0", as.integer(width), "d"), seq_len(n)))
}

.rp_generate_codes <- function(group_id, code_prefix, code_width, code_random_digits, code_random_range, code_by_group) {
  n <- length(group_id)
  if (code_by_group) {
    if (length(code_prefix) == 1L) {
      prefix <- paste0(code_prefix, "-", group_id)
    } else {
      if (is.null(names(code_prefix)) || !all(unique(group_id) %in% names(code_prefix))) {
        stop("When code_by_group is TRUE, code_prefix must be named by group_id or have length 1.", call. = FALSE)
      }
      prefix <- unname(code_prefix[group_id])
    }
  } else {
    prefix <- rep(code_prefix[1], n)
  }

  if (isTRUE(code_random_digits)) {
    pool <- seq.int(as.integer(code_random_range[1]), as.integer(code_random_range[2]))
    if (length(pool) < n) stop("code_random_range is too small to generate unique codes.", call. = FALSE)
    digits <- sample(pool, n, replace = FALSE)
  } else {
    digits <- seq_len(n)
  }
  paste0(prefix, "-", sprintf(paste0("%0", as.integer(code_width), "d"), digits))
}

.rp_set_rng <- function(seed, rng_kind, normal_kind, sample_kind) {
  do.call(RNGkind, list(kind = rng_kind, normal.kind = normal_kind, sample.kind = sample_kind))
  set.seed(seed)
}

.rp_file_sha256 <- function(paths) {
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  stats::setNames(vapply(paths, digest::digest, character(1), file = TRUE, algo = "sha256"), basename(paths))
}

.rp_table_hash <- function(x) {
  digest::digest(x, algo = "sha256")
}

.rp_write_audit <- function(output_dir, event, message, payload = list()) {
  entry <- c(
    list(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      event = event,
      message = message
    ),
    payload
  )
  cat(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null"), "\n",
      file = file.path(output_dir, "audit_log.jsonl"), append = TRUE)
}

.rp_validate_output_dir <- function(output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  test_file <- tempfile(tmpdir = output_dir)
  ok <- tryCatch({ file.create(test_file); unlink(test_file); TRUE }, error = function(e) FALSE)
  if (!ok) stop("output_dir must be writable.", call. = FALSE)
  normalizePath(output_dir, winslash = "/", mustWork = TRUE)
}

#' Generate a clinical trial randomization package.
#'
#' @param project_name Character scalar naming the randomization project. Must be
#'   supplied and is stored in the returned object and generated report.
#' @param protocol_no Character scalar for the protocol number. Must be
#'   supplied and is stored in the returned object and generated report.
#' @param sponsor_name Character scalar naming the sponsor. Must be supplied and
#'   is stored in the returned object and generated report.
#' @param interventions A named character vector or data frame describing the
#'   treatment groups. When a vector is supplied, names are used as `group_id`
#'   values and values are used as `intervention_name`. When a data frame is
#'   supplied, it must contain `group_id` and `intervention_name`, and may also
#'   contain `blind_label` and `allocation_label`. The number of groups must be
#'   between 2 and 5, `group_id` values must be unique, and neither `group_id`
#'   nor `intervention_name` may be empty.
#' @param method Allocation method to use. One of `"simple"`, `"block"`,
#'   `"stratified"`, or `"stratified_block"`. Default: `"simple"`.
#' @param seed Integer seed used to initialize the RNG before allocation.
#'   Required.
#' @param n_total Total sample size. Default: `NULL`. Used when `n_per_group` is
#'   not supplied, and also used as the total sample size that is distributed
#'   across strata for stratified methods.
#' @param n_per_group Named integer vector of exact group counts. Default:
#'   `NULL`. For `method = "simple"`, these counts are used directly. For
#'   `"block"` and `"stratified*"`, the values are summed to determine the
#'   total sample size, while the actual group proportions still come from
#'   `allocation_ratio`.
#' @param allocation_ratio Numeric or integer vector of relative group
#'   weights. Default: `NULL`. May be named or unnamed; if unnamed, `group_id`
#'   names are assigned in group order before validation. Named vectors must use
#'   unique names that match `group_id` values exactly. If `NULL`, equal weights
#'   are used. When `n_per_group` is `NULL`, this argument is used to derive
#'   per-group counts for `"simple"`, `"block"`, and `"stratified*"` methods.
#' @param strata Named list of stratification factors. Default: `NULL`.
#'   Required for `"stratified"` and `"stratified_block"`. Each element is
#'   reduced to unique, non-empty character levels, and at most 6 factors are
#'   supported.
#' @param stratum_n Optional per-stratum sample sizes. Default: `NULL`. Supply
#'   either a named vector named by the generated `stratum_id` values or a data
#'   frame with `stratum_id` and `n`. Names must be unique, cover every
#'   generated `stratum_id`, and values must be non-negative, non-`NA`, and sum
#'   to `n_total`. If omitted, the total sample size is spread as evenly as
#'   possible across strata. For `"stratified_block"`, supplied stratum totals
#'   must be strictly positive and may be adjusted to a reachable allocation
#'   compatible with the available block sizes; impossible requests error.
#' @param center_var Optional stratification factor name identifying the center
#'   column. Default: `NULL`. If supplied, it must match one of the names in
#'   `strata`. It can drive center-specific random numbers, center rows in
#'   `balance_summary`, and center-aware envelope output.
#' @param block_sizes Optional integer vector of block sizes. Default: `NULL`.
#'   Used by `"block"` and `"stratified_block"`; ignored by `"simple"` and the
#'   current `"stratified"` path. Retained block sizes must be positive
#'   multiples of the allocation-ratio sum. For `method = "block"`, an
#'   incompatible effective total sample size now raises an error instead of
#'   returning fewer rows than requested.
#' @param block_size_probs Optional probabilities for `block_sizes`. Default:
#'   `NULL`. When supplied, the vector must have the same length as
#'   `block_sizes`; the values are renormalized over the retained valid block
#'   sizes.
#' @param random_no_prefix Character prefix for `random_no` values. Default:
#'   `"R"`. The prefix is followed by a zero-padded numeric sequence.
#' @param random_no_width Integer width used to zero-pad `random_no` values.
#'   Default: `3`.
#' @param random_no_by_center Logical; if `TRUE` and `center_var` is supplied,
#'   prepend the center value to `random_no` values within each center. Default:
#'   `FALSE`. If `center_var` is not supplied, this flag has no effect.
#' @param code_prefix Character prefix for `drug_code` values. Default:
#'   `"MED"`. When `code_by_group = FALSE`, the first value is used for all
#'   groups. When `code_by_group = TRUE`, supply either a length-1 prefix or a
#'   named vector whose names cover every `group_id`.
#' @param code_width Integer width used to zero-pad the numeric part of
#'   `drug_code` values. Default: `4`.
#' @param code_random_digits Logical; if `TRUE`, numeric code suffixes are sampled
#'   without replacement from `code_random_range`. Default: `TRUE`. If `FALSE`,
#'   suffixes are assigned sequentially.
#' @param code_random_range Integer vector of length 2 giving the inclusive range
#'   used when `code_random_digits = TRUE`. Default: `c(1000, 9999)`. The range
#'   must be large enough to supply unique codes.
#' @param code_by_group Logical; if `TRUE`, code prefixes are group-specific as
#'   described in `code_prefix`. Default: `FALSE`. If `FALSE`, the same prefix
#'   is used for all groups.
#' @param envelope_no_prefix Character prefix for envelope numbering. Default:
#'   `"No. "`.
#' @param envelope_no_width Integer width used to zero-pad envelope numbers.
#'   Default: `3`.
#' @param generate_random_envelope Logical; if `TRUE`, write the random envelope
#'   PDF named by `random_envelope_file`. Default: `FALSE`.
#' @param random_envelope_file File name for the random envelope PDF, written
#'   under `output_dir` when `generate_random_envelope = TRUE`. Default:
#'   `"random_envelopes.pdf"`.
#' @param generate_emergency_envelope Logical; if `TRUE`, write the emergency
#'   unblinding envelope PDF named by `emergency_envelope_file`. Default:
#'   `FALSE`.
#' @param emergency_envelope_file File name for the emergency unblinding PDF,
#'   written under `output_dir` when `generate_emergency_envelope = TRUE`.
#'   Default: `"emergency_unblinding_envelopes.pdf"`.
#' @param generate_report Logical; if `TRUE`, write the Excel report named by
#'   `report_file`. Default: `TRUE`.
#' @param report_file File name for the Excel report, written under `output_dir`
#'   when `generate_report = TRUE`. Default: `"randomization_report.xlsx"`.
#' @param generate_reproducibility Logical; if `TRUE`, write the reproducibility
#'   bundle under `output_dir/reproducibility`. Default: `TRUE`.
#' @param standalone_reproducibility_code Logical; controls how
#'   `reproduce_randomization.R` is written when `generate_reproducibility = TRUE`.
#'   If `TRUE`, the script embeds the `randomPlatform` package function source
#'   definitions so reviewers can inspect the implementation directly. If
#'   `FALSE`, the script calls the installed package API. Default: `TRUE`.
#' @param output_dir Output directory for all generated files. Default: `"."`.
#'   It is created recursively if needed and must be writable.
#' @param language Character scalar currently accepted for API compatibility
#'   only. Default: `"zh-CN"`. `rp_randomize()` does not currently branch on
#'   this value.
#' @param font_family Optional font family name forwarded to the PDF envelope
#'   writers. Default: `NULL`. When `NULL`, the PDF helpers automatically choose
#'   a platform-appropriate Chinese font candidate before opening the PDF device.
#' @param encrypt_sensitive_outputs Logical; currently a boundary-validation flag
#'   only. Default: `FALSE`. When `TRUE`, `password` must be a non-empty string,
#'   but this function does not itself encrypt output files.
#' @param password Optional password used to satisfy the
#'   `encrypt_sensitive_outputs` check. Default: `NULL`. It must be a non-empty
#'   string when `encrypt_sensitive_outputs = TRUE`. Ignored when
#'   `encrypt_sensitive_outputs = FALSE`.
#' @param rng_kind Character string passed to `RNGkind()` before allocation.
#'   Default: `"Mersenne-Twister"`. Must be a valid `kind` value accepted by
#'   `RNGkind()`.
#' @param normal_kind Character string passed to `RNGkind()` before allocation.
#'   Default: `"Inversion"`. Must be a valid `normal.kind` value accepted by
#'   `RNGkind()`.
#' @param sample_kind Character string passed to `RNGkind()` before allocation.
#'   Default: `"Rejection"`. Must be a valid `sample.kind` value accepted by
#'   `RNGkind()`.
#' @param return_object Logical; if `TRUE`, return the `rp_randomization` object.
#'   Default: `TRUE`. If `FALSE`, the object is still created and returned
#'   invisibly.
#'
#' @return An object of class `rp_randomization`, returned as a list with these
#'   components:
#'   \itemize{
#'     \item `design`: project metadata, the selected method, sample-size inputs,
#'       strata, center settings, RNG kinds, and the output directory.
#'     \item `allocation_table`: the full unblinded schedule after intervention
#'       labels are joined.
#'     \item `blinded_table`: the site-facing table with allocation-restricted
#'       columns omitted.
#'     \item `unblinded_table`: the full schedule; in the current implementation
#'       this matches `allocation_table`.
#'     \item `balance_summary`: summary tables by group, and by center and/or
#'       stratum when those dimensions are available.
#'     \item `files`: paths to generated artifacts tracked by the object.
#'       Always-present entries include the blinded and unblinded CSV files and
#'       `hashes_sha256.txt`; optional entries are added when the corresponding
#'       output flags are enabled.
#'     \item `audit`: a list containing `table_hash` and `file_hashes`.
#'     \item `reproducibility`: a list containing the starting `rng_state`.
#'   }
#'
#' @details Sample size can be supplied in one of two ways. Provide `n_per_group`
#'   as a named vector for exact group counts, or provide `n_total` together
#'   with `allocation_ratio`. If `allocation_ratio` is omitted, equal weights are
#'   used. For `method = "simple"`, supplying `n_per_group` uses those exact
#'   counts; if `n_per_group` is `NULL`, per-group counts are derived from
#'   `n_total` and `allocation_ratio`. For `"block"` and `"stratified*"`, the
#'   counts are otherwise driven by `n_total`/`allocation_ratio`, while
#'   supplying `n_per_group` sets the effective total sample size via
#'   `sum(n_per_group)`.
#'
#'   Stratified methods require `strata`, which must be a named list with at
#'   most 6 factors. `stratum_n`, when supplied, must be named by the generated
#'   `stratum_id` values or provided as a data frame with `stratum_id` and `n`.
#'   For `"stratified_block"`, the requested stratum totals may be adjusted so
#'   they are compatible with the available block sizes.
#'
#'   `center_var` must name one of the stratification factors. When supplied,
#'   it can be used for center-specific random numbers, balance summaries, and
#'   envelope generation. `random_no_by_center` only has an effect when
#'   `center_var` is supplied.
#'
#'   The function always writes `blinded_site_table.csv`,
#'   `unblinded_master_table.csv`, `hashes_sha256.txt`, and `audit_log.jsonl` to
#'   `output_dir`. `audit_log.jsonl` is written on every call, but it is not
#'   currently stored in the `files` component. `generate_report`,
#'   `generate_random_envelope`, `generate_emergency_envelope`, and
#'   `generate_reproducibility` control the optional Excel, PDF, and bundle
#'   outputs.
#'
#'   `block_sizes` and `block_size_probs` are used by `"block"` and
#'   `"stratified_block"`; the current `"stratified"` path does not use them.
#'   `language` is currently accepted but unused by `rp_randomize()`. When the
#'   reproducibility bundle is generated, `standalone_reproducibility_code = TRUE`
#'   writes a standalone `reproduce_randomization.R` script that embeds the
#'   package function definitions, while `FALSE` keeps a package-based script.
#'   `encrypt_sensitive_outputs` currently performs only its password check and
#'   does not itself encrypt outputs.
#'
#' @seealso \code{\link{rp_verify_reproducibility}},
#'   \code{\link{rp_launch_app}}
#'
#' @examples
#' out_dir <- file.path(tempdir(), "rp-randomize-docs")
#' x <- rp_randomize(
#'   project_name = "Demo Trial",
#'   protocol_no = "RP-001",
#'   sponsor_name = "Demo Sponsor",
#'   interventions = c(A = "Placebo", B = "Treatment"),
#'   method = "block",
#'   seed = 20260528,
#'   n_total = 8,
#'   allocation_ratio = c(A = 1, B = 1),
#'   block_sizes = 4L,
#'   generate_report = FALSE,
#'   generate_random_envelope = FALSE,
#'   generate_emergency_envelope = FALSE,
#'   generate_reproducibility = FALSE,
#'   output_dir = out_dir
#' )
#' x$allocation_table[1:4, c("sequence_no", "group_id", "random_no")]
#'
#' @export
rp_randomize <- function(
  project_name,
  protocol_no,
  sponsor_name,
  interventions,
  method = c("simple", "block", "stratified", "stratified_block"),
  seed,
  n_total = NULL,
  n_per_group = NULL,
  allocation_ratio = NULL,
  strata = NULL,
  stratum_n = NULL,
  center_var = NULL,
  block_sizes = NULL,
  block_size_probs = NULL,
  random_no_prefix = "R",
  random_no_width = 3,
  random_no_by_center = FALSE,
  code_prefix = "MED",
  code_width = 4,
  code_random_digits = TRUE,
  code_random_range = c(1000, 9999),
  code_by_group = FALSE,
  envelope_no_prefix = "No. ",
  envelope_no_width = 3,
  generate_random_envelope = FALSE,
  random_envelope_file = "random_envelopes.pdf",
  generate_emergency_envelope = FALSE,
  emergency_envelope_file = "emergency_unblinding_envelopes.pdf",
  generate_report = TRUE,
  report_file = "randomization_report.xlsx",
  generate_reproducibility = TRUE,
  standalone_reproducibility_code = TRUE,
  output_dir = ".",
  language = "zh-CN",
  font_family = NULL,
  encrypt_sensitive_outputs = FALSE,
  password = NULL,
  rng_kind = "Mersenne-Twister",
  normal_kind = "Inversion",
  sample_kind = "Rejection",
  return_object = TRUE
) {
  .rp_required(project_name, "project_name")
  .rp_required(protocol_no, "protocol_no")
  .rp_required(sponsor_name, "sponsor_name")
  .rp_required(seed, "seed")
  method <- .rp_match_method(method, c("simple", "block", "stratified", "stratified_block"))
  requested_n_total <- n_total
  requested_n_per_group <- n_per_group
  requested_allocation_ratio <- allocation_ratio
  interventions <- .rp_normalize_interventions(interventions)
  groups <- interventions$group_id
  strata <- .rp_parse_strata(strata)
  output_dir <- .rp_validate_output_dir(output_dir)
  if (isTRUE(encrypt_sensitive_outputs) && (is.null(password) || !nzchar(password))) {
    stop("password is required when encrypt_sensitive_outputs is TRUE.", call. = FALSE)
  }

  if (is.null(allocation_ratio)) {
    allocation_ratio <- stats::setNames(rep(1, length(groups)), groups)
  }
  allocation_ratio_names <- names(allocation_ratio)
  allocation_ratio <- as.numeric(allocation_ratio)
  if (!is.null(allocation_ratio_names) && anyDuplicated(allocation_ratio_names)) {
    stop("allocation_ratio names must be unique.", call. = FALSE)
  }
  names(allocation_ratio) <- allocation_ratio_names %||% groups
  allocation_ratio <- allocation_ratio[groups]
  n_per_group <- .rp_calc_group_n(groups, n_total, n_per_group, allocation_ratio)
  n_total <- sum(n_per_group)

  if (method %in% c("stratified", "stratified_block") && is.null(strata)) {
    stop("strata is required for stratified randomization methods.", call. = FALSE)
  }
  if (!is.null(center_var) && !center_var %in% names(strata)) {
    stop("center_var must name one of the stratification factors.", call. = FALSE)
  }

  old_rng <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_rng)), add = TRUE)
  .rp_set_rng(as.integer(seed), rng_kind, normal_kind, sample_kind)
  rng_start <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

  allocation <- switch(
    method,
    simple = .rp_alloc_simple(n_per_group),
    block = .rp_alloc_block(n_total, allocation_ratio, block_sizes, block_size_probs),
    stratified = .rp_alloc_stratified(method, n_total, n_per_group, allocation_ratio, strata, stratum_n, block_sizes, block_size_probs),
    stratified_block = .rp_alloc_stratified(method, n_total, n_per_group, allocation_ratio, strata, stratum_n, block_sizes, block_size_probs)
  )
  allocation$global_block_id <- ifelse(is.na(allocation$block_id), NA_integer_, as.integer(factor(paste(allocation$stratum_id %||% "GLOBAL", allocation$block_id))))
  allocation$random_no <- .rp_make_numbers(nrow(allocation), random_no_prefix, random_no_width)
  if (isTRUE(random_no_by_center) && !is.null(center_var) && center_var %in% names(allocation)) {
    by_center <- split(seq_len(nrow(allocation)), allocation[[center_var]], drop = TRUE)
    random_no <- character(nrow(allocation))
    for (center in names(by_center)) {
      idx <- by_center[[center]]
      random_no[idx] <- paste0(center, "-", .rp_make_numbers(length(idx), random_no_prefix, random_no_width))
    }
    allocation$random_no <- random_no
  }
  allocation$drug_code <- .rp_generate_codes(allocation$group_id, code_prefix, code_width, code_random_digits, code_random_range, code_by_group)
  allocation$envelope_no <- paste0(envelope_no_prefix, sprintf(paste0("%0", as.integer(envelope_no_width), "d"), seq_len(nrow(allocation))))

  allocation <- merge(allocation, interventions, by = "group_id", sort = FALSE)
  allocation <- allocation[order(allocation$sequence_no), , drop = FALSE]
  rownames(allocation) <- NULL
  allocation$method <- method

  center_col <- if (!is.null(center_var) && center_var %in% names(allocation)) center_var else NULL
  strata_cols <- intersect(names(strata %||% list()), names(allocation))
  base_cols <- intersect(c("envelope_no", center_col, "stratum_id", strata_cols, "random_no", "block_id", "block_size", "drug_code"), names(allocation))
  allocation_table <- allocation[, unique(c(base_cols, "group_id", "intervention_name", "blind_label", "allocation_label", "sequence_no", "method")), drop = FALSE]
  blinded_cols <- intersect(c("envelope_no", center_col, "stratum_id", strata_cols, "random_no", "block_id", "block_size", "drug_code", "blind_label"), names(allocation))
  blinded_table <- allocation[, unique(blinded_cols), drop = FALSE]
  unblinded_table <- allocation_table

  files <- list()
  .rp_write_csv_utf8(blinded_table, file.path(output_dir, "blinded_site_table.csv"))
  .rp_write_csv_utf8(unblinded_table, file.path(output_dir, "unblinded_master_table.csv"))
  files$blinded_site_table <- file.path(output_dir, "blinded_site_table.csv")
  files$unblinded_master_table <- file.path(output_dir, "unblinded_master_table.csv")

  if (isTRUE(generate_report)) {
    files$report <- file.path(output_dir, report_file)
    .rp_write_report(files$report, project_name, protocol_no, sponsor_name, seed, allocation_table, blinded_table, method, rng_kind, normal_kind, sample_kind)
  }
  if (isTRUE(generate_random_envelope)) {
    files$random_envelope <- file.path(output_dir, random_envelope_file)
    .rp_write_random_envelopes(files$random_envelope, project_name, protocol_no, sponsor_name, allocation, center_col, font_family)
  }
  if (isTRUE(generate_emergency_envelope)) {
    files$emergency_envelope <- file.path(output_dir, emergency_envelope_file)
    .rp_write_emergency_envelopes(files$emergency_envelope, project_name, protocol_no, sponsor_name, allocation, center_col, font_family)
  }
  if (isTRUE(generate_reproducibility)) {
    files$reproducibility_dir <- file.path(output_dir, "reproducibility")
    .rp_write_reproducibility_bundle(
      files$reproducibility_dir, project_name, protocol_no, sponsor_name, interventions,
      method, seed, requested_n_total, requested_n_per_group, requested_allocation_ratio, strata, stratum_n, center_var,
      block_sizes, block_size_probs, random_no_prefix, random_no_width, random_no_by_center,
      code_prefix, code_width, code_random_digits, code_random_range, code_by_group,
      envelope_no_prefix, envelope_no_width, language, rng_kind, normal_kind, sample_kind,
      allocation_table, standalone_reproducibility_code, rng_start
    )
  }

  table_hash <- .rp_table_hash(allocation_table)
  .rp_write_audit(output_dir, "randomization_generated", "Randomization output generated.", list(method = method, seed = seed, table_hash = table_hash))
  tracked_paths <- unlist(files, use.names = FALSE)
  tracked_paths <- tracked_paths[file.exists(tracked_paths)]
  hash_paths <- unique(unlist(lapply(tracked_paths, function(path) {
    if (dir.exists(path)) {
      list.files(path, full.names = TRUE, recursive = TRUE)
    } else {
      path
    }
  }), use.names = FALSE))
  hash_paths <- unique(c(hash_paths, file.path(output_dir, "audit_log.jsonl")))
  file_hashes <- .rp_file_sha256(hash_paths)
  writeLines(c(paste0("randomization_table: ", table_hash), paste(names(file_hashes), file_hashes, sep = ": ")), file.path(output_dir, "hashes_sha256.txt"))
  files$hashes <- file.path(output_dir, "hashes_sha256.txt")

  object <- list(
    design = list(
      project_name = project_name,
      protocol_no = protocol_no,
      sponsor_name = sponsor_name,
      method = method,
      seed = seed,
      n_total = n_total,
      n_per_group = n_per_group,
      allocation_ratio = allocation_ratio,
      strata = strata,
      center_var = center_var,
      rng_kind = rng_kind,
      normal_kind = normal_kind,
      sample_kind = sample_kind,
      output_dir = output_dir
    ),
    allocation_table = allocation_table,
    blinded_table = blinded_table,
    unblinded_table = unblinded_table,
    balance_summary = .rp_balance_summary(allocation, center_col),
    files = files,
    audit = list(table_hash = table_hash, file_hashes = file_hashes),
    reproducibility = list(rng_state = rng_start)
  )
  class(object) <- "rp_randomization"
  if (isTRUE(return_object)) object else invisible(object)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.rp_balance_summary <- function(allocation, center_col) {
  by_group <- as.data.frame(table(allocation$group_id), stringsAsFactors = FALSE)
  names(by_group) <- c("group_id", "n")
  out <- list(by_group = by_group)
  if (!is.null(center_col)) {
    out$by_center_group <- as.data.frame(table(allocation[[center_col]], allocation$group_id), stringsAsFactors = FALSE)
    names(out$by_center_group) <- c("center", "group_id", "n")
  }
  if ("stratum_id" %in% names(allocation)) {
    out$by_stratum_group <- as.data.frame(table(allocation$stratum_id, allocation$group_id), stringsAsFactors = FALSE)
    names(out$by_stratum_group) <- c("stratum_id", "group_id", "n")
  }
  out
}

#' @export
print.rp_randomization <- function(x, ...) {
  cat("randomPlatform randomization\n")
  cat("  Project: ", x$design$project_name, "\n", sep = "")
  cat("  Protocol: ", x$design$protocol_no, "\n", sep = "")
  cat("  Method: ", x$design$method, "\n", sep = "")
  cat("  N: ", nrow(x$allocation_table), "\n", sep = "")
  cat("  Table hash: ", x$audit$table_hash, "\n", sep = "")
  invisible(x)
}

#' @export
summary.rp_randomization <- function(object, ...) {
  object$balance_summary
}
