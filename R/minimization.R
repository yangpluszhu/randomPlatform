.rp_min_save <- function(session) {
  saveRDS(session, session$state_file)
  invisible(session)
}

.rp_min_load <- function(session) {
  if (is.character(session) && length(session) == 1L) return(readRDS(session))
  session
}

#' Initialize and save a minimization session.
#'
#' @export
#' @param project_name Character scalar naming the project. Required; stored on
#'   the returned session as metadata.
#' @param protocol_no Character scalar with the protocol number. Required;
#'   stored on the returned session as metadata.
#' @param sponsor_name Character scalar naming the sponsor. Required; stored on
#'   the returned session as metadata.
#' @param interventions Intervention definitions in the same normalized forms
#'   accepted by `rp_randomize()`: either a named character vector, where names
#'   are `group_id` values and values are `intervention_name` values, or a data
#'   frame containing `group_id` and `intervention_name` and optionally
#'   `blind_label` and `allocation_label`. The number of groups must be between
#'   2 and 5, `group_id` values must be unique, and `group_id` and
#'   `intervention_name` values must not be empty.
#' @param allocation_ratio Optional named numeric vector of relative allocation
#'   weights by `group_id`. When `NULL`, equal weights are used across all
#'   intervention groups. If supplied, names must be unique and match
#'   `interventions$group_id` exactly; the values are reordered to that group
#'   order and must be positive because they are used as sampling weights.
#' @param factors Character vector that must resolve to 1 to 6 unique
#'   minimization factors after coercion to character and duplicate removal with
#'   `unique()`. The resulting factor names are stored in this order, and the
#'   same names must be present in the `covariates` supplied to
#'   `rp_assign_next()`.
#' @param weights Optional numeric vector of factor weights. When `NULL`, each
#'   factor gets weight 1. Unnamed vectors must have the same length as
#'   `factors` and are taken positionally. Named vectors are matched by factor
#'   name, reordered to the stored factor order, must cover exactly the resolved
#'   `factors`, and must use unique names. All weights must be positive.
#' @param prob_best Numeric scalar in `(0, 1]` controlling how much probability
#'   mass is given to the best-scoring groups during assignment. Lower values
#'   spread more probability to non-best groups; `1` always chooses among the
#'   best-scoring groups only.
#' @param seed Integer seed used to initialize the RNG for the session.
#'   Required; the resulting `.Random.seed` is stored in the session and reused
#'   by `rp_assign_next()`.
#' @param state_file File name or relative path for the saved session RDS file.
#'   Default: `"minimization_session.rds"`. The path is resolved under
#'   `output_dir` and the session is written there immediately.
#' @param output_dir Output directory for the session file, assignment CSV, and
#'   audit log. Default: `"."`. The directory is created if needed and must be
#'   writable.
#' @param generate_random_envelope Logical scalar. Default: `FALSE`. Stored on
#'   the session object for downstream consumers; it does not change the
#'   minimization algorithm in this function.
#' @param generate_emergency_envelope Logical scalar. Default: `FALSE`. Stored
#'   on the session object for downstream consumers; it does not change the
#'   minimization algorithm in this function.
#' @return An object of class `rp_minimization_session`, returned invisibly and
#'   saved immediately to `state_file`. It is a state container with these
#'   fields:
#'   \describe{
#'     \item{project_name}{Project name metadata.}
#'     \item{protocol_no}{Protocol number metadata.}
#'     \item{sponsor_name}{Sponsor name metadata.}
#'     \item{interventions}{Normalized intervention table used for assignment.}
#'     \item{allocation_ratio}{Relative allocation weights aligned to
#'     `interventions$group_id`.}
#'     \item{factors}{Character vector of minimization factor names.}
#'     \item{weights}{Positive factor weights aligned to `factors`.}
#'     \item{prob_best}{Best-group sampling probability used by
#'     `rp_assign_next()`.}
#'     \item{seed}{Seed used to initialize the RNG.}
#'     \item{state_file}{Normalized path to the session RDS file.}
#'     \item{output_dir}{Validated output directory.}
#'     \item{generate_random_envelope}{Stored logical flag for downstream use.}
#'     \item{generate_emergency_envelope}{Stored logical flag for downstream use.}
#'     \item{assignments}{Data frame of assignments made so far. Initially
#'     empty.}
#'     \item{rng_state}{Saved `.Random.seed` value for reproducible
#'     continuation.}
#'     \item{voided}{Character vector of voided assignment identifiers.}
#'   }
#' @details `rp_minimization_session()` initializes a stateful minimization
#'   session, normalizes and stores the trial metadata, and saves the session to
#'   the RDS file named by `state_file` under `output_dir`. The `interventions`
#'   argument uses the same normalization path as `rp_randomize()`. The saved
#'   object is intended to be passed to `rp_assign_next()` either directly or by
#'   file path so that the assignment sequence can resume from the stored RNG
#'   state.
#' @seealso [rp_assign_next()] for assigning participants from a saved
#'   session, and [rp_randomize()] for batch randomization workflows that share
#'   the same intervention normalization rules.
#' @examples
#' tmp <- file.path(tempdir(), "rp-min-session")
#' dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
#' session <- rp_minimization_session(
#'   project_name = "Demo study",
#'   protocol_no = "RP-001",
#'   sponsor_name = "Acme Pharma",
#'   interventions = c(Control = "Control", Treatment = "Treatment"),
#'   factors = c("center", "sex"),
#'   weights = c(center = 1, sex = 1),
#'   seed = 123,
#'   output_dir = tmp
#' )
#' session$state_file
rp_minimization_session <- function(
  project_name,
  protocol_no,
  sponsor_name,
  interventions,
  allocation_ratio = NULL,
  factors,
  weights = NULL,
  prob_best = 0.8,
  seed,
  state_file = "minimization_session.rds",
  output_dir = ".",
  generate_random_envelope = FALSE,
  generate_emergency_envelope = FALSE
) {
  .rp_required(project_name, "project_name")
  .rp_required(protocol_no, "protocol_no")
  .rp_required(sponsor_name, "sponsor_name")
  .rp_required(seed, "seed")
  interventions <- .rp_normalize_interventions(interventions)
  factors <- unique(as.character(factors))
  if (length(factors) < 1 || length(factors) > 6) stop("factors must contain 1 to 6 balancing factors.", call. = FALSE)
  if (is.null(weights)) {
    weights <- stats::setNames(rep(1, length(factors)), factors)
  } else {
    weight_names <- names(weights)
    weight_values <- as.numeric(weights)
    if (is.null(weight_names)) {
      if (length(weight_values) != length(factors)) stop("weights must have the same length as factors when unnamed.", call. = FALSE)
      weights <- stats::setNames(weight_values, factors)
    } else {
      if (anyDuplicated(weight_names)) stop("weights names must be unique.", call. = FALSE)
      if (!setequal(weight_names, factors)) stop("weights names must match factors.", call. = FALSE)
      weights <- stats::setNames(weight_values, weight_names)
      weights <- weights[factors]
    }
  }
  if (any(is.na(weights)) || any(weights <= 0)) stop("weights must be positive and aligned to factors.", call. = FALSE)
  if (prob_best <= 0 || prob_best > 1) stop("prob_best must be in (0, 1].", call. = FALSE)
  if (is.null(allocation_ratio)) {
    allocation_ratio <- stats::setNames(rep(1, nrow(interventions)), interventions$group_id)
  } else {
    allocation_ratio_names <- names(allocation_ratio)
    allocation_ratio <- as.numeric(allocation_ratio)
    if (is.null(allocation_ratio_names)) stop("allocation_ratio must be named by intervention group_id values.", call. = FALSE)
    if (anyDuplicated(allocation_ratio_names)) stop("allocation_ratio names must be unique.", call. = FALSE)
    if (!setequal(allocation_ratio_names, interventions$group_id)) stop("allocation_ratio names must match interventions$group_id.", call. = FALSE)
    allocation_ratio <- stats::setNames(allocation_ratio, allocation_ratio_names)[interventions$group_id]
    if (any(is.na(allocation_ratio)) || any(allocation_ratio <= 0)) stop("allocation_ratio values must be positive.", call. = FALSE)
  }
  output_dir <- .rp_validate_output_dir(output_dir)
  state_file <- normalizePath(file.path(output_dir, state_file), winslash = "/", mustWork = FALSE)

  old_rng <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_rng)), add = TRUE)
  .rp_set_rng(as.integer(seed), "Mersenne-Twister", "Inversion", "Rejection")
  session <- list(
    project_name = project_name,
    protocol_no = protocol_no,
    sponsor_name = sponsor_name,
    interventions = interventions,
    allocation_ratio = allocation_ratio,
    factors = factors,
    weights = weights,
    prob_best = prob_best,
    seed = seed,
    state_file = state_file,
    output_dir = output_dir,
    generate_random_envelope = generate_random_envelope,
    generate_emergency_envelope = generate_emergency_envelope,
    assignments = data.frame(),
    rng_state = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    voided = character()
  )
  class(session) <- "rp_minimization_session"
  .rp_min_save(session)
}

.rp_min_scores <- function(session, covariates) {
  groups <- session$interventions$group_id
  assigned <- session$assignments
  scores <- stats::setNames(rep(0, length(groups)), groups)
  for (g in groups) {
    candidate_row <- assigned[0, , drop = FALSE]
    candidate_row[1, ] <- NA
    candidate_row$group_id[1] <- g
    for (f in session$factors) candidate_row[[f]][1] <- as.character(covariates[[f]])
    tmp <- rbind(assigned, candidate_row)
    score <- 0
    for (f in session$factors) {
      level_rows <- tmp[[f]] == covariates[[f]]
      counts <- table(factor(tmp$group_id[level_rows], levels = groups))
      score <- score + session$weights[[f]] * (max(counts) - min(counts))
    }
    total_counts <- table(factor(tmp$group_id, levels = groups))
    target <- sum(total_counts) * session$allocation_ratio / sum(session$allocation_ratio)
    score <- score + sum(abs(as.numeric(total_counts) - target))
    scores[[g]] <- score
  }
  scores
}

#' Assign the next participant in a minimization session.
#'
#' @export
#' @param session Either an `rp_minimization_session` object returned by
#'   `rp_minimization_session()` or a single character path to a saved session
#'   RDS file. If a path is supplied, the session is loaded from disk before the
#'   assignment is computed.
#' @param subject_id Character scalar identifying the participant. Required;
#'   must be unique among existing `session$assignments$subject_id` values.
#' @param covariates Named list, named data frame row, or similar named object
#'   containing the current participant's minimization factor values. All
#'   factors listed in `session$factors` must be present, and the values are
#'   reordered to `session$factors` before scoring. Any extra names are ignored.
#' @param operator Optional character scalar identifying the operator entering
#'   the assignment. Default: `NA_character_`. Stored in the unblinded row and
#'   written to the session assignment history.
#' @param note Optional character scalar with a free-text note. Default:
#'   `NA_character_`. Stored in the unblinded row and written to the session
#'   assignment history.
#' @return A list with the following members:
#'   \describe{
#'     \item{session}{Updated `rp_minimization_session` object after appending
#'     the new assignment and refreshing the saved RNG state.}
#'     \item{blinded}{A data frame row containing the blinded assignment view,
#'     including `subject_id`, `envelope_no`, `random_no`, `drug_code`,
#'     `blind_label`, and the factor columns.}
#'     \item{unblinded}{The full assignment row with group and intervention
#'     details, timestamps, operator, note, and factor values.}
#'     \item{scores}{Named numeric vector of minimization scores, one per
#'     intervention group, computed before the new assignment is sampled.}
#'     \item{probabilities}{Named numeric vector of sampling probabilities used
#'     to choose the assigned group.}
#'   }
#' @details `rp_assign_next()` accepts either an in-memory session object or a
#'   path to a saved session RDS file. It validates that `subject_id` has not
#'   already been assigned, requires covariates for every factor in
#'   `session$factors`, and reorders those covariates to the stored factor order
#'   before scoring. The function restores the saved RNG state, computes the
#'   minimization scores, samples the next group, appends the assignment to the
#'   session, saves the updated session back to `session$state_file`, writes
#'   `minimization_assignments.csv` in `session$output_dir`, and appends an audit
#'   log entry. The returned `blinded` and `unblinded` rows expose the same new
#'   assignment in blinded and full-detail forms. The `generate_random_envelope`
#'   and `generate_emergency_envelope` session fields are preserved on the saved
#'   session object but are not otherwise consumed by this function.
#' @seealso [rp_minimization_session()] for creating the saved session object
#'   used here, and [rp_randomize()] for the batch randomization API that shares
#'   the same intervention normalization rules.
#' @examples
#' tmp <- file.path(tempdir(), "rp-min-assign")
#' dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
#' session <- rp_minimization_session(
#'   project_name = "Demo study",
#'   protocol_no = "RP-001",
#'   sponsor_name = "Acme Pharma",
#'   interventions = c(Control = "Control", Treatment = "Treatment"),
#'   factors = c("center", "sex"),
#'   weights = c(center = 1, sex = 1),
#'   seed = 123,
#'   output_dir = tmp
#' )
#' result <- rp_assign_next(
#'   session = session,
#'   subject_id = "SUBJ-001",
#'   covariates = list(center = "A", sex = "F")
#' )
#' names(result)
#' file.exists(file.path(tmp, "minimization_assignments.csv"))
rp_assign_next <- function(
  session,
  subject_id,
  covariates,
  operator = NA_character_,
  note = NA_character_
) {
  session <- .rp_min_load(session)
  .rp_required(subject_id, "subject_id")
  if (subject_id %in% session$assignments$subject_id) stop("subject_id has already been assigned.", call. = FALSE)
  if (!all(session$factors %in% names(covariates))) stop("covariates must include all minimization factors.", call. = FALSE)
  covariates <- covariates[session$factors]
  .Random.seed <<- session$rng_state
  scores <- .rp_min_scores(session, covariates)
  best <- names(scores)[scores == min(scores)]
  groups <- session$interventions$group_id
  if (length(best) == length(groups)) {
    probs <- session$allocation_ratio / sum(session$allocation_ratio)
  } else {
    probs <- rep((1 - session$prob_best) / (length(groups) - length(best)), length(groups))
    names(probs) <- groups
    probs[best] <- session$prob_best / length(best)
  }
  group <- sample(groups, 1L, prob = probs)
  n <- nrow(session$assignments) + 1L
  random_no <- paste0("R", sprintf("%03d", n))
  drug_code <- paste0("MED-", sprintf("%04d", sample(seq.int(1000, 9999), 1L)))
  envelope_no <- paste0("No. ", sprintf("%03d", n))
  intervention_name <- session$interventions$intervention_name[match(group, session$interventions$group_id)]
  blind_label <- session$interventions$blind_label[match(group, session$interventions$group_id)]
  row <- data.frame(
    subject_id = subject_id,
    envelope_no = envelope_no,
    random_no = random_no,
    drug_code = drug_code,
    group_id = group,
    intervention_name = intervention_name,
    blind_label = blind_label,
    assigned_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    operator = operator,
    note = note,
    stringsAsFactors = FALSE
  )
  for (f in session$factors) row[[f]] <- as.character(covariates[[f]])
  session$assignments <- rbind(session$assignments, row)
  session$rng_state <- .Random.seed
  .rp_min_save(session)
  .rp_write_csv_utf8(session$assignments, file.path(session$output_dir, "minimization_assignments.csv"))
  .rp_write_audit(session$output_dir, "minimization_assignment", "Participant assigned by minimization.", list(subject_id = subject_id, group_id = group))
  list(
    session = session,
    blinded = row[, c("subject_id", "envelope_no", "random_no", "drug_code", "blind_label", session$factors), drop = FALSE],
    unblinded = row,
    scores = scores,
    probabilities = probs
  )
}
