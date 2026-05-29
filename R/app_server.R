rp_build_call_args <- function(input) {
  interventions <- rp_collect_interventions(input)
  alloc <- rp_collect_allocation(input, interventions)
  strata <- rp_collect_strata(input)
  method <- input$method
  if (identical(method, "minimization")) {
    stop("\u8BF7\u5728\u201C\u6700\u5C0F\u5316\u6CD5\u52A8\u6001\u968F\u673A\u5316\u201D\u9875\u9762\u4F7F\u7528\u9010\u4F8B\u52A8\u6001\u968F\u673A\u5316\u3002", call. = FALSE)
  }
  center_var <- input$center_var
  if (is.null(center_var) || !nzchar(center_var) || identical(center_var, "<none>")) center_var <- NULL
  list(
    project_name = input$project_name,
    protocol_no = input$protocol_no,
    sponsor_name = input$sponsor_name,
    interventions = interventions,
    method = method,
    seed = as.integer(input$seed),
    n_total = alloc$n_total,
    n_per_group = alloc$n_per_group,
    allocation_ratio = alloc$allocation_ratio,
    strata = strata,
    center_var = center_var,
    block_sizes = rp_parse_numeric_csv(input$block_sizes, integer = TRUE),
    block_size_probs = rp_parse_numeric_csv(input$block_size_probs),
    random_no_prefix = input$random_no_prefix,
    random_no_width = as.integer(input$random_no_width),
    random_no_by_center = isTRUE(input$random_no_by_center),
    code_prefix = input$code_prefix,
    code_width = as.integer(input$code_width),
    code_random_digits = isTRUE(input$code_random_digits),
    code_random_range = c(as.integer(input$code_range_min), as.integer(input$code_range_max)),
    code_by_group = isTRUE(input$code_by_group),
    envelope_no_prefix = input$envelope_no_prefix,
    envelope_no_width = as.integer(input$envelope_no_width),
    generate_random_envelope = isTRUE(input$generate_random_envelope),
    random_envelope_file = input$random_envelope_file,
    generate_emergency_envelope = isTRUE(input$generate_emergency_envelope),
    emergency_envelope_file = input$emergency_envelope_file,
    generate_report = isTRUE(input$generate_report),
    report_file = input$report_file,
    generate_reproducibility = isTRUE(input$generate_reproducibility),
    standalone_reproducibility_code = isTRUE(input$standalone_reproducibility_code),
    output_dir = input$output_dir,
    language = input$language,
    encrypt_sensitive_outputs = isTRUE(input$encrypt_sensitive_outputs),
    password = input$password
  )
}

rp_group_n_from_ratio <- function(n_total, ratio) {
  n_total <- as.integer(n_total)
  ratio_names <- names(ratio)
  ratio <- as.numeric(ratio)
  names(ratio) <- ratio_names
  if (is.na(n_total) || n_total < 0 || any(is.na(ratio)) || any(ratio < 0) || sum(ratio) <= 0) {
    return(stats::setNames(rep(0L, length(ratio)), names(ratio)))
  }
  raw_n <- n_total * ratio / sum(ratio)
  base_n <- floor(raw_n)
  remainder <- n_total - sum(base_n)
  if (remainder > 0) {
    order_add <- order(raw_n - base_n, decreasing = TRUE)
    base_n[order_add[seq_len(remainder)]] <- base_n[order_add[seq_len(remainder)]] + 1L
  }
  stats::setNames(as.integer(base_n), names(ratio))
}

rp_app_pages <- function() {
  c(
    project = "\u9879\u76EE\u4FE1\u606F",
    interventions = "\u5E72\u9884\u7EC4\u8BBE\u7F6E",
    randomization = "\u968F\u673A\u5316\u8BBE\u8BA1",
    outputs = "\u8F93\u51FA\u8BBE\u7F6E",
    results = "\u751F\u6210\u4E0E\u9884\u89C8",
    downloads = "\u6587\u4EF6\u4E0B\u8F7D",
    audit = "\u5BA1\u8BA1\u4E0E\u590D\u73B0"
  )
}

rp_copy_download <- function(file_path, file) {
  if (is.null(file_path) || !file.exists(file_path)) stop("Requested file has not been generated.", call. = FALSE)
  file.copy(file_path, file, overwrite = TRUE)
}

rp_zip_paths <- function(paths, zip_file) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) stop("No generated files are available to download.", call. = FALSE)
  old <- setwd(dirname(paths[1]))
  on.exit(setwd(old), add = TRUE)
  zip::zipr(zipfile = zip_file, files = basename(paths))
}

rp_directory_roots <- function() {
  roots <- list()
  add_root <- function(label, path) {
    if (!is.null(path) && length(path) == 1 && nzchar(path) && dir.exists(path)) {
      roots[[label]] <<- normalizePath(path, winslash = "/", mustWork = TRUE)
    }
  }

  home <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
  add_root("Home", home)
  add_root("Desktop", file.path(home, "Desktop"))
  add_root("Documents", file.path(home, "Documents"))
  add_root("Downloads", file.path(home, "Downloads"))
  add_root("Workspace", getwd())

  if (identical(.Platform$OS.type, "windows")) {
    drives <- paste0(LETTERS, ":/")
    for (drive in drives[dir.exists(drives)]) {
      add_root(paste0("Drive ", sub("/$", "", drive)), drive)
    }
  } else {
    add_root("Root /", "/")
  }

  unlist(roots, use.names = TRUE)
}

rp_app_server <- function(input, output, session) {
  result <- shiny::reactiveVal(NULL)
  last_error <- shiny::reactiveVal(NULL)
  min_session <- shiny::reactiveVal(NULL)
  min_last <- shiny::reactiveVal(NULL)
  syncing_sample_size <- FALSE

  roots <- rp_directory_roots()
  shinyFiles::shinyDirChoose(input, "choose_output_dir", roots = roots, session = session)

  shiny::observeEvent(input$choose_output_dir, {
    chosen <- shinyFiles::parseDirPath(roots, input$choose_output_dir)
    if (length(chosen) == 1 && nzchar(chosen)) {
      shiny::updateTextInput(session, "output_dir", value = normalizePath(chosen, winslash = "/", mustWork = TRUE))
    }
  })

  output$intervention_rows <- shiny::renderUI({
    rp_intervention_rows_ui(as.integer(input$group_count %||% 2), input$sample_input_mode %||% "ratio")
  })

  output$sample_size_hint <- shiny::renderUI({
    if (identical(input$sample_input_mode, "n_per_group")) {
      shiny::tags$div(class = "rp-muted rp-inline-note", "\u5F53\u524D\u6A21\u5F0F\u4E0B\uFF0C\u603B\u6837\u672C\u91CF\u5C06\u6309\u5404\u7EC4\u6837\u672C\u91CF\u4E4B\u548C\u81EA\u52A8\u66F4\u65B0\u3002")
    } else {
      shiny::tags$div(class = "rp-muted rp-inline-note", "\u5F53\u524D\u6A21\u5F0F\u4E0B\uFF0C\u5404\u7EC4\u6837\u672C\u91CF\u6309\u603B\u6837\u672C\u91CF\u548C\u5206\u914D\u6BD4\u4F8B\u81EA\u52A8\u8BA1\u7B97\uFF08\u6700\u5927\u4F59\u6570\u6CD5\uFF09\u3002")
    }
  })

  shiny::observe({
    if (isTRUE(syncing_sample_size)) return()
    shiny::req(input$group_count)
    n <- as.integer(input$group_count)
    if (n < 1 || !identical(input$sample_input_mode, "ratio")) return()
    ratio <- vapply(seq_len(n), function(i) input[[paste0("ratio_", i)]] %||% 1, numeric(1))
    group_id <- vapply(seq_len(n), function(i) input[[paste0("group_id_", i)]] %||% LETTERS[i], character(1))
    names(ratio) <- group_id
    n_group <- rp_group_n_from_ratio(input$n_total %||% 0, ratio)
    syncing_sample_size <<- TRUE
    on.exit(syncing_sample_size <<- FALSE, add = TRUE)
    for (i in seq_len(n)) {
      shiny::updateNumericInput(session, paste0("n_group_", i), value = unname(n_group[i]))
    }
  })

  shiny::observe({
    if (isTRUE(syncing_sample_size)) return()
    shiny::req(input$group_count)
    n <- as.integer(input$group_count)
    if (n < 1 || !identical(input$sample_input_mode, "n_per_group")) return()
    n_group <- vapply(seq_len(n), function(i) input[[paste0("n_group_", i)]] %||% 0, numeric(1))
    total <- sum(as.integer(n_group), na.rm = TRUE)
    syncing_sample_size <<- TRUE
    on.exit(syncing_sample_size <<- FALSE, add = TRUE)
    shiny::updateNumericInput(session, "n_total", value = total)
  })

  output$strata_rows <- shiny::renderUI({
    rp_strata_rows_ui(as.integer(input$strata_count %||% 0))
  })

  output$center_var_ui <- shiny::renderUI({
    strata <- rp_collect_strata(input)
    choices <- c("<none>", names(strata))
    shiny::selectInput("center_var", "\u5206\u5C42\u53D8\u91CF", choices = choices, selected = if ("center" %in% choices) "center" else "<none>")
  })

  output$strata_preview <- shiny::renderUI({
    strata <- rp_collect_strata(input)
    if (is.null(strata)) {
      return(rp_info_grid(
        rp_stat_line("\u5206\u5C42\u7EC4\u5408\u6570", "0"),
        rp_stat_line("\u72B6\u6001", "\u672A\u542F\u7528\u5206\u5C42")
      ))
    }
    n_combo <- prod(lengths(strata))
    n_total <- if (identical(input$sample_input_mode, "ratio")) as.integer(input$n_total) else {
      sum(vapply(seq_len(input$group_count), function(i) input[[paste0("n_group_", i)]], numeric(1)))
    }
    per <- if (n_combo > 0) floor(n_total / n_combo) else NA_integer_
    risk <- if (n_combo > max(1, n_total / 2)) "\u98CE\u9669\u63D0\u793A\uFF1A\u5206\u5C42\u7EC4\u5408\u6570\u76F8\u5BF9\u6837\u672C\u91CF\u8FC7\u591A\uFF0C\u5EFA\u8BAE\u51CF\u5C11\u5206\u5C42\u56E0\u7D20\u6216\u8C03\u6574\u968F\u673A\u5316\u8BBE\u8BA1\u3002" else "\u5206\u5C42\u7EC4\u5408\u6570\u5904\u4E8E\u53EF\u63A5\u53D7\u8303\u56F4\u3002"
    rp_info_grid(
      rp_stat_line("\u5206\u5C42\u7EC4\u5408\u6570", n_combo),
      rp_stat_line("\u6BCF\u5C42\u9884\u8BA1\u6837\u672C\u91CF\u7EA6", per),
      rp_stat_line("\u8BC4\u4F30", risk)
    )
  })

  output$parameter_summary <- shiny::renderUI({
    args <- tryCatch(rp_build_call_args(input), error = function(e) e)
    if (inherits(args, "error")) {
      return(shiny::tags$div(class = "rp-muted", paste("\u53C2\u6570\u5C1A\u672A\u5B8C\u6574\uFF1A", args$message)))
    }
    rp_info_grid(
      rp_stat_line("\u9879\u76EE\u540D\u79F0", args$project_name),
      rp_stat_line("\u65B9\u6848\u7F16\u53F7", args$protocol_no),
      rp_stat_line("\u7533\u529E\u5355\u4F4D", args$sponsor_name),
      rp_stat_line("\u968F\u673A\u5316\u65B9\u6CD5", args$method),
      rp_stat_line("\u968F\u673A\u79CD\u5B50", args$seed),
      rp_stat_line("\u5206\u7EC4\u6570", nrow(args$interventions)),
      rp_stat_line("\u603B\u6837\u672C\u91CF", args$n_total %||% sum(args$n_per_group)),
      rp_stat_line("\u5206\u5C42\u56E0\u7D20", paste(names(args$strata %||% list()), collapse = ", ")),
      rp_stat_line("\u751F\u6210\u968F\u673A\u4FE1\u5C01", if (isTRUE(args$generate_random_envelope)) "\u662F" else "\u5426"),
      rp_stat_line("\u751F\u6210\u5E94\u6025\u7834\u76F2\u4FE1\u5C01", if (isTRUE(args$generate_emergency_envelope)) "\u662F" else "\u5426")
    )
  })

  shiny::observeEvent(input$generate, {
    last_error(NULL)
    shiny::withProgress(message = "\u6B63\u5728\u751F\u6210\u968F\u673A\u5316\u7ED3\u679C", value = 0, {
      shiny::incProgress(0.2, detail = "\u6821\u9A8C\u53C2\u6570")
      args <- tryCatch(rp_build_call_args(input), error = function(e) e)
      if (inherits(args, "error")) {
        last_error(args$message)
        return(NULL)
      }
      shiny::incProgress(0.4, detail = "\u6267\u884C\u968F\u673A\u5316")
      res <- tryCatch(do.call(rp_randomize, args), error = function(e) e)
      if (inherits(res, "error")) {
        last_error(res$message)
        return(NULL)
      }
      shiny::incProgress(0.3, detail = "\u6574\u7406\u8F93\u51FA")
      result(res)
      shiny::showModal(shiny::modalDialog(
        title = "\u968F\u673A\u5316\u5DF2\u5B8C\u6210",
        "\u968F\u673A\u5316\u7ED3\u679C\u53CA\u8F93\u51FA\u6587\u4EF6\u5DF2\u751F\u6210\u3002\u8BF7\u5728\u201C\u751F\u6210\u4E0E\u9884\u89C8\u201D\u9875\u9762\u6838\u5BF9\u7ED3\u679C\uFF0C\u5E76\u5728\u201C\u6587\u4EF6\u4E0B\u8F7D\u201D\u9875\u9762\u4E0B\u8F7D\u5F52\u6863\u6587\u4EF6\u3002",
        easyClose = TRUE,
        footer = shiny::modalButton("\u786E\u5B9A")
      ))
      session$sendCustomMessage("rp-show-page", "results")
      shiny::incProgress(0.1)
    })
  })

  output$generation_status <- shiny::renderUI({
    if (!is.null(last_error())) {
      return(shiny::tags$div(class = "rp-error", paste("\u751F\u6210\u5931\u8D25\uFF1A", last_error())))
    }
    if (is.null(result())) return(shiny::tags$div(class = "rp-muted", "\u5C1A\u672A\u751F\u6210\u968F\u673A\u5316\u7ED3\u679C\u3002"))
    shiny::tags$div(class = "rp-ok", "\u968F\u673A\u5316\u7ED3\u679C\u5DF2\u751F\u6210\u3002")
  })

  output$blinded_table <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$blinded_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$unblinded_table <- DT::renderDT({
    shiny::req(result(), input$show_unblinded)
    DT::datatable(result()$unblinded_table, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$balance_table <- DT::renderDT({
    shiny::req(result())
    DT::datatable(result()$balance_summary$by_group, options = list(dom = "t"))
  })

  output$files_table <- DT::renderDT({
    shiny::req(result())
    files <- result()$files
    DT::datatable(data.frame(name = names(files), path = unlist(files), stringsAsFactors = FALSE), options = list(pageLength = 20, scrollX = TRUE))
  })

  output$hash_table <- DT::renderDT({
    shiny::req(result())
    hashes <- result()$audit$file_hashes
    DT::datatable(data.frame(file = names(hashes), sha256 = unname(hashes), stringsAsFactors = FALSE), options = list(pageLength = 20, scrollX = TRUE))
  })

  output$runtime_info <- shiny::renderUI({
    res <- result()
    rp_info_grid(
      rp_stat_line("R \u7248\u672C", R.version.string),
      rp_stat_line("randomPlatform \u7248\u672C", "V1.0"),
      rp_stat_line("RNG \u8BBE\u7F6E", paste(RNGkind(), collapse = " / ")),
      rp_stat_line("\u968F\u673A\u79CD\u5B50", input$seed %||% if (is.null(res)) "" else res$design$seed),
      rp_stat_line("\u751F\u6210\u65F6\u95F4", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    )
  })

  output$audit_preview <- shiny::renderText({
    shiny::req(result())
    path <- file.path(result()$design$output_dir %||% dirname(result()$files$blinded_site_table), "audit_log.jsonl")
    if (!file.exists(path)) return("\u6682\u65E0\u5BA1\u8BA1\u65E5\u5FD7\u3002")
    paste(utils::tail(readLines(path, warn = FALSE), 20), collapse = "\n")
  })

  output$verify_result <- shiny::renderText("")

  shiny::observeEvent(input$verify_repro, {
    shiny::req(result())
    repro_dir <- result()$files$reproducibility_dir
    script_path <- file.path(repro_dir, "reproduce_randomization.R")
    if (is.null(repro_dir) || !dir.exists(repro_dir) || !file.exists(script_path)) {
      output$verify_result <- shiny::renderText("当前结果未生成复现包，无法执行复现校验。")
      return()
    }

    check <- tryCatch({
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(repro_dir)
      repro_env <- new.env(parent = baseenv())
      sys.source(script_path, envir = repro_env)
      if (!exists("res", envir = repro_env, inherits = FALSE)) {
        stop("复现脚本未生成对象 res。", call. = FALSE)
      }
      repro_res <- get("res", envir = repro_env, inherits = FALSE)
      rp_verify_reproducibility(result()$allocation_table, repro_res$allocation_table)
    }, error = function(e) {
      list(message = paste0("复现校验失败：", e$message))
    })

    output$verify_result <- shiny::renderText(check$message)
  })

  download_one <- function(name) {
    shiny::downloadHandler(
      filename = function() basename(result()$files[[name]] %||% paste0(name, ".dat")),
      content = function(file) {
        shiny::req(result())
        if (name %in% c("unblinded_master_table", "emergency_envelope", "reproducibility_dir") && !isTRUE(input$confirm_sensitive_download)) {
          stop("\u8BF7\u5148\u786E\u8BA4\u5F53\u524D\u64CD\u4F5C\u8005\u6709\u6743\u4E0B\u8F7D\u654F\u611F\u6587\u4EF6\u3002", call. = FALSE)
        }
        if (identical(name, "reproducibility_dir")) {
          paths <- list.files(result()$files$reproducibility_dir, full.names = TRUE, recursive = TRUE)
          zip::zipr(file, paths)
        } else {
          rp_copy_download(result()$files[[name]], file)
        }
      }
    )
  }
  output$download_report <- download_one("report")
  output$download_blinded <- download_one("blinded_site_table")
  output$download_unblinded <- download_one("unblinded_master_table")
  output$download_random_pdf <- download_one("random_envelope")
  output$download_emergency_pdf <- download_one("emergency_envelope")
  output$download_repro <- download_one("reproducibility_dir")
  output$download_audit <- shiny::downloadHandler(
    filename = "audit_log.jsonl",
    content = function(file) {
      shiny::req(result())
      rp_copy_download(file.path(dirname(result()$files$blinded_site_table), "audit_log.jsonl"), file)
    }
  )
  output$download_hashes <- download_one("hashes")
  output$download_zip <- shiny::downloadHandler(
    filename = function() paste0("randomPlatform_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      shiny::req(result())
      if (!isTRUE(input$confirm_sensitive_download)) stop("\u8BF7\u5148\u786E\u8BA4\u5F53\u524D\u64CD\u4F5C\u8005\u6709\u6743\u4E0B\u8F7D\u654F\u611F\u6587\u4EF6\u3002", call. = FALSE)
      root <- dirname(result()$files$blinded_site_table)
      paths <- list.files(root, full.names = TRUE, recursive = TRUE)
      zip::zipr(file, paths)
    }
  )

  min_factors <- shiny::reactive({
    x <- trimws(strsplit(input$min_factors %||% "", ",", fixed = TRUE)[[1]])
    x[nzchar(x)][seq_len(min(6, length(x[nzchar(x)])))]
  })

  output$min_covariate_inputs <- shiny::renderUI({
    f <- min_factors()
    shiny::tagList(lapply(f, function(nm) shiny::textInput(paste0("min_cov_", nm), paste0(nm, " \u6C34\u5E73"), value = "")))
  })

  shiny::observeEvent(input$min_start, {
    interventions <- rp_collect_interventions(input)
    alloc <- rp_collect_allocation(input, interventions)
    weights <- rp_parse_numeric_csv(input$min_weights)
    names(weights) <- min_factors()
    sess <- tryCatch(rp_minimization_session(
      project_name = input$project_name,
      protocol_no = input$protocol_no,
      sponsor_name = input$sponsor_name,
      interventions = interventions,
      allocation_ratio = alloc$allocation_ratio %||% stats::setNames(rep(1, nrow(interventions)), interventions$group_id),
      factors = min_factors(),
      weights = weights,
      prob_best = input$min_prob_best,
      seed = input$seed,
      state_file = input$min_state_file,
      output_dir = input$output_dir,
      generate_random_envelope = input$generate_random_envelope,
      generate_emergency_envelope = input$generate_emergency_envelope
    ), error = function(e) e)
    if (inherits(sess, "error")) {
      shiny::showNotification(sess$message, type = "error")
    } else {
      min_session(sess)
      shiny::showNotification("\u6700\u5C0F\u5316\u6CD5\u4F1A\u8BDD\u5DF2\u542F\u52A8\u3002", type = "message")
    }
  })

  shiny::observeEvent(input$min_assign, {
    shiny::req(min_session())
    cov <- lapply(min_factors(), function(nm) input[[paste0("min_cov_", nm)]])
    names(cov) <- min_factors()
    out <- tryCatch(rp_assign_next(min_session(), input$min_subject_id, cov, input$min_operator, input$min_note), error = function(e) e)
    if (inherits(out, "error")) {
      shiny::showNotification(out$message, type = "error")
    } else {
      min_session(out$session)
      min_last(out)
    }
  })

  output$min_blinded <- DT::renderDT({
    shiny::req(min_last())
    DT::datatable(min_last()$blinded, options = list(dom = "t", scrollX = TRUE))
  })
  output$min_unblinded <- DT::renderDT({
    shiny::req(min_last(), input$min_show_unblinded)
    DT::datatable(min_last()$unblinded, options = list(dom = "t", scrollX = TRUE))
  })
  output$min_assignments <- DT::renderDT({
    shiny::req(min_session())
    DT::datatable(min_session()$assignments, options = list(pageLength = 10, scrollX = TRUE))
  })

  always_live_outputs <- c(
    "intervention_rows",
    "sample_size_hint",
    "center_var_ui",
    "strata_rows",
    "strata_preview",
    "parameter_summary",
    "generation_status",
    "blinded_table",
    "unblinded_table",
    "balance_table",
    "files_table",
    "hash_table",
    "min_covariate_inputs",
    "min_blinded",
    "min_unblinded",
    "min_assignments",
    "runtime_info",
    "verify_result",
    "audit_preview"
  )
  for (id in always_live_outputs) {
    shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  }
}
