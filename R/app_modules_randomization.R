rp_randomization_panel <- function() {
  shiny::tagList(
    rp_card(
      "\u968F\u673A\u5316\u8BBE\u8BA1",
      subtitle = "\u9009\u62E9\u968F\u673A\u5316\u65B9\u6CD5\uFF0C\u5E76\u56FA\u5B9A\u968F\u673A\u79CD\u5B50\u4EE5\u652F\u6301\u590D\u73B0\u3002",
      rp_grid(
        shiny::selectInput(
          "method",
          "\u968F\u673A\u5316\u65B9\u6CD5",
          choices = c("Simple randomization" = "simple", "Block randomization" = "block", "Stratified randomization" = "stratified", "Stratified block randomization" = "stratified_block")
        ),
        shiny::numericInput("seed", "\u968F\u673A\u79CD\u5B50\u6570", value = 20260528, min = 1, step = 1)
      )
    ),
    rp_card(
      "\u7F16\u53F7\u89C4\u5219",
      subtitle = "\u968F\u673A\u53F7\u3001\u4FE1\u5C01\u6D41\u6C34\u53F7\u4E0E\u836F\u7269/\u5E72\u9884\u7F16\u7801\u7684\u683C\u5F0F\u3002",
      rp_grid(
        shiny::textInput("random_no_prefix", "\u968F\u673A\u53F7\u524D\u7F00", value = "R"),
        shiny::numericInput("random_no_width", "\u968F\u673A\u53F7\u4F4D\u6570", value = 3, min = 1, max = 8),
        shiny::textInput("envelope_no_prefix", "\u4FE1\u5C01\u6D41\u6C34\u53F7\u524D\u7F00", value = "No. "),
        shiny::numericInput("envelope_no_width", "\u4FE1\u5C01\u6D41\u6C34\u53F7\u4F4D\u6570", value = 3, min = 1, max = 8),
        shiny::textInput("code_prefix", "\u836F\u7269/\u5E72\u9884\u7F16\u7801\u524D\u7F00", value = "MED"),
        shiny::numericInput("code_width", "\u7F16\u7801\u6570\u5B57\u4F4D\u6570", value = 4, min = 1, max = 10)
      ),
      rp_grid(
        shiny::checkboxInput("random_no_by_center", "\u6309\u4E2D\u5FC3\u751F\u6210\u968F\u673A\u53F7", value = FALSE),
        shiny::checkboxInput("code_random_digits", "\u4F7F\u7528\u968F\u673A\u6570\u5B57\u7F16\u7801", value = TRUE),
        shiny::checkboxInput("code_by_group", "\u7F16\u7801\u524D\u7F00\u7ED1\u5B9A\u7EC4\u522B\uFF08\u76F2\u6001\u8BBE\u8BA1\u8BF7\u52FF\u52FE\u9009\uFF01\uFF09", value = FALSE)
      )
    ),
    rp_card(
      "\u533A\u7EC4\u4E0E\u7F16\u7801\u8303\u56F4",
      subtitle = "\u591A\u4E2A\u6570\u503C\u8BF7\u7528\u82F1\u6587\u9017\u53F7\u5206\u9694\u3002",
      rp_grid(
        shiny::numericInput("code_range_min", "\u968F\u673A\u6570\u5B57\u6700\u5C0F\u503C", value = 1000, min = 0, step = 1),
        shiny::numericInput("code_range_max", "\u968F\u673A\u6570\u5B57\u6700\u5927\u503C", value = 9999, min = 1, step = 1),
        shiny::textInput("block_sizes", "\u533A\u7EC4\u5927\u5C0F", value = "4,6,8"),
        shiny::textInput("block_size_probs", "\u533A\u7EC4\u5927\u5C0F\u6982\u7387\uFF08\u53EF\u7A7A\uFF09", value = "")
      )
    ),
    rp_card(
      "\u5206\u5C42\u56E0\u7D20",
      subtitle = "\u6700\u591A 6 \u4E2A\u5206\u5C42\u56E0\u7D20\uFF1B\u5C42\u6570\u8FC7\u591A\u65F6\u754C\u9762\u4F1A\u63D0\u793A\u98CE\u9669\u3002",
      rp_grid(
        shiny::numericInput("strata_count", "\u5206\u5C42\u56E0\u7D20\u6570\u91CF", value = 0, min = 0, max = 6, step = 1),
        shiny::uiOutput("center_var_ui")
      ),
      shiny::uiOutput("strata_rows"),
      shiny::tags$div(class = "rp-info-box", shiny::uiOutput("strata_preview"))
    )
  )
}

rp_strata_rows_ui <- function(strata_count) {
  if (strata_count < 1) return(shiny::tags$em("\u672A\u8BBE\u7F6E\u5206\u5C42\u56E0\u7D20\u3002"))
  shiny::tagList(lapply(seq_len(strata_count), function(i) {
    shiny::tags$div(
      class = "rp-repeat-row rp-repeat-row-compact",
      shiny::tags$div(class = "rp-repeat-index", paste0("#", i)),
      shiny::textInput(paste0("strata_name_", i), paste0("\u5206\u5C42\u56E0\u7D20 ", i, " \u540D\u79F0"), value = if (i == 1) "center" else paste0("factor", i)),
      shiny::textInput(paste0("strata_levels_", i), "\u5206\u5C42\u6C34\u5E73\uFF08\u9017\u53F7\u5206\u9694\uFF09", value = if (i == 1) "C01,C02" else "\u4F4E,\u9AD8")
    )
  }))
}

rp_collect_strata <- function(input) {
  n <- as.integer(input$strata_count %||% 0)
  if (n < 1) return(NULL)
  out <- list()
  for (i in seq_len(n)) {
    nm <- trimws(input[[paste0("strata_name_", i)]])
    lv <- trimws(strsplit(input[[paste0("strata_levels_", i)]], ",", fixed = TRUE)[[1]])
    lv <- lv[nzchar(lv)]
    if (nzchar(nm) && length(lv) > 0) out[[nm]] <- lv
  }
  if (length(out) == 0) NULL else out
}

rp_parse_numeric_csv <- function(value, integer = FALSE) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) return(NULL)
  out <- as.numeric(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
  if (any(is.na(out))) stop("Comma-separated numeric fields must contain only numbers.", call. = FALSE)
  if (integer) out <- as.integer(out)
  out
}
