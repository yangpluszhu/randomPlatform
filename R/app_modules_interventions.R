rp_interventions_panel <- function() {
  shiny::tagList(
    rp_card(
      "\u5E72\u9884\u7EC4\u8BBE\u7F6E",
      subtitle = "\u652F\u6301 2-5 \u4E2A\u5E72\u9884\u7EC4\uFF0C\u53EF\u6309\u5206\u914D\u6BD4\u6216\u76F4\u63A5\u8F93\u5165\u5404\u7EC4\u6837\u672C\u91CF\u3002",
      rp_grid(
        shiny::numericInput("group_count", "\u5206\u7EC4\u6570", value = 2, min = 2, max = 5, step = 1),
        shiny::numericInput("n_total", "\u603B\u6837\u672C\u91CF", value = 20, min = 1, step = 1)
      ),
      shiny::radioButtons(
        "sample_input_mode",
        "\u6837\u672C\u91CF\u8F93\u5165\u65B9\u5F0F",
        choices = c("Total N + allocation ratio" = "ratio", "Group sample sizes" = "n_per_group"),
        selected = "ratio",
        inline = TRUE
      ),
      shiny::textInput("blind_label", "\u76F2\u6001\u6807\u7B7E\uFF08\u6240\u6709\u7EC4\u76F8\u540C\uFF09", value = "\u6CBB\u7597\u65B9\u6848"),
      shiny::uiOutput("intervention_rows"),
      shiny::uiOutput("sample_size_hint")
    ),
    shiny::tags$div(
      class = "rp-warning",
      shiny::strong("\u76F2\u6CD5\u63D0\u793A\uFF1A"),
      "\u82E5\u836F\u7269\u7F16\u7801\u524D\u7F00\u4E0E\u7EC4\u522B\u7ED1\u5B9A\uFF0C\u7814\u7A76\u73B0\u573A\u53EF\u80FD\u63A8\u6D4B\u5206\u7EC4\u3002\u53CC\u76F2\u7814\u7A76\u5EFA\u8BAE\u4F7F\u7528\u7EDF\u4E00\u7F16\u7801\u524D\u7F00\u3002"
    )
  )
}

rp_intervention_rows_ui <- function(group_count, sample_input_mode = "ratio") {
  defaults <- LETTERS[seq_len(group_count)]
  derived_n <- identical(sample_input_mode, "ratio")
  rows <- lapply(seq_len(group_count), function(i) {
    shiny::tags$div(
      class = paste("rp-repeat-row", if (derived_n) "rp-ratio-mode" else "rp-direct-n-mode"),
      shiny::tags$div(class = "rp-repeat-index", paste0("#", i)),
      shiny::textInput(paste0("group_id_", i), "\u7EC4\u522B\u4EE3\u7801", value = defaults[i]),
      shiny::textInput(paste0("intervention_name_", i), "\u5E72\u9884\u540D\u79F0", value = if (i == 1) "\u8BD5\u9A8C\u7EC4" else if (i == 2) "\u5BF9\u7167\u7EC4" else paste0("\u5E72\u9884\u7EC4", i)),
      shiny::tagAppendAttributes(
        shiny::numericInput(paste0("ratio_", i), "\u5206\u914D\u6BD4\u4F8B", value = 1, min = 0.01, step = 0.01),
        class = "rp-ratio-field"
      ),
      shiny::tagAppendAttributes(
        shiny::numericInput(paste0("n_group_", i), "\u7EC4\u6837\u672C\u91CF", value = 10, min = 0, step = 1),
        class = "rp-n-group-field"
      )
    )
  })
  readonly_script <- sprintf(
    "setTimeout(function() {
      var ratioMode = %s;
      document.querySelectorAll('#intervention_rows input[id^=\"ratio_\"]').forEach(function(el) {
        el.readOnly = !ratioMode;
        el.classList.toggle('rp-readonly-input', !ratioMode);
      });
      document.querySelectorAll('#intervention_rows input[id^=\"n_group_\"]').forEach(function(el) {
        el.readOnly = ratioMode;
        el.classList.toggle('rp-readonly-input', ratioMode);
      });
    }, 0);",
    if (derived_n) "true" else "false"
  )
  shiny::tagList(rows, shiny::tags$script(shiny::HTML(readonly_script)))
}

rp_collect_interventions <- function(input) {
  n <- as.integer(input$group_count)
  group_id <- vapply(seq_len(n), function(i) input[[paste0("group_id_", i)]] %||% LETTERS[i], character(1))
  intervention_name <- vapply(seq_len(n), function(i) input[[paste0("intervention_name_", i)]] %||% paste0("Group ", i), character(1))
  blind_label_value <- input$blind_label %||% "Treatment"
  blind_label <- rep(as.character(blind_label_value), n)
  data.frame(group_id = group_id, intervention_name = intervention_name, blind_label = blind_label, stringsAsFactors = FALSE)
}

rp_collect_allocation <- function(input, interventions) {
  n <- nrow(interventions)
  ratio <- vapply(seq_len(n), function(i) input[[paste0("ratio_", i)]] %||% 1, numeric(1))
  n_group <- vapply(seq_len(n), function(i) input[[paste0("n_group_", i)]] %||% 0, numeric(1))
  names(ratio) <- interventions$group_id
  names(n_group) <- interventions$group_id
  if (identical(input$sample_input_mode, "n_per_group")) {
    n_group <- stats::setNames(as.integer(n_group), names(n_group))
    list(n_total = NULL, allocation_ratio = NULL, n_per_group = n_group)
  } else {
    list(n_total = as.integer(input$n_total), allocation_ratio = ratio, n_per_group = NULL)
  }
}
