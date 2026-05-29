rp_minimization_panel <- function() {
  shiny::tagList(
    rp_card(
      "\u6700\u5C0F\u5316\u6CD5\u52A8\u6001\u968F\u673A\u5316",
      subtitle = "\u7528\u4E8E\u9010\u4F8B\u5165\u7EC4\u7684\u52A8\u6001\u968F\u673A\u5206\u914D\uFF0C\u4F1A\u8BDD\u72B6\u6001\u4F1A\u4FDD\u5B58\u5230\u672C\u5730\u6587\u4EF6\u3002",
      rp_grid(
        shiny::textInput("min_factors", "\u5E73\u8861\u56E0\u7D20\uFF08\u9017\u53F7\u5206\u9694\uFF0C\u6700\u591A6\u4E2A\uFF09", value = "center,sex"),
        shiny::textInput("min_weights", "\u56E0\u7D20\u6743\u91CD\uFF08\u9017\u53F7\u5206\u9694\uFF09", value = "1,1"),
        shiny::numericInput("min_prob_best", "prob_best", value = 0.8, min = 0.01, max = 1, step = 0.01),
        shiny::textInput("min_state_file", "\u72B6\u6001\u6587\u4EF6\u540D", value = "minimization_session.rds")
      ),
      shiny::tags$div(class = "rp-action-row", shiny::actionButton("min_start", "\u542F\u52A8/\u91CD\u7F6E\u4F1A\u8BDD", class = "btn-primary"))
    ),
    rp_card(
      "\u9010\u4F8B\u5206\u914D",
      rp_grid(
        shiny::textInput("min_subject_id", "\u53D7\u8BD5\u8005\u7B5B\u9009\u53F7\u6216\u5185\u90E8 ID", value = ""),
        shiny::textInput("min_operator", "\u64CD\u4F5C\u8005\u59D3\u540D\uFF08\u53EF\u9009\uFF09", value = "")
      ),
      shiny::uiOutput("min_covariate_inputs"),
      shiny::textAreaInput("min_note", "\u5907\u6CE8\uFF08\u53EF\u9009\uFF09", value = "", rows = 2),
      shiny::tags$div(class = "rp-action-row", shiny::actionButton("min_assign", "\u5206\u914D\u4E0B\u4E00\u4F8B", class = "btn-success"))
    ),
    rp_table_card("\u6700\u8FD1\u4E00\u6B21\u76F2\u6001\u5206\u914D\u7ED3\u679C", DT::DTOutput("min_blinded")),
    shiny::tags$details(
      shiny::tags$summary("\u6700\u8FD1\u4E00\u6B21\u975E\u76F2\u6001\u7ED3\u679C\uFF08\u4EC5\u6388\u6743\u4EBA\u5458\u67E5\u770B\uFF09"),
      shiny::checkboxInput("min_show_unblinded", "\u786E\u8BA4\u5F53\u524D\u64CD\u4F5C\u8005\u4E3A\u6388\u6743\u4EBA\u5458", value = FALSE),
      DT::DTOutput("min_unblinded")
    ),
    rp_table_card("\u5F53\u524D\u5206\u914D\u8868", DT::DTOutput("min_assignments"))
  )
}
