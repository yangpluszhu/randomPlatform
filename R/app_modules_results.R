rp_results_panel <- function() {
  shiny::tagList(
    rp_card(
      "\u751F\u6210\u4E0E\u9884\u89C8",
      subtitle = "\u751F\u6210\u524D\u8BF7\u6838\u5BF9\u53C2\u6570\u6458\u8981\uFF1B\u751F\u6210\u540E\u53EF\u9884\u89C8\u8868\u683C\u548C\u6587\u4EF6 hash\u3002",
      shiny::tags$div(class = "rp-info-box", shiny::uiOutput("parameter_summary")),
      shiny::uiOutput("generation_status")
    ),
    rp_table_card("\u76F2\u6001\u73B0\u573A\u8868\u9884\u89C8", DT::DTOutput("blinded_table")),
    shiny::tags$details(
      shiny::tags$summary("\u975E\u76F2\u6001\u4E3B\u8868\uFF08\u4EC5\u6388\u6743\u4EBA\u5458\u67E5\u770B\uFF09"),
      shiny::checkboxInput("show_unblinded", "\u786E\u8BA4\u5F53\u524D\u64CD\u4F5C\u8005\u4E3A\u6388\u6743\u4EBA\u5458", value = FALSE),
      DT::DTOutput("unblinded_table")
    ),
    rp_table_card("\u5206\u7EC4\u6C47\u603B", DT::DTOutput("balance_table")),
    rp_table_card("\u8F93\u51FA\u6587\u4EF6", DT::DTOutput("files_table")),
    rp_table_card("SHA-256", DT::DTOutput("hash_table"))
  )
}

rp_downloads_panel <- function() {
  shiny::tagList(
    rp_card(
      "\u6587\u4EF6\u4E0B\u8F7D",
      subtitle = "\u654F\u611F\u6587\u4EF6\u9700\u5148\u786E\u8BA4\u6388\u6743\u3002",
      shiny::checkboxInput("confirm_sensitive_download", "\u6211\u786E\u8BA4\u5F53\u524D\u64CD\u4F5C\u8005\u6709\u6743\u4E0B\u8F7D\u5305\u542B\u76F2\u5E95\u6216\u53EF\u63ED\u76F2\u4FE1\u606F\u7684\u654F\u611F\u6587\u4EF6\u3002", value = FALSE),
      shiny::tags$div(
        class = "rp-download-grid",
        shiny::downloadButton("download_zip", "\u4E0B\u8F7D\u5B8C\u6574\u8F93\u51FA ZIP"),
        shiny::downloadButton("download_report", "\u4E0B\u8F7D\u968F\u673A\u5316\u62A5\u8868"),
        shiny::downloadButton("download_blinded", "\u4E0B\u8F7D\u76F2\u6001\u73B0\u573A\u8868"),
        shiny::downloadButton("download_unblinded", "\u4E0B\u8F7D\u975E\u76F2\u6001\u4E3B\u8868"),
        shiny::downloadButton("download_random_pdf", "\u4E0B\u8F7D\u968F\u673A\u4FE1\u5C01 PDF"),
        shiny::downloadButton("download_emergency_pdf", "\u4E0B\u8F7D\u5E94\u6025\u7834\u76F2\u4FE1\u5C01 PDF"),
        shiny::downloadButton("download_repro", "\u4E0B\u8F7D\u590D\u73B0\u5305 ZIP"),
        shiny::downloadButton("download_audit", "\u4E0B\u8F7D\u5BA1\u8BA1\u65E5\u5FD7"),
        shiny::downloadButton("download_hashes", "\u4E0B\u8F7D hash \u6587\u4EF6")
      )
    )
  )
}

rp_audit_panel <- function() {
  shiny::tagList(
    rp_card(
      "\u5BA1\u8BA1\u4E0E\u590D\u73B0",
      subtitle = "\u67E5\u770B R \u8FD0\u884C\u73AF\u5883\u3001RNG \u8BBE\u7F6E\u3001hash \u548C\u590D\u73B0\u6821\u9A8C\u7ED3\u679C\u3002",
      shiny::tags$div(class = "rp-info-box", shiny::uiOutput("runtime_info")),
      shiny::tags$div(class = "rp-action-row", shiny::actionButton("verify_repro", "\u8FD0\u884C\u590D\u73B0\u6821\u9A8C", class = "btn-primary")),
      shiny::verbatimTextOutput("verify_result")
    ),
    rp_card(
      "\u5BA1\u8BA1\u65E5\u5FD7\u9884\u89C8",
      shiny::tags$div(class = "rp-preview-box", shiny::verbatimTextOutput("audit_preview"))
    ),
    rp_card(
      "\u590D\u73B0\u547D\u4EE4",
      shiny::tags$code("source('reproducibility/reproduce_randomization.R')")
    )
  )
}
