rp_outputs_panel <- function() {
  shiny::tagList(
    rp_card(
      "\u8F93\u51FA\u8BBE\u7F6E",
      subtitle = "\u9009\u62E9\u9700\u8981\u751F\u6210\u7684\u62A5\u8868\u3001PDF \u4FE1\u5C01\u548C\u590D\u73B0\u6587\u4EF6\u3002",
      rp_grid(
        shiny::checkboxInput("generate_report", "\u751F\u6210\u968F\u673A\u5316\u62A5\u8868", value = TRUE),
        shiny::textInput("report_file", "\u62A5\u8868\u6587\u4EF6\u540D", value = "randomization_report.xlsx")
      ),
      rp_grid(
        shiny::checkboxInput("generate_random_envelope", "\u751F\u6210\u968F\u673A\u4FE1\u5C01 PDF", value = FALSE),
        shiny::textInput("random_envelope_file", "\u968F\u673A\u4FE1\u5C01\u6587\u4EF6\u540D", value = "random_envelopes.pdf")
      ),
      rp_grid(
        shiny::checkboxInput("generate_emergency_envelope", "\u751F\u6210\u5E94\u6025\u7834\u76F2\u4FE1\u5C01 PDF", value = FALSE),
        shiny::textInput("emergency_envelope_file", "\u5E94\u6025\u7834\u76F2\u4FE1\u5C01\u6587\u4EF6\u540D", value = "emergency_unblinding_envelopes.pdf")
      ),
      rp_grid(
        shiny::checkboxInput("generate_reproducibility", "\u751F\u6210\u590D\u73B0\u5305", value = TRUE),
        shiny::checkboxInput("standalone_reproducibility_code", "\u751F\u6210\u5B8C\u5168\u72EC\u7ACB\u590D\u73B0\u4EE3\u7801", value = TRUE),
        shiny::checkboxInput("generate_print_checklist", "\u751F\u6210\u6253\u5370\u88C5\u888B\u6838\u5BF9\u6E05\u5355", value = TRUE),
        shiny::checkboxInput("split_files_by_center", "\u6309\u4E2D\u5FC3\u62C6\u5206\u4FE1\u5C01\u6587\u4EF6\uFF08\u9884\u7559\uFF09", value = FALSE)
      )
    ),
    shiny::tags$div(class = "rp-sensitive", "\u654F\u611F\u8F93\u51FA\u5305\u62EC\u975E\u76F2\u6001\u4E3B\u8868\u3001\u5E94\u6025\u7834\u76F2\u4FE1\u5C01\u3001\u590D\u73B0\u5305\u4E2D\u7684\u76F2\u5E95\u4FE1\u606F\u3002\u4E0B\u8F7D\u524D\u8BF7\u786E\u8BA4\u64CD\u4F5C\u8005\u6743\u9650\u3002")
  )
}
