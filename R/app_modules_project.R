rp_project_panel <- function() {
  output_dir <- rp_default_output_dir()
  shiny::tagList(
    rp_card(
      "\u9879\u76EE\u4FE1\u606F",
      subtitle = "\u7528\u4E8E\u62A5\u8868\u3001\u4FE1\u5C01\u548C\u590D\u73B0\u6587\u4EF6\u7684\u57FA\u7840\u4FE1\u606F\u3002",
      rp_grid(
        shiny::textInput("project_name", "\u9879\u76EE\u540D\u79F0", value = "\u793A\u4F8B\u4E34\u5E8A\u8BD5\u9A8C"),
        shiny::textInput("protocol_no", "\u65B9\u6848\u7F16\u53F7", value = "RP-001"),
        shiny::textInput("sponsor_name", "\u7533\u529E\u5355\u4F4D\u540D\u79F0", value = "\u793A\u4F8B\u7533\u529E\u5355\u4F4D"),
        shiny::selectInput("language", "\u8BED\u8A00", choices = c("Chinese" = "zh-CN", "English" = "en-US"), selected = "zh-CN")
      )
    ),
    rp_card(
      "\u8F93\u51FA\u4E0E\u5B89\u5168",
      subtitle = "\u9ED8\u8BA4\u4EC5\u5728\u672C\u5730\u8FD0\u884C\uFF0C\u654F\u611F\u6587\u4EF6\u4E0B\u8F7D\u524D\u9700\u8981\u4E8C\u6B21\u786E\u8BA4\u3002",
      shiny::tags$div(
        class = "rp-output-dir-row",
        shiny::textInput("output_dir", "\u8F93\u51FA\u76EE\u5F55", value = output_dir),
        shinyFiles::shinyDirButton("choose_output_dir", "\u9009\u62E9\u76EE\u5F55", "\u8BF7\u9009\u62E9\u8F93\u51FA\u76EE\u5F55", class = "btn-secondary rp-dir-button")
      ),
      rp_grid(
        shiny::checkboxInput("encrypt_sensitive_outputs", "\u542F\u7528\u654F\u611F\u8F93\u51FA\u5BC6\u7801\u5FC5\u586B\u68C0\u67E5\uFF08\u5F53\u524D\u4E0D\u52A0\u5BC6\u6587\u4EF6\uFF09", value = FALSE),
        shiny::passwordInput("password", "\u5BC6\u7801\uFF08\u5F53\u524D\u4EC5\u6821\u9A8C\u662F\u5426\u586B\u5199\uFF09", value = "")
      )
    )
  )
}

rp_default_output_dir <- function() {
  home <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
  documents <- file.path(home, "Documents")
  root <- if (dir.exists(documents)) documents else home
  path <- file.path(root, "randomPlatform_outputs")
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(if (dir.exists(path)) path else root, winslash = "/", mustWork = TRUE)
}
