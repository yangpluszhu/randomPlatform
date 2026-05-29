#' Launch the local randomPlatform Shiny application.
#'
#' Starts the bundled Shiny app by building `shiny::shinyApp(ui = rp_app_ui(),
#' server = server)` and passing it to `shiny::runApp()`.
#'
#' @param host Character scalar giving the interface to bind to. The default is
#'   `"127.0.0.1"`, which keeps the app local to the current machine. Use
#'   `"0.0.0.0"` only when you intentionally want broader network reachability;
#'   doing so triggers a warning. Other valid host values are those accepted by
#'   `shiny::runApp()`.
#' @param port Integer scalar giving the TCP port to listen on. The default is
#'   `3838`. Choose a free port if `3838` is already in use.
#' @param launch.browser Logical scalar. When `TRUE` (the default), Shiny will
#'   attempt to open the app in the system browser after launch. Set to `FALSE`
#'   when launching in an environment where browser automation is not desired.
#' @param stop_on_session_end Logical scalar. When `TRUE` (the default), the app
#'   server is wrapped so that `shiny::stopApp()` is called when the session
#'   ends. When `FALSE`, `rp_app_server` is used directly and the app keeps the
#'   standard Shiny session lifecycle.
#' @param options Named list of additional arguments passed through to
#'   `shiny::runApp()` via `do.call()`. Defaults to `list()`. Use this to
#'   supply any extra runApp options that are not exposed as formal arguments
#'   here.
#'
#' @return The value returned by `shiny::runApp()`, invisibly or visibly as
#'   provided by Shiny.
#'
#' @details
#' The app binds to `127.0.0.1` by default so it is only reachable from the
#' local machine. Binding to `0.0.0.0` exposes the app on all network
#' interfaces, so the function warns when that host is selected. If
#' `stop_on_session_end = TRUE`, a small wrapper server calls `shiny::stopApp()`
#' when the session ends; otherwise, `rp_app_server` is passed through as-is.
#' Any entries supplied in `options` are forwarded directly to
#' `shiny::runApp()`.
#'
#' @seealso [rp_randomize()]
#'
#' @examples
#' if (interactive()) {
#'   rp_launch_app()
#' }
#'
#' @export
rp_launch_app <- function(
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE,
  stop_on_session_end = TRUE,
  options = list()
) {
  if (identical(host, "0.0.0.0")) {
    warning(
      "The app is being bound to 0.0.0.0 and may be reachable from other machines. ",
      "Use this only in a controlled network.",
      call. = FALSE
    )
  }
  server <- if (isTRUE(stop_on_session_end)) {
    function(input, output, session) {
      rp_app_server(input, output, session)
      session$onSessionEnded(function() {
        shiny::stopApp()
      })
    }
  } else {
    rp_app_server
  }
  app <- shiny::shinyApp(ui = rp_app_ui(), server = server)
  do.call(
    shiny::runApp,
    c(list(appDir = app, host = host, port = port, launch.browser = launch.browser), options)
  )
}
