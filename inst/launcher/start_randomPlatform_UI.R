args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

if (!requireNamespace("randomPlatform", quietly = TRUE)) {
  pkg_dir <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
    install.packages(pkg_dir, repos = NULL, type = "source")
  }
}

if (!requireNamespace("randomPlatform", quietly = TRUE)) {
  stop("randomPlatform is not installed. Please install the package first.", call. = FALSE)
}

randomPlatform::rp_launch_app(
  host = "127.0.0.1",
  port = 3838,
  launch.browser = TRUE,
  stop_on_session_end = TRUE
)
