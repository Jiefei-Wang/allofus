#' @keywords internal
.onLoad <- function(libname = find.package("allofus"), pkgname = "allofus") {
  # if the verily env vars aren't set, try to source them from ~/.aou-env
  if (Sys.getenv("WORKSPACE_CDR") == "") {
    aou_env_path <- path.expand("~/.aou-env")

    if (file.exists(aou_env_path)) {
      lines <- readLines(aou_env_path, warn = FALSE)
      lines <- lines[grepl("^\\s*export\\s+[^=]+=", lines)]
      kv <- sub("^\\s*export\\s+", "", lines)
      keys <- sub("=.*$", "", kv)
      vals <- gsub('^"|"$', "", sub("^[^=]+=", "", kv))
      names(vals) <- keys

      # set CDR and bucket directly from the file
      to_set <- list(
        WORKSPACE_CDR = unname(vals["WORKSPACE_CDR"]),
        WORKSPACE_BUCKET = unname(vals["WORKSPACE_BUCKET"])
      )

      # derive billing project from the bucket: strip leading "gs://cloned-mybucket-"
      if (!is.na(vals["WORKSPACE_BUCKET"])) {
        google_project <- sub(
          "^gs://cloned-mybucket-",
          "",
          vals["WORKSPACE_BUCKET"]
        )
        to_set$GOOGLE_PROJECT <- unname(google_project)
      }

      # only set vars that have a value and aren't already set
      to_set <- to_set[
        !vapply(to_set, function(x) is.na(x) || x == "", logical(1))
      ]
      to_set <- to_set[Sys.getenv(names(to_set)) == ""]

      if (length(to_set) > 0) do.call(Sys.setenv, to_set)
    }
  }

  op <- options()
  op.aou <- list(
    aou.default.cdr = Sys.getenv("WORKSPACE_CDR"),
    aou.default.bucket = Sys.getenv("WORKSPACE_BUCKET"),
    aou.default.con = NULL
  )
  toset <- !(names(op.aou) %in% names(op))
  if (any(toset)) {
    options(op.aou[toset])
  }
  invisible()
}

greet_startup <- function() {
  msg <- paste0(
    c(
      "{cli::symbol$heart} Thank you for using the {.pkg allofus} R package! {cli::symbol$heart}",
      "{cli::symbol$warning} This package continues to be developed as All of Us grows and changes. Please report any issues to {.url https://github.com/roux-ohdsi/allofus/issues}.",
      "{cli::symbol$info} The {.pkg allofus} R package is not affiliated with or endorsed by the All of Us Research Program. \n\n"
    ),
    collapse = "\n"
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}

.onAttach <- function(libname = find.package("allofus"), pkgname = "allofus") {
  greet_startup()
  if (!on_workbench()) {
    rlang::inform(cli::format_inline("{cli::symbol$warning} This package has limited functionality outside of the All of Us Researcher Workbench"),
      class = "packageStartupMessage"
    )
  }
}
