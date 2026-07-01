#' @keywords internal
.onLoad <- function(libname = find.package("allofus"), pkgname = "allofus") {
  # if the verily env vars aren't set, try to source them from ~/.aou-env
  # (workbench 1.0-style manual setup), then fall back to querying the
  # Workbench Manager API directly (workbench 2.0, where WORKSPACE_CDR is
  # never injected as an OS env var)
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
    } else {
      to_set <- tryCatch(workbench_env_vars(), error = function(e) NULL)
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

#' Fetch workspace environment variables from the Workbench CLI
#' @description On workbench 2.0 (Verily-based), WORKSPACE_CDR is never
#'   injected as an OS env var the way it was on classic Terra-based
#'   workbenches, and there's no `~/.aou-env` file created automatically.
#'   `wb auth print-access-token` can't be used to hit the Workspace Manager
#'   API directly here: it returns the workspace's pet service account
#'   identity (a GCE VM instance-identity token), which the API doesn't
#'   accept, rather than the logged-in user's credential. `wb resource list`
#'   sidesteps this entirely by having the CLI handle its own authentication
#'   internally. Returns `NULL` (never errors) if any step fails, so
#'   `.onLoad()` can proceed without env vars set.
#' @return A named list with `WORKSPACE_CDR` and (if available) `GOOGLE_PROJECT`,
#'   or `NULL`.
#' @keywords internal
workbench_env_vars <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NULL)
  }

  response <- suppressWarnings(system2("wb", c("resource", "list", "--format=json"), stdout = TRUE, stderr = FALSE))
  resources <- tryCatch(
    jsonlite::fromJSON(paste(response, collapse = "\n"), simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(resources)) {
    return(NULL)
  }

  # the main CDR is a referenced BigQuery dataset; workspaces can also have a
  # "prep_"-prefixed scratch dataset alongside it, which we don't want
  cdr_resource <- purrr::detect(
    resources,
    ~ .x$resourceType %in% c("BQ_DATASET", "BIGQUERY_DATASET", "BIG_QUERY_DATASET") &&
      .x$stewardshipType == "REFERENCED" &&
      !startsWith(.x$datasetId, "prep_")
  )
  if (is.null(cdr_resource)) {
    return(NULL)
  }

  to_set <- list(WORKSPACE_CDR = paste0(cdr_resource$projectId, ".", cdr_resource$datasetId))

  context_path <- path.expand("~/.workbench/context.json")
  if (file.exists(context_path)) {
    context <- tryCatch(jsonlite::fromJSON(context_path, simplifyVector = FALSE), error = function(e) NULL)
    google_project <- context$workspace$googleProjectId
    if (!is.null(google_project) && google_project != "") {
      to_set$GOOGLE_PROJECT <- google_project
    }
  }

  to_set
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
