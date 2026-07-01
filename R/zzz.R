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

#' Fetch workspace environment variables from the Workbench Manager API
#' @description On workbench 2.0 (Verily-based), WORKSPACE_CDR is never
#'   injected as an OS env var the way it was on classic Terra-based
#'   workbenches, and there's no `~/.aou-env` file created automatically. This
#'   reads workspace identity out of `~/.workbench/context.json`, gets a
#'   short-lived access token from the `wb` CLI, and queries the Workspace
#'   Manager API directly for the referenced BigQuery CDR dataset. Returns
#'   `NULL` (never errors) if any step fails, so `.onLoad()` can proceed
#'   without env vars set.
#' @return A named list with `WORKSPACE_CDR` and (if available) `GOOGLE_PROJECT`,
#'   or `NULL`.
#' @keywords internal
workbench_env_vars <- function() {
  context_path <- path.expand("~/.workbench/context.json")
  if (!file.exists(context_path) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(NULL)
  }

  context <- jsonlite::fromJSON(context_path, simplifyVector = FALSE)
  workspace_id <- context$workspace$uuid
  wsm_uri <- context$server$workspaceManagerUri
  if (is.null(workspace_id) || is.null(wsm_uri)) {
    return(NULL)
  }

  # context.json's config$wbPath can point to a stale location, so rely on
  # PATH resolution (same as the `wb` CLI's own shell scripts do) rather than
  # trusting it
  token <- suppressWarnings(system2("wb", c("auth", "print-access-token"), stdout = TRUE, stderr = FALSE))
  if (length(token) != 1 || token == "") {
    return(NULL)
  }

  resources_url <- paste0(wsm_uri, "/api/workspaces/v1/", workspace_id, "/resources")
  response <- suppressWarnings(system2(
    "curl",
    c("-s", "-H", paste0("Authorization: Bearer ", token), resources_url),
    stdout = TRUE, stderr = FALSE
  ))
  resources <- jsonlite::fromJSON(paste(response, collapse = "\n"), simplifyVector = FALSE)$resources

  # the main CDR is a referenced BigQuery dataset; workspaces can also have a
  # "prep_"-prefixed scratch dataset alongside it, which we don't want
  cdr_resource <- purrr::detect(
    resources,
    ~ .x$metadata$resourceType == "BIG_QUERY_DATASET" &&
      .x$metadata$stewardshipType == "REFERENCED" &&
      !startsWith(.x$metadata$name, "prep_")
  )
  if (is.null(cdr_resource)) {
    return(NULL)
  }

  bq <- cdr_resource$resourceAttributes$gcpBqDataset
  to_set <- list(WORKSPACE_CDR = paste0(bq$projectId, ".", bq$datasetId))

  google_project <- context$workspace$googleProjectId
  if (!is.null(google_project) && google_project != "") {
    to_set$GOOGLE_PROJECT <- google_project
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
