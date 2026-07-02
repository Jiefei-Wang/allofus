#' Create a Cloud Storage bucket for the workspace
#' @description Verily Workbench workspaces (workbench 2.0) don't
#'   automatically provision a workspace bucket the way older ("workbench
#'   1.0") All of Us workspaces did. This creates one via the `wb` CLI (or
#'   resolves it if a bucket with the same name already exists) and sets
#'   `WORKSPACE_BUCKET` (or `WORKSPACE_TEMP_BUCKET` if `temporary = TRUE`) for
#'   the current R session, so functions like `aou_ls_bucket()` work without
#'   further setup.
#' @details The resolved bucket URL is cached to `~/.aou-env` (alongside
#'   `WORKSPACE_CDR` and `GOOGLE_PROJECT`, which are cached there the same
#'   way when the package loads). This means you only need to run
#'   `aou_create_bucket()` once per workspace, not once per session: on
#'   subsequent loads, `library(allofus)` reads the cached values from
#'   `~/.aou-env` directly instead of recreating anything or re-querying the
#'   `wb` CLI.
#'
#'   To see what's currently cached (e.g., to check which buckets you've
#'   already created in this workspace), read the file directly:
#'   `cat(readLines("~/.aou-env"))`. Each line is a plain
#'   `export VARIABLE="value"` entry. If a cached value ever goes stale (for
#'   example, a bucket was deleted outside of R), delete the corresponding
#'   line, or delete the file entirely to force everything to be re-resolved
#'   on the next `library(allofus)` call.
#' @param temporary If `TRUE`, creates a temporary bucket whose contents are
#'   automatically deleted after `auto_delete` days, instead of the
#'   persistent workspace bucket. Useful for intermediate files you don't
#'   need to keep.
#' @param name Resource name for the bucket within the workspace. Defaults to
#'   `"workspace-bucket"`, or `"temporary-workspace-bucket"` if
#'   `temporary = TRUE`.
#' @param auto_delete Number of days after which objects in a temporary
#'   bucket are automatically deleted. Only used if `temporary = TRUE`.
#' @return The bucket's `gs://` URL (invisibly).
#' @export
#' @examplesIf on_workbench()
#' aou_create_bucket()
#' aou_create_bucket(temporary = TRUE)
aou_create_bucket <- function(temporary = FALSE,
                               name = if (temporary) "temporary-workspace-bucket" else "workspace-bucket",
                               auto_delete = 14) {
  if (!on_workbench()) {
    cli::cli_abort("This function only works on the All of Us Researcher Workbench.", call = NULL)
  }

  existing <- suppressWarnings(system2("wb", c("resource", "resolve", "--name", name), stdout = TRUE, stderr = FALSE))
  already_exists <- is.null(attr(existing, "status"))

  if (!already_exists) {
    # --description is omitted: the `wb` CLI's argument parser splits on
    # whitespace regardless of how the argument is passed from R, so any
    # multi-word description gets misread as unmatched extra arguments
    create_args <- c(
      "resource", "create", "gcs-bucket",
      paste0("--name=", name),
      "--cloning=COPY_NOTHING"
    )
    if (isTRUE(temporary)) {
      create_args <- c(create_args, paste0("--auto-delete=", auto_delete))
    }

    created <- suppressWarnings(system2("wb", create_args, stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(created, "status"))) {
      cli::cli_abort(c(
        "Failed to create bucket {.val {name}}.",
        "x" = paste(created, collapse = "\n")
      ), call = NULL)
    }
  }

  bucket_url <- suppressWarnings(system2("wb", c("resource", "resolve", "--id", name), stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(bucket_url, "status")) || length(bucket_url) != 1) {
    cli::cli_abort("Unable to resolve the URL for bucket {.val {name}}.", call = NULL)
  }

  env_var <- if (isTRUE(temporary)) "WORKSPACE_TEMP_BUCKET" else "WORKSPACE_BUCKET"
  env_list <- list(bucket_url)
  names(env_list) <- env_var
  do.call(Sys.setenv, env_list)
  write_aou_env(env_list)

  if (identical(env_var, "WORKSPACE_BUCKET")) {
    options(aou.default.bucket = bucket_url)
  }

  cli::cli_inform(c("v" = "{.envvar {env_var}} set to {.val {bucket_url}}"))
  invisible(bucket_url)
}
