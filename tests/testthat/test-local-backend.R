# Tests for the backend-aware execution path (get_query_table / get_query_table_local)
# that lets allofus run against a non-BigQuery DBI backend such as a local DuckDB
# database (see the mockallofus package). Uses an inline DuckDB connection so the
# tests are self-contained and need no Workbench.

skip_if_not_installed("duckdb")

local_duckdb <- function(envir = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = envir)
  DBI::dbExecute(con, "CREATE TABLE person (person_id BIGINT, year_of_birth INTEGER)")
  DBI::dbExecute(con, "INSERT INTO person SELECT i, 1980 + (i % 30) FROM range(1, 51) t(i)")
  old <- options(aou.default.con = con, aou.default.cdr = "main")
  withr::defer(options(old), envir = envir)
  con
}

test_that("get_query_table dispatches non-BigQuery connections to the local path", {
  con <- local_duckdb()
  res <- get_query_table("SELECT person_id FROM person", collect = TRUE, con = con)
  expect_equal(nrow(res), 50)
})

test_that("aou_sql runs on a local DuckDB connection without GOOGLE_PROJECT", {
  withr::local_envvar(GOOGLE_PROJECT = "")
  con <- local_duckdb()
  res <- aou_sql("SELECT count(*) AS n FROM `{CDR}.person`", collect = TRUE)
  expect_equal(as.numeric(res$n), 50)
})

test_that("get_query_table_local strips backticks and maps FLOAT64 to DOUBLE", {
  con <- local_duckdb()
  res <- get_query_table_local(
    "SELECT CAST(year_of_birth AS FLOAT64) AS yob FROM `main.person`",
    collect = TRUE, con = con
  )
  expect_type(res$yob, "double")
})

test_that("the CREATE TEMP TABLE ... ; SELECT * FROM ... wrap returns a tbl reference", {
  con <- local_duckdb()
  q <- "CREATE TEMP TABLE table1 AS\nSELECT person_id FROM person WHERE year_of_birth > 1990;\nSELECT * FROM table1"
  ref <- get_query_table(q, collect = FALSE, con = con)
  expect_s3_class(ref, "tbl_sql")
  expect_gt(nrow(dplyr::collect(ref)), 0)
})

test_that("chained aou_compute() calls do not collide on a reused temp name", {
  con <- local_duckdb()
  step1 <- dplyr::tbl(con, "person") |>
    dplyr::filter(.data$year_of_birth > 1985) |>
    aou_compute()
  # a second compute that reads from the first must still find the first result
  step2 <- step1 |>
    dplyr::summarise(n = dplyr::n()) |>
    aou_compute()
  expect_gt(as.numeric(dplyr::pull(dplyr::collect(step2), n)), 0)
})

test_that("aou_create_temp_table works on a local DuckDB connection", {
  con <- local_duckdb()
  df <- data.frame(concept_id = c(201826L, 4193704L), category = c("a", "b"))
  tt <- aou_create_temp_table(df, con = con)
  expect_equal(nrow(dplyr::collect(tt)), 2)
})
