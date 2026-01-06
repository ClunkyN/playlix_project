#!/usr/bin/env Rscript
# db_test.R
# Quick DB connectivity checks using environment variables.
# Usage:
#  Rscript db_test.R
# Ensure env vars: DB_HOST, DB_USER, DB_PASSWORD, DB_NAME, DB_PORT, DB_SSL_CA (optional)

suppressMessages({
  library(DBI)
  library(RMariaDB)
})

safe_get <- function(name) {
  v <- Sys.getenv(name, unset = "")
  if (v == "") return(NA_character_) else return(v)
}

host <- safe_get('DB_HOST')
user <- safe_get('DB_USER')
password <- safe_get('DB_PASSWORD')
dbname <- safe_get('DB_NAME')
port <- as.integer(safe_get('DB_PORT') %||% '3306')
ssl_ca <- safe_get('DB_SSL_CA')

cat('DB_HOST=', host, '\n', sep='')
cat('DB_USER=', user, '\n', sep='')
cat('DB_NAME=', dbname, '\n', sep='')
cat('DB_PORT=', port, '\n', sep='')
cat('DB_SSL_CA=', ifelse(is.na(ssl_ca), '<not set>', ssl_ca), '\n', sep='')
if (!is.na(ssl_ca)) cat('DB_SSL_CA exists? ', file.exists(ssl_ca), '\n')

if (any(is.na(c(host, user, password, dbname)))) {
  cat('ERROR: one or more required env vars are missing (DB_HOST, DB_USER, DB_PASSWORD, DB_NAME)\n')
  quit(status = 2)
}

opts <- list(host = host, user = user, password = password, dbname = dbname, port = port)
if (!is.na(ssl_ca)) opts$ssl.ca <- ssl_ca

con <- NULL
res_code <- 0
tryCatch({
  con <- do.call(DBI::dbConnect, c(list(RMariaDB::MariaDB()), opts))
  cat('Connected OK\n')
  cat('SELECT 1 ->\n')
  print(dbGetQuery(con, 'SELECT 1 AS ok'))
  cat('Tables ->\n')
  print(dbListTables(con))
  cat('Count movies ->\n')
  print(tryCatch(dbGetQuery(con, 'SELECT COUNT(*) AS cnt FROM movies'), error = function(e) e))
  cat('Last 5 rows from movies ->\n')
  print(tryCatch(dbGetQuery(con, 'SELECT * FROM movies ORDER BY id DESC LIMIT 5'), error = function(e) e))
}, error = function(e) {
  cat('DB connection / query error:\n')
  cat(conditionMessage(e), '\n')
  res_code <<- 1
}, finally = {
  if (!is.null(con)) try(dbDisconnect(con), silent = TRUE)
})

quit(status = res_code)
