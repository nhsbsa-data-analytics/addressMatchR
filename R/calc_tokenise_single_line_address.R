#' Tokenise a column of a lazy Oracle table
#'
#' Tokenise a column of a lazy Oracle table. This function will retain
#' all columns and add a TOKEN column to the table.
#' https://stackoverflow.com/questions/59537458/how-to-pipe-sql-into-rs-dplyr
#'
#' @param df Database table
#' @param col Column to be tokenized
#'
#' @examples
#'
#' @export
calc_oracle_tokenise <- function(df, col) {

  # Pull the connection
  db_connection <- df$src$con

  # Formulate the SQL for tokenising in Oracle
  sql_query <- dbplyr::build_sql(
    con = db_connection,
      "
      SELECT
        t.*,
        REGEXP_SUBSTR(t.", dplyr::sql(col), ", '[^[:space:]]+', 1, level) as TOKEN

      FROM (", dbplyr::sql_render(df), ") t

      CONNECT BY
        level <= length(regexp_replace(t.", dplyr::sql(col), ", '[^[:space:]]+')) + 1
      "
  )

  dplyr::tbl(db_connection, dplyr::sql(sql_query))
}
