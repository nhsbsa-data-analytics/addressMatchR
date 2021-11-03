#' Tokenise a column of a lazy Oracle table
#'
#' Tokenise a column of a lazy Oracle table. This function will retain
#' all columns and add a TOKEN_NUMBER and TOKEN columns to the table.
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

  # Pull the columns and create a comma'd list sring of them "col1, col2, col3"
  # to use in the SQL query
  col_string <- paste0(colnames(df), collapse = ", ")

  # Formulate the SQL for tokenising in Oracle
  sql_query <- dbplyr::build_sql(
    con = db_connection,
      "
      SELECT
        ", dplyr::sql(col_string),",
        ROW_NUMBER() OVER (PARTITION BY ", dplyr::sql(col_string), " ORDER BY lines.column_value) AS TOKEN_NUMBER,
        TRIM(REGEXP_SUBSTR(", dplyr::sql(col), ", '[^[:space:]]+', 1, lines.column_value))        AS TOKEN

      FROM
        (", dbplyr::sql_render(df), "),
        TABLE(CAST(MULTISET(SELECT LEVEL FROM dual CONNECT BY INSTR(", dplyr::sql(col), " , ' ', 1, LEVEL - 1) > 0)  as  sys.odciNumberList))  lines
      "
  )

  dplyr::tbl(db_connection, dplyr::sql(sql_query))
}
