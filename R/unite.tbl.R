#' Unite multiple columns into one by pasting strings together
#'
#' Convenience function to paste together multiple columns into one in a `{dbplyr}`
#' lazy table.
#'
#' @param data A `{dbplyr}` lazy table.
#' @inheritParams tidyr::unite
#'
#' @return The data frame, with additional column created
#'
#' @note https://stackoverflow.com/a/76013501/8519200
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df -> unite.tbl(
#'   united_col,
#'   col_1, col_2,
#'   sep = " ", remove = FALSE, na.rm = TRUE
#' )
#' }
unite.tbl <- function (data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)
{
  dot_names  <- sapply(substitute(list(...))[-1], deparse)

  shown_cols <- data %>% colnames()
  shown_cols <- `if`(
    remove,
    setdiff(shown_cols, dot_names),
    shown_cols
  )
  shown_col_str <- paste(shown_cols, collapse = ", ")

  concat_str <- ifelse(
    na.rm,
    paste0(
      paste0(
        "NVL2(",
        dot_names%>% utils::head(-1), ", ",
        dot_names%>% utils::head(-1), " || '", sep, "'",
        ", '')",
        collapse = " || "
      ),
      " || NVL(", dot_names%>% utils::tail(1), ", '')"
    ),
    paste0(dot_names, collapse = paste0(" || '", sep, "' || "))
  )

  col <- deparse(substitute(col))

  subquery <- utils::capture.output(dplyr::show_query(data))[-1] %>%
    paste(collapse = " ")

  query    <- paste(
    "SELECT", shown_col_str, ",",
    concat_str, "AS", col,
    "FROM (",
    subquery,
    ")"
  )

  dplyr::tbl(data$src$con, dplyr::sql(query))
}
