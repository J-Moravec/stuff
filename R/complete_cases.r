#' Find complete cases
#'
#' Find complete cases in a matrix or a data.fame, i.e., rows with no missing values.
#'
#' This function is a user-friendly wrapper for [stats::complete.cases()], which returns
#' a logical vector.
#'
#' @param x a matrix or a data.frame
#' @param select a numeric vector of indices or a character vector of column names,
#' the presence of missing values is checked only in these columns, other columns are ignored
#' @return a matrix or a data.frame with no missing values in selected columns
#'
#' @seealso
#' [stats::complete.cases()] for a similar function returning a logical indicator,
#' [base::is.na()] for checking if the vector or a matrix contains `NA`,
#' [stats::na.omit()] for a generic used to remove `NA`s.
#'
#' @examples
#' d = data.frame(
#'   "foo" = c(1, NA, 3),
#'   "bar" = c(NA, 2, 3)
#'   )
#'
#' # Following are identical
#' complete_cases(d)
#' d[stats::complete.cases(d),]
#'
#' # Consider only first column
#' complete_cases(d, "foo")
#' complete_cases(d, 1)
#' d[stats::complete.cases(d[1]),]
#'
#' @export
complete_cases = function(x, select = NULL){
    if(is.null(select))
        select = seq_len(ncol(x))
    x[stats::complete.cases(x[,select]),]
    }
