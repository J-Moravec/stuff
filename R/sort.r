#' Sort data frame
#'
#' Sort data frame according to selected columns.
#'
#' This is an S3 method for the [base::sort()] generics and in many way
#' it is similar to the later [base::sort_by()] function with the major difference
#' is the interface, while [base::sort_by()] is using a formula interface and thus
#' non-standard evaluation, this function is expecting index or column names.
#'
#' The consequence is that column names for sorting can be provided with a variable,
#' but we are limited to the parameter order of [base::sort()], with `decreasing` preceding
#' `by`.
#'
#' @param x a data.frame
#' @param decreasing logical vector passed to [base::order()] indicating increasing or decreasing
#'  order of a particular column is used, recycled as necessary
#' @param by column names or indices, by default the first column is used
#' @param ... arguments passed to [base::order()] such as `method` or `na.last`
#' @return a data.frame sorted according to selected columns
#'
#' @seealso
#' The [base::sort()] is the generic function.
#' [base::order()] is internally called to determines ordering.
#' [base::sort_by()] for an alternative interface with a formula allowing non-standard evaluation
#'
#' @examples
#' # By default, first column is used
#' sort(iris)
#'
#' # Following are identical
#' sort(iris, by = "Species")
#' iris |> sort(by = "Species")
#' iris[order(iris[["Species"]]),]
#'
#' # Pass columns using a variable
#' col = "Species"
#' iris |> sort(by = col)
#'
#' # With multiple columns, order is important
#' iris |> sort(by = c("Sepal.Length", "Sepal.Width"))
#' iris |> sort(by = c("Sepal.Width", "Sepal.Length"))
#'
#' # Compare with 'sort_by'
#' iris |> sort_by( ~ Species)
#'
#' # Won't work:
#' # iris |> sort_by("Species")
#' # iris |> sort_by(col)
#'
#' @method sort data.frame
#' @export
sort.data.frame = function(x, decreasing = FALSE, by = 1, ...){
    nm = names(x)

    if(is.character(by) && !all(by %in% nm))
        stop("invalid 'by' argument")
    if(is.numeric(by) && ( any(by < 1) && any(by > length(x)) ) )
        stop("invalid 'by' argument")

    x[do.call(order, c(x[by], "decreasing" = decreasing, ...)),, drop = FALSE]
    }
