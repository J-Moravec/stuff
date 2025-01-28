#' Replace values
#'
#' Replace the values in `x`, specified by the vector of values
#' `values` with the values in the vector `replace`.
#'
#' This is a variant of [base::replace()] that might be more useful when replacing
#' several differet values, e.g., when cleaning and recoding data.
#'
#' @param x a subsetable object, such as a vector, matrix, or a data.frame
#' @param values a vector of values in 'x' to be replaced by values in `replace`
#' @param replace replacement values
#' @param select a numeric or a character vector specifying which columns are selected
#' for replacement
#' @param ... further arguments passed to or from other methods
#' @return an object with replaced values
#'
#' @seealso
#' [base::replace()] for a different interface for a similar idea,
#' [base::Extract] for general subsetting and replacement
#'
#' Consider also [base::transform()], [base::with()], and [base::within()] for
#' `data.frame` modification.
#'
#' @examples
#' vec = c("foo", "bar", "baz")
#' replace2(vec, "bar", "spam")
#'
#' # No error is reported if values are not in object
#' replace2(vec, "spam", "eggs")
#'
#' # Useful when recoding
#' replace2(
#'     iris,
#'     c("setosa", "versicolor", "virginica"),
#'     c("set", "ver", "vir")
#'     )
#'
#'
#' @export
replace2 = function(x, values, replace, ...){
    UseMethod("replace2", x)
    }


#' @rdname replace2
#' @export
replace2.default = function(x, values, replace, ...){
    if(length(values) != length(replace))
        stop("The vector `values` and `replace` must have the same length!")

    match = match(x, values)
    x[!is.na(match)] = replace[match][!is.na(match)]
    x
    }


#' @rdname replace2
#' @export
replace2.data.frame = function(x, values, replace, select = NULL, ...){
    if(is.null(select))
        select = seq_along(x)

    x[select] = lapply(x[select], replace2.default, values, replace)
    x
    }
