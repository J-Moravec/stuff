#' Operators
#'
#' A collection of operators.
#'
#' # Details
#'
#' `Not()` is a function alias for `!` allowing for negation to be used in pipes
#' and enhance readability in some contexts.
#'
#' `%nin%` is a simple negation of `%in%`.
#'
#' @param x,y arguments to operators
#' @return return value depends on the operator, see details
#' @name operators
#'
#' @examples
#' # Operators can't be used in pipes.
#' # Won't work:
#' # TRUE |> !
#' # TRUE |> `!`()
#'
#' # Works:
#' TRUE |> Not()
#'
#' # Following are identical
#' !("Dinosaur" %in% names(iris))
#' "Dinosaur" %nin% names(iris)
NULL

#' @rdname operators
#' @export
Not = `!`

#' @rdname operators
#' @export
`%nin%` = function(x, y) !(match(x, y, nomatch = 0L) > 0L)
