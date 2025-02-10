#' Divide an object into equal parts
#'
#' Divide an object such as vectors, lists, matrices or data.frames into an `k` number of parts.
#'
#' If the object of size `n` is divisible by `k` such that `n %% k = 0`,
#' the object is split into `k` parts of equal size of size `n %/% k`.
#' Otherwise, the first `n %% k` parts will have size `n %/% k + 1`.
#'
#' The object inheriting from `matrix` or `data.frame` can be divided along their
#' rows (`dim = 1`) or columns (`dim = 2`).
#' Arrays are currently unsupported and treated as vectors.
#'
#' @param x an object to be split
#' @param k number of objects
#' @param dim a dimension along which the division will be made, e.g.,
#' for `matrix` and `data.frame` 1 (default) divides along rows and 2 along columns
#' @param ... arguments to be passed to or from methods
#' @return a list of `k` objects derived from `x`, see details
#'
#' @seealso
#' [base::split()] for spliting an object according to factors
#'
#' @examples
#' l = letters[1:10]
#'
#' # perfect division
#' divide(l, 2)
#' divide(l, 5)
#'
#' # unequal division
#' divide(l, 3)
#' divide(l, 4)
#'
#' # list are vectors
#' divide(as.list(l), 3)
#'
#' # matrices and data.frames
#' divide(iris, 3)
#' divide(iris, 3, 2)
#' divide(as.matrix(iris), 3)
#'
#' arr = array(letters, c(2,2,2)) # array 2x2x2
#' identical(
#'   divide(arr, 3),
#'   divide(as.vector(arr), 3)
#'   )
#'
#' @export
divide = function(x, k, ...){
    UseMethod("divide")
    }


#' @rdname divide
#' @export
divide.default = function(x, k, ...){
    id = rep(seq_len(k), length.out = length(x)) |> sort()
    split(x, id)
    }


#' @rdname divide
#' @export
#' @method divide data.frame
divide.data.frame = function(x, k, dim = 1, ...){
    n = dim(x)[dim]
    id = rep(seq_len(k), length.out = n) |> sort()
    s = split(seq_len(n), id)
    lapply(s, \(y) .dimget(x, dim, y, drop = FALSE))
    }


#' @rdname divide
#' @export
#' @method divide matrix
divide.matrix = divide.data.frame


.dimget = function(x, dim, idx, drop = TRUE){
    d = rep(TRUE, length(dim(x))) |> as.list()
    d[[dim]] = idx
    do.call(`[`, c(list(x), d, drop = drop))
    }
