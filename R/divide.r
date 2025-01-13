#' Divide an object into equal parts
#'
#' Divide an object such as vectors, lists, or data.frames into an `k` number of parts.
#'
#' If the object of size `n` is divisible by `k` such that `n %% k = 0`,
#' the object is split into `k` parts of equal size of size `n %/% k`.
#' Otherwise, the first `n %% k` parts will have size `n %/% k + 1`.
#'
#' Currently, the only non-vector object that is supported are data.frames,
#' which are split along their rows.
#' Matrices and arrays are treated as vectors.
#'
#' @param x an object to be split
#' @param k number of objects
#' @param ... arguments to be passed to or from methods (currently unused)
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
#' # data.frame
#' divide(iris, 3)
#'
#' # matrices and arrays are treated as vectors
#' mat = as.matrix(head(iris))
#' identical(
#'   divide(mat, 3),
#'   divide(as.vector(mat), 3)
#'   )
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
    id = rep(seq_len(k), length.out=length(x)) |> sort()
    split(x, id)
    }


#' @rdname divide
#' @export
#' @method divide data.frame
divide.data.frame = function(x, k, ...){
    id = rep(seq_len(k), length.out=nrow(x)) |> sort()
    split(x, id)
    }
