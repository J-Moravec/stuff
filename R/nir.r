#' Nice rectangulate
#'
#' Returns a number of rows and columns that will fit the number of elements in `x`.
#'
#' The main purpose of this function is to find out how many columns or rows are required
#' to fit a certain number of elements. This is useful when plotting multiple facet
#' plots with `mfrow` and `mcol` (see [graphics::par()]).
#'
#' If neither `row` or `col` are specified, squared dimensions are returned.
#' Otherwise, if `row` or `col` are specified, rectangular dimensions are returned with
#' fixed pre-specified dimension.
#'
#' @param x a either a numeric vector of size one specifying the number of elements
#' or an object, typically a list, with a length
#' @param row,col the number of rows and columns respectively
#' @return a vector of size two specifying the number of rows and column the elements can fix in.
#'
#' @examples
#' nir(11, col = 3) # c(4, 3)
#' nir(11, row = 3) # c(3, 4)
#' nir(11) # c(4, 4)
#'
#' # plot each Iris species as separate facet
#' # with 2 columns per row
#' d = split(iris, ~ Species)
#' par(mfrow = nir(d, col = 2))
#' Map(
#'     d, names(d),
#'     f = \(x, name) plot(Sepal.Width ~ Sepal.Length, data = x, main = name)
#'     ) |> invisible()
#'
#' @export
nir = function(x, row = NULL, col = NULL){
    if(!is.numeric(x) || length(x) > 1)
        x = length(x)

    if(is.null(row) && is.null(col))
        return( rep(sqrt(x) |> ceiling(), 2) )

    if(is.null(row))
        return( c(ceiling(x/col), col) )

    if(is.null(col))
        return( c(row, ceiling(x/row)) )

    c(row, col)
    }


