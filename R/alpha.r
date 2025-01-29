#' Change colour opacity
#'
#' Change opacity of colours by modifying the alpha value (typically `[0,1]`).
#'
#' This is a shorthand for `adjustcolor(x, alpha.f = alpha)` which offers more knobs to modify.
#' Adding alpha to colours is useful for overploting multiple elements,
#' such as plotting multiple points with similar values.
#'
#' @param x vector of colours, in any format that `col2rgb()` accepts
#' @param alpha factor modifying the opacity alpha; typically in `[0,1]`
#' @return a color vector of the same length as `x` with requested alpha value
#'
#' @seealso
#' [grDevices::adjustcolor()]
#'
#' @examples
#' # These are identical:
#' grDevices::adjustcolor("red", alpha.f = 0.5)
#' alpha("red", 0.5)
#'
#' # Unlike adjustcolor, alpha preserves names
#' col = c("red", "blue", "green") |> setNames(c("foo", "bar", "baz"))
#' alpha(col, 0.5)
#'
#' @export
alpha = function(x, alpha){
    if(is.null(alpha) | anyNA(alpha)) return(x)
    set_names(grDevices::adjustcolor(x, alpha.f = alpha), names(x))
    }
