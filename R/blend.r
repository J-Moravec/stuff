#' Blend colors in RGB space
#'
#' Blend together two or more colours in RGB space by averaging the colours in specified ratio.
#'
#' If only `x` is specified, all colours in `x` are averaged and a single colour is returned.
#' If both `x` and `y` are specified, they are averaged element-wise and vector of the same
#' length as the longer of `x` and `y` is returned.
#'
#' @param x,y a vector of colours specified either by name or in a exadecimal form, see details
#' @param ratio a ratio in which colours are blended
#' @return If `y = NULL`, a single value resulting from blending elements of `x`.
#' If both `x` and `y` are specified, then a vector of the same length as the longer of `x` and `y`.
#'
#' @seealso
#' [grDevices::adjustcolor] for adjusting a vector of colours.
#'
#' @examples
#' # Mix two colors in 1:1 ratio:
#' blend("black", "white")
#' blend(c("black", "white"))
#'
#' # Mix two colours in 5:1 ratio:
#' blend("black", "white", 5)
#' blend(c("black", "white"), ratio = 5)
#'
#' # Ratio is extended with 1 as required:
#' blend(c("red", "green", "blue"))
#' blend(c("red", "green", "blue"), ratio = 1)
#' blend(c("red", "green", "blue"), ratio = c(1, 1))
#' blend(c("red", "green", "blue", ratio = c(1, 1, 1)))
#'
#' # Ratio 1:1 and 2:2 etc. are identical
#' blend("black", "white", c(1, 1))
#' blend("black", "white", c(2, 2))
#'
#' # Make a vector of colours a little lighter
#' col = palette.colors(5, "Set 1")
#' blend(col, "white", c(2, 1))
#' blend("white", col, c(1, 2))
#'
#' @export
blend = function(x, y = NULL, ratio = 1){
    .blend = function(x, ratio = 1){
        ratio = pad(ratio, length(x), 1, "right")
        ratio = ratio / sum(ratio)

        rgb = grDevices::col2rgb(x, alpha=TRUE)/255
        do.call(grDevices::rgb, as.list(ratio %*% t(rgb)))
        }

    if(is.null(y))
        return(.blend(x, ratio = ratio))

    z = cbind(x, y)
    apply(z, 1, blend, ratio = ratio)
    }
