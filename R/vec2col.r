#' Convert a vector to colors
#'
#' Generate a vector of colours where each unique element of the input vector will be assigned
#' a colour value.
#'
#' If the `x` is a factor, then levels are used for categories.
#' This allows consistent colour generation across subsets.
#' Otherwise, sorted unique elements are used instead.
#'
#' The parameters `palette` and `type` determine which colour generating method is used.
#' The `hcl`, `palette`, and `colorRamp` call [grDevices::hcl.colors()],
#' [grDevices::palette.colors()], and [grDevices::colorRampPalette()] respectively with
#' `palette` as its first argument specifying the name of the palette (for `hcl` and `palette`)
#' or the vector of colours from which palette is generated (for `colorRamp`).
#' If the `type` is either `color` or `colours`, the `palette` is expected to be a vector
#' of colours which is then directly used. The vector is recycled as required.
#'
#' @param x an input vector with preferably ordinal or categorical values
#' @param palette a name of a palette, either a name (for type `hcl` or `palette`),
#' or a vector of colours (for type `colorRamp`, `colors` or `colours`).
#' @param type type of palette, see details
#' @param ... other parameters passed to the palette functions
#' @return a named vector of colours of the same length as `x`
#'
#' @seealso
#' [grDevices::hcl.colors()], [grDevices::palette.colors()], [grDevices::colorRampPalette()]
#' for colour-generating functions used internally.
#'
#' @examples
#' # Convenient way to generate colours for dataset
#' vec2col(iris$Species, "Roma")
#'
#' # For < 8 categories, `palette` is more divergent
#' vec2col(iris$Species, "Set 2", "palette")
#'
#' # When Passing factor, levels are used
#' mtcars$carb |> unique() # no 5 or 7
#' carb = factor(mtcars$carb, levels = 1:8)
#' vec2col(carb, "Set 2", "palette")
#'
#' # E.g., smooth colour transition despite incomplete representation
#' col = vec2col(carb, c("blue", "red"), "colorRamp")
#' plot(mtcars$cyl, mtcars$carb, col = col, pch = 19, cex = 2)
#'
#' @export
vec2col = function(
    x,
    palette,
    type = c("hcl", "palette", "colorRamp", "colors", "colours"),
    ...
    ){
    type = match.arg(type)

    ux = if(is.factor(x)) levels(x) else unique(x) |> sort()

    cols = switch(type,
        "hcl" = grDevices::hcl.colors(length(ux), palette, ...),
        "palette" = grDevices::palette.colors(length(ux), palette, ...),
        "colorRamp" = (grDevices::colorRampPalette(palette, ...))(length(ux)),
        "colors" = ,
        "colours" = rep_len(palette, length(ux))
        )

    if(is.null(names(cols)))
        names(cols) = ux

    cols[x]
    }
