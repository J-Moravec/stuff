#' Rename object
#'
#' Rename object, such as columns of data.frame, using the [base::names()] generics.
#' Two interfaces are provided: `rename()` to modify matching names and `frename()`
#' renaming is performed using user-provided function.
#'
#' `rename()` will rename the names of `x` that match the `old` vector with `new` vector.
#' The `old` and `new` vectors must have the same length.
#'
#' `frename()` will rename the names of `x` using the function `f` by applying the `f`
#' on the names of `x` and assigning the return value back to the names.
#' The function `f` assumes that the names of `x` are the first argument.
#'
#' Compared to some other implementations, non-standard evaluation is not used.
#' This makes passsing old or new names with variables easier.
#'
#' @param x an object that is to be renamed, generally any object for which the generic `names()`
#' and `names()<-` exist. This means that for `data.frame`, column names are modified.
#' @param old a character vector of names to be replaced with `new`
#' @param new a character vector of new names replacing `old`
#' @param strict a logical, if `FALSE` elements of `old` that do not have matching value in
#' `names(x)` are ignored. Otherwise, an error is signaled.
#' @param f a function with `names(x)` as its first unnamed argument (see examples)
#' @param ... other arguments passed to `f`
#'
#' @examples
#'
#' rename(mtcars, "hp", "horsepower")
#' rename(mtcars, c("hp", "cyl"), c("horsepower", "cylinders"))
#' old = c("hp", "cyl"); new = c("horsepower", "cylinders")
#' rename(mtcars, old, new)
#'
#' # These would throw error:
#' \dontrun{
#' rename(mtcars, "hp", c("horsepower", "cylinders")) # unequal length
#' rename(mtcars, "bp", "horsepower") # bp not in names(mtcars)
#' }
#'
#' # with strict = FALSE: bp ignored here, hp still renamed
#' rename(mtcars, c("bp, "hp"),
#'        c("foo, "horsepower"), strict = FALSE)
#'
#'
#' # first argumnet passed to `f` is names(x)
#' frename(mtcars, f = paste0, "_foo")
#'
#' # named arguments allow more precision
#' frename(mtcars, f = gsub, pattern = "[aeiou]", replacement = "_")
#'
#' @export
rename = function(x, old, new, strict = TRUE){
    stopifnot(length(old) == length(new))

    match = match(old, names(x))
    if(strict && anyNA(match))
        stop("Some names not in object:", to.string(old[is.na(match)]))

    names(x)[match[!is.na(match)]] = new[!is.na(match)]

    x
    }

#' @rdname rename
#' @export
frename = function(x, f, ...){
    names(x) = f(names(x), ...)

    x
    }

