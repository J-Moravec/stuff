#' Does string start or end with another string?
#'
#' Determines if entries of `x` start or end with string (entries of)
#' `prefix` or `suffix` respecitvely, where strings are recycled to common length.
#'
#' This is wrapper around [base::startsWith()] and [base::endsWith()] respectively
#' with additional argument `value`, setting this to `TRUE` will return the matching elements
#' instead the `TRUE/FALSE/NA` values, similar to `grep(..., value = TRUE)`, see examples.
#'
#' @param x character vector whose starts or ends are considered
#' @param prefix,suffix character vector, typically a length of one
#' @param value a logical, if `TRUE`, vector containing the matching elements is returned
#' @return If `value = FALSE` (default) a logical vector of common length of `x` and
#' `prefix/suffix`, i.e., both vectors are recycled to the longer of two lengths.
#' If `value = TRUE`, a character vector resulting from subsetting `x` with the logical
#' vector described above.
#'
#' @seealso
#' [base::startsWith()] and [base::endsWith()] for the original implementation,
#' [base::grep()] and [base::grepl()] for regexp-based matching,
#' [base::substring()] for direct comparison of substrings,
#' [base::charmatch()] and [base::pmatch()] for partial string matching
#'
#' @examples
#' x = c("foo", "bar", "baz")
#'
#' # like base::startsWith()
#' identical(
#'     startsWith(x, "b"),
#'     base::startsWith(x, "b")
#'     )
#'
#' # return the matching value:
#' identical(
#'    startsWith(x, "b", TRUE),
#'    c("bar", "baz")
#'    )
#'
#' # These are equivalent
#' startsWith(x, "b", TRUE)
#' x[base::startsWith(x, "b")]
#' grep("^b.*", x, value = TRUE)
#'
#' # In data.frames, columns often have prefix/suffix
#' # and it is useful to get the names directly
#' startsWith(names(iris), "Sepal", TRUE)
#' endsWith(names(iris), "Width", TRUE)
#'
#' @export
startsWith = function(x, prefix, value = FALSE){
    if(value){
        x[base::startsWith(x, prefix)]
        } else {
        base::startsWith(x, prefix)
        }
    }


#' @rdname startsWith
#' @export
endsWith = function(x, suffix, value = FALSE){
    if(value){
        x[base::endsWith(x, suffix)]
        } else {
        base::endsWith(x, suffix)
        }
    }
