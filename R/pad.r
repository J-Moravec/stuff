#' Pad a vector
#'
#' This function will par (extend) a vector with selected values to desired length.
#'
#' If `truncate = TRUE` (default), the vector `x` is truncated to guarantee that the returned
#' vector size is exactly `length`. This is done in standard way by taking the first `length`
#' values of the vector `x`.
#'
#' @param x a vector
#' @param length a desired length of output vector, if `length` is smaller than length of `x`,
#' the resulting vector is unchanged.
#' @param value a value used for padding the vector
#' @param side direction in which the vector will be padded,
#' either `left` where the values are added at the start of the vector,
#' or `right` where the values are added at the end of the vector
#' @param truncate if `TRUE` (default), truncate the vector to a given length, see details.
#' @return a vector of specified `length`
#'
#'
#' @examples
#'
#' x = 1:5
#' pad(x, 10)
#' pad(x, 10, side = "right")
#' pad(x, 3) # truncated
#' pad(x, 3, truncate = TRUE) # unchanged
#'
#' # type conversion:
#' pad(x, 10) # integer
#' pad(x, 10, "foo") # character
#'
#' # converted even if no padding is done
#' pad(x, 3, "foo") # character
#' pad(x, 3, "foo", truncate = FALSE) # character
#'
#' @seealso
#' [stuff::strpad()] for padding strings
#' @export
pad = function(x, length, value = NA, side = c("left", "right"), truncate = TRUE){
    side = match.arg(side)
    nx = min(length(x), length)

    if(truncate)
        x = rep_len(x, nx)
    xpad = rep_len(value, length - nx)

    switch(side,
        "left" = c(xpad, x),
        "right" = c(x, xpad)
        )
    }


#' Pad a character vector
#'
#' Pad a character vector to the same width (number of characters).
#'
#' If `truncate = TRUE` (default), the strings in `x` are truncated to the desired width
#' using `substring(x, 1, width)`.
#'
#' @param x a character vector
#' @param width a desired width, by default, the width of the largest string is used
#' (`max(nchar(x))`)
#' @param value a string, typically a single character, longer strings are allowed, but
#' won't produce desired width.
#' @param side direction in which the strings will be padded,
#' either `left` where the padding is added at the start of the string,
#' or `right` where the padding is added at the end of the string
#' @param truncate if `TRUE` (default), truncate the strings to a given length, see details.
#' @return a character vector of strings with specified `width`
#'
#' @examples
#' x = c("a", "aa", "aaa")
#' strpad(x)
#' strpad(x, 5)
#' strpad(x, side = "right")
#' strpad(x, value = "_")
#'
#' # truncation
#' strpad(x, 1)
#' strpad(x, 1, truncate = FALSE)
#'
#'
#' @seealso
#' [stuff::pad()] for padding vectors
#' [base::sprintf()] for C-style string formatting
#' [base::format()] and [base::formatC()] for a more general formatting functions
#'
#' @export
strpad = function(x, width = NULL, value = " ", side = c("left", "right"), truncate = TRUE){
    side = match.arg(side)
    x = as.character(x)
    nx = nchar(x)
    if(is.null(width)) width = max(nx)

    if(truncate){
        x = substring(x, 1, width)
        nx = nchar(x)
        }

    xpad = strrep(value, pmax(width - nx, 0))

    switch(side,
        "left" = paste0(xpad, x),
        "right" = paste0(x, xpad)
        )
    }
