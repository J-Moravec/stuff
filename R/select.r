#' Select and deselect elements
#'
#' Pipe-friendly subsetting functions for vectors, lists, and data.frames.
#'
#' `select()` will subset a `vector`, `list` or a `data.frame` based on character or
#' numeric indices `i`. In many ways it behaves like `[`, but it is safer in the sense
#' that non-existing indices will always throw error (`[` returns `NA` instead)
#'
#' `deselect()` is the opposite of `select()`, it removes elements specified by `i`
#' from the object. Non-existing elements are ignored.
#'
#' `fill()` is a variant of `select()` that works with non-existing elements as well.
#' If `i` is in `x`, it the output is identical to `select()`, otherwise it fills
#' the missing elements with `NA` (for vectors or data.frames) or `NULL` (for lists).
#'
#' @param x a vector, list, data.frame, or other object for which `[` and `[[` are defined.
#' @param i a numeric index or a character vector if `x` is named;
#' an error is reported if the index `i` is out of bound or the name is not in the vector
#' @param drop drop extra dimensions, relevant only for certain objects and only for `select`,
#' e.g., if `x` is a data.frame and `length(x) == 1`, a vector is returned as if `[[` was called.
#' `deselect` should never drop dimensions, if you want to deselect all but one columns,
#' just select that column.
#' @return an array-like object derived by subsetting `x`
#'
#' @examples
#' select(iris, "Species")
#'
#' # Compare:
#' iris |> getElement("Species")
#' iris |> (\(x) x[["Species"]])()
#' iris |> select("Species")
#'
#' # For multiple elements/columns
#' iris |> (\(x) x[c("Sepal.Width", "Sepal.Length")])()
#' iris |> select(c("Sepal.Width", "Sepal.Length"))
#'
#' # Won't work:
#' # iris |> (\(x) x[[c("Sepal.Width", "Sepal.Length")]])()
#' # iris |> getElement(c("Sepal.Width", "Sepal.Length"))
#'
#' # No non-standard evaluation
#' try(iris |> select(Species))
#' columns = c("Sepal.Width", "Sepal.Length")
#' iris |> select(columns)
#'
#' # Don't drop columns:
#' iris |> select("Species", drop = FALSE)
#'
#' # Remove columns
#' iris |> deselect("Species")
#' iris |> deselect(c("Sepal.Width", "Sepal.Length"))
#'
#' @seealso
#' [base::Extract] and [base::subset()]
#'
#' @export
select = function(x, i, drop = TRUE){
    if(!is.numeric(i) && !is.character(i))
        stop("Invalid index value, must be numeric or character")

    if(is.numeric(i) && any(i > length(x)))
        stop("Undefined columns selected.")

    if(is.character(i) && any(!i %in% names(x)))
        stop("Undefined columns selected.")

    if(length(i) == 1 && drop){
        x[i][[1]]
        } else {
        x[i]
        }
    }


#' @rdname select
#' @export
deselect = function(x, i){
    if(is.character(i)){
        i = match(i, names(x))
        }

    x[-i]
    }


#' @rdname select
#' @export
fill = function(x, i, drop = TRUE){
    if(!is.numeric(i) && !is.character(i))
        stop("Invalid index value, must be numeric or character")


    if(is.character(i)){
        s = i[!i %in% names(x)]
        x[s] = if(inherits(x, "list")) list(NULL) else NA
        }

    if(is.numeric(i)){
        n = length(x)
        s = i > n
        if(any(s)){
            m = seq(n + 1, max(i))
            x[m] = if(inherits(x, "list")) list(NULL) else NA
            names(x)[m] = paste0("V", m)
            }
        }

    if(length(i) == 1 && drop){
        x[i][[1]]
        } else {
        x[i]
        }
    }
