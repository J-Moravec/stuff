#' Set the names of an object
#'
#' This is a convenience function that sets the names of an object
#' and returns the object. It is a direct replacement of [stats::setNames()]
#' with an additional parameter specifying the dimension.
#' This allows for setting row names for `data.frame` as well as modifying
#' dimnames of matrices and arrays.
#'
#' By default, the function is a direct replacement of [stats::setNames()]
#' as a functional version of the `names<-()` 
#' However, if the `dim` is specified `setNames()` will instead replace the `dimnames`
#' of the object specified by `dim`. See examples.
#'
#' The side-effect of [base::dimnames()] is a conversion of all dimnames into a character vector.
#'
#' @param object an object for which generics [base::names<-()] or [base::dimnames<-()] exist
#' @param nm a character vector of names to assign to the object
#' @param dim an integer, dimension to be changed
#'
#' @seealso
#' [stats::setNames()] for the original function,
#' [base::names()], [base::dimnames()] for modifying names and dimnames
#'
#' @examples
#'
#' setNames(c(1, 2, 3), c("a", "b", "c"))
#' # Useful with pipes
#' c(1, 2, 3) |> setNames(c("a", "b", "c"))
#'
#' # alias
#' c(1, 2, 3) |> set_names(c("a", "b", "c"))
#'
#' # Setting row of matrix and data.frame
#' matrix(1:4, 2, 2) |>
#'    setNames(c("a", "b"), 1)
#' data.frame("foo" = 1:2, "bar" = 3:4) |>
#'    setNames(c("a", "b"), 1)
#'
#' @export
setNames = function(object = nm, nm, dim = NULL){
    if(is.null(dim)){
        names(object) = nm
        } else {
        dimnames(object)[[dim]] = nm
        }
    object
    }


#' @rdname setNames
#' @export
set_names = setNames
