#' Make directory
#'
#' Make one or more directories.
#'
#' This function provides a different interface for `dir.create` that is more similar to
#' the Unix command `mkdir` (specifically `mkdir -p`). The two major differences
#' being a vectorization over `x` so unlike `dir.create` this functions accepts a vector.
#' The other difference being a test for the directory existence so a directory is created
#' only if it doesn't already exists.
#'
#' @param x one or more paths
#' @param recursive logical, should elements of the paths be created? If set to `FALSE` and the
#' directory `foo` doesn't exist, then `mkdir("foo/bar, recursive = FALSE)` would fail.
#' @param showWarnings logical, should the warning on failure be shown?
#' @param mode the mode to be used on Unix-alike, see `?dir.create` for more information.
#' @return A logical vector indicating if the operation succeeded for each of the directories
#' or if the directory already exists.
#'
#' @seealso
#' [base::dir.exists()] and [base::dir.create()] for testing existence and creation of directories.
#'
#' @examples
#' \dontrun{
#' # create multiple directories at once
#' mkdir(c("foo", "bar"))
#'
#' # Following are identical:
#' if(!dir.exist("foo/bar")
#'    dir.create("foo/bar", recursive = TRUE)
#'
#' mkdir("foo/bar")
#' }
#'
#' @export
mkdir = function(x, recursive = TRUE, showWarnings = TRUE, mode = "0777"){
    e = dir.exists(x)

    if(!all(e))
        e[!e] = unlist(lapply(
            FUN = dir.create,
            x[!e], showWarnings = showWarnings, recursive = recursive, mode = mode
            ))

    invisible(e)
    }
