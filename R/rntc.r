#' Row names to column
#'
#' Transform row names of a `data.frame` to first column.
#'
#' @param x a data.frame
#' @param name a character string specifying a name for the newly created column
#' @param remove logical, remove column names
#' @return a data.frame with row names transformed into the first column
#'
#' @examples
#' rntc(mtcars)
#'
#' # use "model" as a more appropriate label
#' rntc(mtcars, "model")
#'
#' # do not remove old row names
#' rntc(mtcars, remove = FALSE)
#'
#' @seealso
#' [base::rownames()] for setting or resetting row names,
#' or see the `row.names` argument in [base::data.frame()] for the construction
#' of `data.frames` with the rownames attribute.
#'
#' @export
rntc = function(x, name = "name", remove = TRUE){
    data.frame(
        "name" = rownames(x),
        x,
        row.names = if(remove) seq_len(nrow(x)) else rownames(x),
        check.names = FALSE
        ) |> rename("name", name)
    }
