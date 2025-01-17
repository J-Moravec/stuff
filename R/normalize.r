#' Normalize data
#'
#' Perform data normalization by adjusting selected columns using a calculated statistics.
#' The statistics can be calculated only on a part of data.
#'
#' These functions are expanded and user-friendly variant of [base::scale()] for `data.frames`.
#'
#' `standardize` is a user-friendly [base::scale()] for `data.frames`.
#' It performs scaling and centering on selected columns against a subset specified by `where`.
#'
#' `normalize` is a more general version that allows specifying given statistics
#' such as [base::mean()] for centering or [stats::sd()] for scaling,
#' and operation such as subtraction or division respectively.
#' Like `standardize`, the statistics can be calculated only from specified subset of data
#' specified by `where`.
#'
#' Data normalization is a general term with a range of meaning. In the most general sense
#' data normalization is adjusting data, i.e., applying some function using calculated statistic
#' either provided or more commonly calculated from the data itself.
#' For instance, *centering* removes mean and *scalig* divides by standard deviation.
#'
#' A common problem might be to perform normalization against only part of data
#' such as normalizing your treatments against control and obtaining relative effect sizes.
#'
#' @param x a data.frame
#' @param select a subset of columns that is normalized
#' @param where a logical expression indicating rows from which is the statistics calculated
#' @param FUN a vectorized function of two arguments representing the application of statistics
#' (second argument) on the data (first argument), such as `-` or `/` for centering or scaling.
#' @param STATS a function that calculate single statistics
#' or alternatively a numeric vector with pre-calculated statistics for every selected
#' column, the vector is recycled as required
#' @param SPLIT a factor or a formula passed to [base::split()] that further splits `x`
#' into independently normalized subsets, see [base::split()].
#' @param ... further arguments passed to `STATS`
#' @param center perform centering
#' @param scale perform scaling
#' @return normalized data.frame
#'
#' @seealso
#' See [base::scale()] for standardizing (centering and scaling),
#' [base::sweep()] that allows centering and scaling with arbitrary statistics,
#' [base::split()] for dividing data into subset,
#' and [base::subset()] for selecting subset of data.
#'
#' @examples
#' normalize(iris, Sepal.Length)
#' normalize(iris, "Sepal.Length")
#' normalize(iris, c("Sepal.Length", "Sepal.Width"))
#'
#' # Normalize all iris columns except species against setosa
#' normalize(iris, setdiff(names(iris), "Species"), Species == "setosa")
#'
#' # Standardize all iris column except species against setosa
#' standardize(iris, setdiff(names(iris), "Species"), Species == "setosa")
#'
#' # Normalize Sepal.Length by dividing by "setosa" mean
#' normalize(iris, "Sepal.Length", Species == "setosa", FUN = `/`)
#'
#' # Normalize each species independently
#' normalize(iris, setdiff(names(iris), "Species"), SPLIT = ~ Species)
#'
#' @importFrom stats sd
#' @export
normalize = function(x, select, where, FUN = `-`, STATS = `mean`, SPLIT = NULL, ...){
    FUN = match.fun(FUN)
    if(is.character(STATS))
        STATS = match.fun(STATS)

    if(missing(select)){
        select = rep_len(TRUE, ncol(x))
        } else {
        nm = seq_along(x) |> as.list() |> stats::setNames(names(x))
        select = substitute(select) |> eval(nm, parent.frame())
        }

    if(missing(where)){
        where = rep_len(TRUE, nrow(x))
        } else {
        where = substitute(where) |> eval(x, parent.frame())
        if(!is.logical(where))
            stop("\"where\" must be logical")
        where = where & !is.na(where)

        if(sum(where) == 0)
            stop("\"where\" results in empty selection")
        }


    if(!is.null(SPLIT)){
        if(inherits(SPLIT, "formula"))
            SPLIT = .formula2varlist(SPLIT, x)

        y = split(x, SPLIT, drop = TRUE)
        where = split(where, SPLIT, drop = TRUE)
        STATS = if(is.list(STATS)) STATS else list(STATS)
        y = mapply(
            .normalize, y, where = where, STATS = STATS, SIMPLIFY = FALSE,
            MoreArgs = c(list(select = select, FUN = FUN), ...)
            )
        y = unsplit(y, SPLIT)
        y = y[row.names(x),]
        # fix conversion to character
        attr(y, "row.names") = attr(x, "row.names")
        return(y)
        }

    .normalize(x, select, where, FUN, STATS, ...)
    }


# internal function
#
# avoiding all the non-standard evaluation
.normalize = function(x, select, where, FUN, STATS, ...){
    if(is.function(STATS)){
        stats = x[where, select, drop = FALSE] |> lapply(STATS, ...)
        } else {
        stats = rep(STATS, length.out = length(select))
        }

    x[select] = Map(FUN, x[select], stats)
    x
    }


#' @rdname normalize
#' @export
standardize = function(x, select, where, SPLIT = NULL, center = TRUE, scale = TRUE){
    # center
    if(center)
        x = do.call(normalize, list(
            x = x,
            select = substitute(select),
            where = substitute(where),
            FUN = `-`,
            STATS = `mean`,
            SPLIT = SPLIT,
            na.rm=TRUE
            ))

    # scale
    if(scale)
        x = do.call(normalize, list(
            x = x,
            select = substitute(select),
            where = substitute(where),
            FUN = `/`,
            STATS = `sd`,
            SPLIT = SPLIT,
            na.rm=TRUE
            ))

    x
    }
