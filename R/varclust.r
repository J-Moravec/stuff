#' Variable Clustering
#'
#' Perform variable clustering by calculating a similarity measure and then perform
#' hierarchical clustering to build a similarity tree of variables.
#'
#' Variable clustering is a simple method for feature selection, or at least dimensional
#' reduction for highly correlated variable spaces. It is also a highly-effective method
#' for visualizing correlation matrices with the ability to reduce large matrices to a small
#' clusters of related variables.
#'
#' `varclust()` performs variable clustering using correlation and a hierarchical clustering.
#'
#' `varcut()` cuts the tree from hierarchical clustering at some similarity threshold,
#' this defines clusters of highly related variables.
#' A single variable is selected out of each cluster, peforming a feature selection.
#'
#' `plot.varclust()` is a plotting method
#'
#' @param x for `varclust()` a data.frame, categorical variables should be provided as factors
#'   and are dummy-coded, for `varcut()` or `plot.varclust()` an object of class `varclust`
#' @param similarity a character string specifiying the chosen method for comparing variables,
#'   currently only correlation methods are supported
#' @param method a character string specifying the linking for the hierarchical clustering,
#'   the default "complete" makes the most sense as it creates continuous clusters
#' @param use a character string giving a method for computing correlation
#'   in the presence of missing variables, see [stats::cor()] for details
#' @param value a single numeric value giving the similarity threshold at which the tree
#'   from hierarchical clustering is cut to define clusters
#' @param ylab,main,at,digits graphical options for plot
#' @param val.col,val.lwd,val.lty graphical options for the threshold value line
#' @param ... other graphical options passed to [stats::plot.dendrogram()]
#' @return
#' for `varclust()` an object of the class `varclust` containing the calculated similarity matrix
#' and hierarchical clustering.
#' for `varcut()` a list with diagnostic information and selected variable names
#'
#' @examples
#' v = varclust(iris)
#' varcut(v)$select
#'
#' @export
varclust = function(
    x,
    similarity = c("pearson", "kendall", "spearman"),
    method = "complete", use = "na.or.complete"
    ){
    similarity = match.arg(similarity)

    # code factors as dummy variables
    x = Map(function(x, y){ if(is.factor(x)) dummy_code(x, name=y) else x},
        x, colnames(x)
        ) |> do.call(what=cbind) |> as.data.frame(row.names = rownames(x))

    y = stats::cor(x, method = similarity, use = "na.or.complete")
    clust = stats::hclust(stats::as.dist(1 - abs(y)), method = method)
    structure(
        list(
            "clust" = clust,
            "similarity" = y
            ),
        class = "varclust"
        )
    }



#' @rdname varclust
#' @export
varcut = function(x, value = 0.7){
    n = sum(x$clust$height > (1 - value)) + 1
    cluster = stats::cutree(x$clust, n)    

    nm = names(cluster) |> split(cluster)
    repres = lapply(nm, function(y, sim){colMeans(sim[y, y, drop=FALSE])}, sim = x$similarity)

    y = data.frame(
        "name" = nm |> unlist(),
        "cluster" = cluster[nm |> unlist()],
        "repres" = repres |> unlist(),
        "select" = lapply(repres, is_max) |> unlist(),
        row.names = NULL
        )

    list(
        "table" = y,
        "threshold" = value,
        "n_original" = nrow(y),
        "n_pruned" = sum(y$select),
        "select" = y$name[y$select]
        )
    }


is_max = function(x){
    y = logical(length(x))
    y[which.max(x)] = TRUE
    y
    }


dummy_code = function(x, name=NULL, sep = ":"){
    x = droplevels(x)
    levels = levels(x)
    y = matrix(0, nrow = length(x), ncol = length(levels), dimnames = list(NULL, levels))

    for(i in levels){
        y[,i] = ifelse(x == i, 1, 0)
        }

    colnames(y) = if(is.null(name)) levels else paste(name, levels, sep = sep)

    y
    }


#' @rdname varclust
#' @export
plot.varclust = function(
    x,
    ylab = "Similarity",
    main = "",
    at = c("range", "values"),
    digits = 1,
    value = 0.7,
    val.col = "red",
    val.lwd = 3,
    val.lty = 1,
    ...
    ){
    at = match.arg(at)

    plot(x$clust, ylab = ylab, xlab = "", sub = "", main = main, axes = FALSE, ...)

    if(!is.null(cut))
        graphics::abline(h = 1 - value, col = val.col, lwd = val.lwd, lty = val.lty)

    at = switch(at,
        "range" = x$clust$height |> range() |> pretty(),
        "values" = x$clust$height
        )
    graphics::axis(2, at = at, labels = (1 - at) |> format(digits = digits), las = 1, xpd = TRUE)
    }
