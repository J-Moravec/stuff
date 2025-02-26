#' @importFrom graphics par
NULL

#' Strip plots
#'
#' Various strip functions for plotting one-dimensional charts.
#'
#' These plots enable convenient way to plot either multiple variables at once,
#' or a single variable split according to some factor.
#'
#' * `strip_chart()` plots a dot-plot or a single-dimensional scatterplot
#' * `strip_hist()` plots a histogram
#' * `strip_heatmap()` plots a single-dimensional heatmap.
#'
#' * `strip_chart_sensible()` comes with some useful defaults.
#'
#' @param x a numeric vector or a list of numeric vectors
#' if `x` is a list, each element of a list is assumed to be a different category/class
#' and plotted separately, this is the most useful function of `strip` functions
#' @param jitter numeric value giving the amount of random noise (jitter) in the x-axis
#' @param offset numeric value giving a fixed offset from the x-coordinates
#' @param group.names character vector specifying group labels if x is a list
#' @param cex.names numeric value of a character expansion (cex) for group labels
#' @param at a numeric vector giving x coordinates for x, by default 1 to n is used
#' @param add logical, if `TRUE`, plot is drawn into existing window
#' @param xlim,ylim numeric vector of length two giving x and y limits of the plot
#' @param xlab,ylab character value specifying labels for the x and y axis respectively
#' @param main character value, the main plot title (on top), see [graphics::title()]
#' @param axes logical, if `FALSE` axes are not drawn
#' @param frame.plot logical, if `FALSE` the plot frame is not drawn
#' @param las numeric value, stlye of axis labels, see [graphics::par()]
#' @param log character string specifying which axis is logarithmic
#' @param pch,col,bg,lwd graphical options for plotted elements see [graphics::par()],
#' can be specified either as a single value, a vector, or a list with similar structure as `x`
#' @param cex numerical value of a character expansion for plotted elements
#' @param mean logical, if `TRUE` a mean of each category is plotted
#' @param width numeric value specifying the width of the mean bar
#' @param col.mean,lwd.mean graphical options for mean, see [graphics::par()]
#' @param grid logical, if `TRUE` a grid of horizontal lines is plotted
#' @param col.grid,lwd.grid,lty.grid graphical options for grid, see [graphics::par()]
#' @param breaks style of breaks for histogram, see [graphics::hist()]
#' @param gap numeric value, a gap between subsequent histogram rectangles
#' @param border a style of border for histogram rectangles
#' @param ... other graphical parameters passed to functions
#'
#' @seealso
#' [graphics::stripchart()] for a base version of `strip_chart()`,
#' [graphics::boxplot()] for the classical one-dimensional plots
#'
#' @examples
#'
#' # Plot all features except Species
#' strip_chart(iris[-5])
#'
#' # Plot Sepal.Length split by Species
#' split(iris$Sepal.Length, iris$Species) |> strip_chart()
#'
#' # Plot both Sepal.Length and Petal.Length side by side
#' sepal_length = split(iris$Sepal.Length, iris$Species)
#' petal_length = split(iris$Petal.Length, iris$Species)
#' strip_chart_sensible(
#'   sepal_length, offset = -0.2, col = "green", grid = TRUE,
#'   ylim = range(iris$Sepal.Length, iris$Petal.Length)
#'   )
#' strip_chart_sensible(petal_length, offset = 0.2, add = TRUE, col = "blue")
#'
#' # Histograms
#' strip_hist(iris[1:4], gap = 0.1)
#'
#' # Heatmaps
#' strip_heatmap(iris[1:4], gap = 0.1, width = 0.5, border = "lightgray")
#' @name strip
NULL


#' @rdname strip
#' @export
strip_chart = function(
    x,
    jitter = 0.1,
    offset = 0,
    group.names,
    cex.names = par("cex"),
    at = NULL,
    add = FALSE,
    xlim = NULL, ylim = NULL,
    ylab = NULL, xlab = NULL,
    main = "",
    axes = TRUE, frame.plot = axes, las = par("las"),
    log = "",
    pch = par("pch"), col = par("fg"), bg = par("bg"), lwd = par("lwd"), cex = par("cex"),
    mean = FALSE, width = jitter, col.mean = par("fg"), lwd.mean = par("lwd"),
    grid = FALSE, col.grid = "lightgray", lwd.grid = par("lwd"), lty.grid = 2,
    ...
    ){
    groups = x
    if(is.numeric(groups))
        groups = list(groups)
    if(!is.list(groups))
        stop("invalid first argument")

    n = length(groups)
    if (!n) stop("invalid first argument")

    if(!is.list(col)){
        col = as.list(rep_len(col, n))
        }

    if(!is.list(bg)){
        bg = as.list(rep_len(bg, n))
        }

    if(!is.list(lwd)){
        lwd = as.list(rep_len(lwd, n))
        }

    col.mean = rep_len(col.mean, n)

    if (!missing(group.names)){
        attr(groups, "names") = group.names
        } else if (is.null(attr(groups, "names"))) {
        attr(groups, "names") = seq_len(n)
        }

    if(is.null(at)){
        at = seq_len(n)
        } else if (length(at) != n) { 
            stop(
                gettextf("'at' must have length equal to the number %d of groups", n),
                domain = NA
                )
        }

    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)


    if (!add) {
        if(is.null(xlim)){
            xlim = c(1L, n)
            xlim = xlim + jitter * if(n == 1) c(-5, 5) else c(-2, 2)
            xlim = xlim + c(-abs(offset), +abs(offset))
            }

        if(is.null(ylim))
            ylim = range(unlist(groups, use.names = FALSE), na.rm = TRUE)

        graphics::plot.new()
        graphics::plot.window(xlim, ylim, log, ...)

        if (frame.plot) 
            graphics::box()

        if (axes) {
            if (n > 1L) 
              graphics::axis(1, at = at, labels = names(groups),
                             cex.axis = cex.names, las = las, ...)
            graphics::Axis(x, side = 2, las = las, ...)
        }
        if (is.null(ylab)) 
            ylab = deparse1(substitute(x))
        graphics::title(xlab = xlab, ylab = ylab, ...)
        graphics::title(main, ...)
    }

    if(grid)
        graphics::abline(v = seq_len(n), lwd = lwd.grid, lty = lty.grid, col = col.grid)

    csize = cex * graphics::xinch(graphics::par("cin")[1L])
    for (i in seq_len(n)) {
        x = groups[[i]]
        ccol = rep_len(col[[i]], length(x))
        bbg = rep_len(bg[[i]], length(x))
        llwd = rep_len(lwd[[i]], length(x))
        y = rep.int(at[i], length(x))
        y = y + stats::runif(length(y), -jitter, jitter) + offset
        graphics::points(y, x, col = ccol, bg = bbg, lwd = llwd,
            pch = pch[(i - 1L)%%length(pch) + 1L], cex = cex, 
            ...)

        if(mean){
            mu = if(is.null(x)) NA else mean(x, na.rm=TRUE)
            graphics::segments(
                i - width + offset,
                mu, i + width + offset,
                mu,
                lwd = lwd.mean,
                col = col.mean[i]
                )
            }

        }
    invisible()
    }


#' @rdname strip
#' @export
strip_chart_sensible = function(
    x,
    col = alpha("black", 0.3),
    bg = NA,
    jitter = 0.1, width = jitter*1.2,
    pch = 19, las = 1, mean = TRUE, lwd = par("lwd"),
    lwd.mean = 3,
    col.mean = alpha("red", 0.7),
    xlab = "", ylab = "",
    ...
    ){
    strip_chart(
        x,
        col = col,
        bg = bg,
        pch = pch,
        xlab = xlab,
        ylab = ylab,
        las = las,
        mean = mean,
        lwd = lwd,
        lwd.mean = lwd.mean,
        col.mean = col.mean,
        jitter = jitter,
        width = width,
        frame.plot = FALSE,
        ...
        )
    }


#' @rdname strip
#' @export
strip_hist = function(
    x,
    breaks = "Sturges",
    col = "black",
    offset = 0, add = FALSE, las = 1,
    gap = 0,
    width = 0.5,
    border = FALSE,
    main = ""
    ){

    x = if(is.list(x)) x else list(x)
    n = length(x)

    if(!is.list(col)){
        col = rep_len(col, n)
        col = as.list(col)
        }

    xx = unlist(x)
    xx = xx[is.finite(xx)]
    if(is.character(breaks)){
        breaks = match.arg(tolower(breaks), c("sturges", "fd", "freedman-diaconis", "scott"))
        breaks = switch(breaks,
            "sturges" = grDevices::nclass.Sturges(xx),
            "freedman-diaconis" = ,
            "fd" = grDevices::nclass.FD(xx),
            "scott" = grDevices::nclass.scott(xx),
            stop("unknown 'breaks' algorithm")
            )
        }

    nB = length(breaks)
    if(nB == 1){
        if (!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L) 
            stop("invalid number of 'breaks'")
        if(breaks > 1e+06){
            warning(gettextf("'breaks = %g' is too large and set to 1e6", breaks), domain = NA)
            breaks = 1000000L
            }

        breaks = pretty(range(xx), n = breaks, min.n = 1)
        nB = length(breaks)
        if(nB <= 1){
            stop(gettextf("hist.default: pretty() error, breaks=%s",
                 format(breaks)), domain = NA)
            }
        }
    rm(xx)


    if(!add){
        xlim = c(1L, n)
        xlim = xlim + c(-0.5, 0.5) + c(-abs(offset), +abs(offset))

        graphics::plot.new()
        graphics::plot.window(xlim = xlim, ylim = range(breaks))

        if (n > 1L) 
          graphics::axis(1, at = seq_len(n), labels = names(x), las = las)
        graphics::axis(2, at = breaks, labels = breaks, las = las)
        graphics::title(main)
        }


    for(i in seq_len(n)){
        counts = .bincode(x[[i]], breaks, TRUE, TRUE) |> tabulate(nB - 1)
        vals = counts / max(counts) * width

        y = data.frame(
            from = breaks[-length(breaks)],
            to = breaks[-1],
            vals = vals
            )
        y = subset(y, vals > 0)

        graphics::rect(
            i - y$vals/2 + offset, y$from + gap/2,
            i + y$vals/2 + offset, y$to - gap/2,
            border = border, col = col[[i]]
            )
        }
    }


#' @rdname strip
#' @export
strip_heatmap = function(
    x,
    breaks = "Sturges",
    col = c("white", "black"),
    gap = 0,
    width = 1,
    offset = 0,
    add = FALSE,
    las = par("las"),
    border = FALSE,
    main = ""
    ){
    x = if(is.list(x)) x else list(x)
    n = length(x)

    xx = unlist(x)
    xx = xx[is.finite(xx)]
    if(is.character(breaks)){
        breaks = match.arg(tolower(breaks), c("sturges", "fd", "freedman-diaconis", "scott"))
        breaks = switch(breaks,
            "sturges" = grDevices::nclass.Sturges(xx),
            "freedman-diaconis" = ,
            "fd" = grDevices::nclass.FD(xx),
            "scott" = grDevices::nclass.scott(xx),
            stop("unknown 'breaks' algorithm")
            )
        }

    nB = length(breaks)
    if(length(breaks) == 1){
        if (!is.numeric(breaks) || !is.finite(breaks) || breaks < 1L) 
            stop("invalid number of 'breaks'")
        if(breaks > 1e+06){
            warning(gettextf("'breaks = %g' is too large and set to 1e6", breaks), domain = NA)
            breaks = 1000000L
            }

        breaks = pretty(range(xx), n = breaks, min.n = 1)
        nB = length(breaks)
        if(nB <= 1){
            stop(gettextf("hist.default: pretty() error, breaks=%s",
                 format(breaks)), domain = NA)
            }
        }
    rm(xx)


    if(!add){
        xlim = c(1L, n)
        xlim = xlim + c(-0.5, 0.5) + c(-abs(offset), +abs(offset))

        graphics::plot.new()
        graphics::plot.window(xlim = xlim, ylim = range(breaks))

        if (n > 1L) 
          graphics::axis(1, at = seq_len(n), labels = names(x), las = las)
        graphics::axis(2, at = breaks, labels = breaks, las = las)
        graphics::title(main)
        }


    for(i in seq_len(n)){
        counts = .bincode(x[[i]], breaks, TRUE, TRUE) |> tabulate(nB - 1)
        vals = counts / max(counts)
        cols = grDevices::colorRamp(col)(vals) |> grDevices::rgb(maxColorValue=255)

        graphics::rect(
            i - width/2, breaks[-length(breaks)] + gap/2,
            i + width/2, breaks[-1] - gap/2,
            border = border, col = cols
            )
        }
    }
