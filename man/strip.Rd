% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strip.r
\name{strip}
\alias{strip}
\alias{strip_chart}
\alias{strip_chart_sensible}
\alias{strip_hist}
\alias{strip_heatmap}
\title{Strip plots}
\usage{
strip_chart(
  x,
  jitter = 0.1,
  offset = 0,
  group.names,
  cex.names = par("cex"),
  at = NULL,
  add = FALSE,
  xlim = NULL,
  ylim = NULL,
  ylab = NULL,
  xlab = NULL,
  main = "",
  axes = TRUE,
  frame.plot = axes,
  las = par("las"),
  log = "",
  pch = par("pch"),
  col = par("fg"),
  bg = par("bg"),
  lwd = par("lwd"),
  cex = par("cex"),
  mean = FALSE,
  width = jitter,
  col.mean = par("fg"),
  lwd.mean = par("lwd"),
  grid = FALSE,
  col.grid = "lightgray",
  lwd.grid = par("lwd"),
  lty.grid = 2,
  ...
)

strip_chart_sensible(
  x,
  col = alpha("black", 0.3),
  bg = NA,
  jitter = 0.1,
  width = jitter * 1.2,
  pch = 19,
  las = 1,
  mean = TRUE,
  lwd = par("lwd"),
  lwd.mean = 3,
  col.mean = alpha("red", 0.7),
  xlab = "",
  ylab = "",
  ...
)

strip_hist(
  x,
  breaks = "Sturges",
  col = "black",
  offset = 0,
  add = FALSE,
  las = 1,
  gap = 0,
  width = 0.5,
  border = FALSE,
  main = ""
)

strip_heatmap(
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
)
}
\arguments{
\item{x}{a numeric vector or a list of numeric vectors
if \code{x} is a list, each element of a list is assumed to be a different category/class
and plotted separately, this is the most useful function of \code{strip} functions}

\item{jitter}{numeric value giving the amount of random noise (jitter) in the x-axis}

\item{offset}{numeric value giving a fixed offset from the x-coordinates}

\item{group.names}{character vector specifying group labels if x is a list}

\item{cex.names}{numeric value of a character expansion (cex) for group labels}

\item{at}{a numeric vector giving x coordinates for x, by default 1 to n is used}

\item{add}{logical, if \code{TRUE}, plot is drawn into existing window}

\item{xlim, ylim}{numeric vector of length two giving x and y limits of the plot}

\item{xlab, ylab}{character value specifying labels for the x and y axis respectively}

\item{main}{character value, the main plot title (on top), see \code{\link[graphics:title]{graphics::title()}}}

\item{axes}{logical, if \code{FALSE} axes are not drawn}

\item{frame.plot}{logical, if \code{FALSE} the plot frame is not drawn}

\item{las}{numeric value, stlye of axis labels, see \code{\link[graphics:par]{graphics::par()}}}

\item{log}{character string specifying which axis is logarithmic}

\item{pch, col, bg, lwd}{graphical options for plotted elements see \code{\link[graphics:par]{graphics::par()}},
can be specified either as a single value, a vector, or a list with similar structure as \code{x}}

\item{cex}{numerical value of a character expansion for plotted elements}

\item{mean}{logical, if \code{TRUE} a mean of each category is plotted}

\item{width}{numeric value specifying the width of the mean bar}

\item{col.mean, lwd.mean}{graphical options for mean, see \code{\link[graphics:par]{graphics::par()}}}

\item{grid}{logical, if \code{TRUE} a grid of horizontal lines is plotted}

\item{col.grid, lwd.grid, lty.grid}{graphical options for grid, see \code{\link[graphics:par]{graphics::par()}}}

\item{...}{other graphical parameters passed to functions}

\item{breaks}{style of breaks for histogram, see \code{\link[graphics:hist]{graphics::hist()}}}

\item{gap}{numeric value, a gap between subsequent histogram rectangles}

\item{border}{a style of border for histogram rectangles}
}
\description{
Various strip functions for plotting one-dimensional charts.
}
\details{
These plots enable convenient way to plot either multiple variables at once,
or a single variable split according to some factor.
\itemize{
\item \code{strip_chart()} plots a dot-plot or a single-dimensional scatterplot
\item \code{strip_hist()} plots a histogram
\item \code{strip_heatmap()} plots a single-dimensional heatmap.
\item \code{strip_chart_sensible()} comes with some useful defaults.
}
}
\examples{

# Plot all features except Species
strip_chart(iris[-5])

# Plot Sepal.Length split by Species
split(iris$Sepal.Length, iris$Species) |> strip_chart()

# Plot both Sepal.Length and Petal.Length side by side
sepal_length = split(iris$Sepal.Length, iris$Species)
petal_length = split(iris$Petal.Length, iris$Species)
strip_chart_sensible(
  sepal_length, offset = -0.2, col = "green", grid = TRUE,
  ylim = range(iris$Sepal.Length, iris$Petal.Length)
  )
strip_chart_sensible(petal_length, offset = 0.2, add = TRUE, col = "blue")

# Histograms
strip_hist(iris[1:4], gap = 0.1)

# Heatmaps
strip_heatmap(iris[1:4], gap = 0.1, width = 0.5, border = "lightgray")
}
\seealso{
\code{\link[graphics:stripchart]{graphics::stripchart()}} for a base version of \code{strip_chart()},
\code{\link[graphics:boxplot]{graphics::boxplot()}} for the classical one-dimensional plots
}
