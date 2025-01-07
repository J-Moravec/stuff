# stuff: A collection of utility functions

`stuff` contain various utility functions, or *stuff*, that are used across projects,
but do not form an unified toolset.

`stuff` is a mixture of text (`strpad`), and data processing (`normalize`) functions,
base graphics (`strip_chart`), and some utility functions to make piping easier (`select`).

## Installation

Use your favorite of `pak`, `remotes`, `mpd`, or `base::install.packages(repo = NULL)`

Such as using the [mpd](https://github.com/J-Moravec/mpd/) package:

```r
mpd::install_github("J-Moravec/stuff")
```

## Dependencies

`stuff` is trying to be dependency free.

## Usage

```r
# use library
library(stuff)

# use the :: notation
stuff::strpad(c("a", "aa", "aaa"))
```
