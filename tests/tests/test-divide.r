TEST_SET("divide splits objects equally", {
    # vector
    TEST(identical(
        divide(letters[1:10], 3),
        list(
            "1" = c("a", "b", "c", "d"),
            "2" = c("e", "f", "g"),
            "3" = c("h", "i", "j")
            )
        ))

    # list is a vector
    TEST(identical(
        divide(as.list(letters[1:10]), 3),
        lapply(divide(letters[1:10], 3), as.list)
        ))

    # array is a vector
    arr = array(letters, c(2, 2, 2))
    TEST(identical(divide(arr, 3), divide(as.vector(arr), 3)))

    # data.frame is split along rows
    df = head(iris, n = 5)
    TEST(identical(
        divide(df, 3),
        list(
            "1" = df[1:2, ],
            "2" = df[3:4, ],
            "3" = df[5, ]
            )
        ))

    # unless dim is specified
    TEST(identical(
        divide(df, 3, 2),
        list(
            "1" = df[, 1:2],
            "2" = df[, 3:4],
            "3" = df[, 5, drop = FALSE]
            )
        ))


    # matrix is a data.frame
    mat = as.matrix(df)
    TEST(identical(
        divide(mat, 3),
        list(
            "1" = mat[1:2, ],
            "2" = mat[3:4, ],
            "3" = mat[5, , drop = FALSE]
            )
        ))
    })
