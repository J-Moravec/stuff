TEST_SET("select and deselect correctly subsets", {
    x = data.frame(a = 1, b = 2, c = 3)

    TEST(identical(select(x, "a"), x$a))
    TEST(identical(select(x, "a", TRUE), x$a))
    TEST(identical(select(x, "a", FALSE), x["a"]))
    TEST(identical(select(x, c("a","b")), x[c("a","b")]))

    TEST(identical(select(x, 1), x[[1]]))
    TEST(identical(select(x, 1, TRUE), x[[1]]))
    TEST(identical(select(x, 1, FALSE), x[1]))
    TEST(identical(select(x, c(1, 2)), x[c(1,2)]))

    TEST(identical(deselect(x, "a"), x[c("b","c")]))
    TEST(identical(deselect(x, c("a", "b")), x["c"])) # won't drop
    TEST(identical(deselect(x, 1), x[2:3]))
    TEST(identical(deselect(x, 1:2), x[3]))

    TEST_ERROR(select(x, "d"))
    TEST_ERROR(select(x, c("c","d")))
    TEST_ERROR(select(x, 4))
    TEST_ERROR(select(x, 3:4))

    # abridged tests on vector
    x = seq_along(letters) |> setNames(letters)

    TEST(identical(select(x, "a"), 1L))
    TEST(identical(select(x, "a", FALSE), c("a" = 1L)))
    TEST(identical(select(x, 1), 1L))
    TEST(identical(select(x, c("a","b")), x[1:2]))

    # selecting non-existing elements for vectors returns NA
    TEST(identical(select(x, "aa"), NA_integer_))
    TEST(identical(select(x, 30), NA_integer_))
    })
