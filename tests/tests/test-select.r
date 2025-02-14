TEST_SET("select, deselect and fill correctly subsets", {
    d = data.frame(a = 1L, b = 2L, c = 3L)
    v = (seq_along(letters) |> setNames(letters))[1:3]
    l = as.list(v)

    # select on data.frame
    for(o in list(d, v, l)){
        msg = paste0("is not TRUE for ", class(o))
        TEST(identical(select(o, "a"), o[["a"]]), msg)
        TEST(identical(select(o, "a", TRUE), o[["a"]]), msg)
        TEST(identical(select(o, "a", FALSE), o["a"]), msg)
        TEST(identical(select(o, c("a", "b")), o[c("a", "b")]), msg)

        TEST(identical(select(o, 1), o[[1]]), msg)
        TEST(identical(select(o, 1, TRUE), o[[1]]), msg)
        TEST(identical(select(o, 1, FALSE), o[1]), msg)
        TEST(identical(select(o, c(1, 2)), o[c(1, 2)]), msg)

        # deselect
        TEST(identical(deselect(o, "a"), o[c("b","c")]), msg)
        TEST(identical(deselect(o, c("a", "b")), o["c"]), msg) # won't drop
        TEST(identical(deselect(o, 1), o[2:3]), msg)
        TEST(identical(deselect(o, 1:2), o[3]), msg)

        # missing elements: index / name out of bound
        msg = paste0("does not signal an error for ", class(o))
        TEST_ERROR(select(o, "d"), msg)
        TEST_ERROR(select(o, c("c", "d")), msg)
        TEST_ERROR(select(o, 4), msg)
        TEST_ERROR(select(o, 3:4), msg)
        }

    for(o in list(d, v, l)){
        msg = paste0("is not TRUE for ", class(o))

        # For present elements, fill is like select
        TEST(identical(fill(o, "a"), select(o, "a")), msg)
        TEST(identical(fill(o, "a", TRUE), select(o, "a", TRUE)), msg)
        TEST(identical(fill(o, "a", FALSE), select(o, "a", FALSE)), msg)
        TEST(identical(fill(o, c("a", "b")), select(o, c("a", "b"))), msg)

        TEST(identical(fill(o, 1), select(o, 1)), msg)
        TEST(identical(fill(o, 1, TRUE), select(o, 1, TRUE)), msg)
        TEST(identical(fill(o, 1, FALSE), select(o, 1, FALSE)), msg)
        TEST(identical(fill(o, c(1, 2)), select(o, c(1, 2))), msg)
        }

    # data.frame
    TEST(identical(fill(d, "d"), NA))
    TEST(identical(fill(d, "d", FALSE), data.frame("d" = NA)))
    TEST(identical(fill(d, c("a", "d")), data.frame("a" = 1L, "d" = NA)))

    TEST(identical(fill(d, 5), NA))
    TEST(identical(fill(d, 5, FALSE), data.frame("V5" = NA)))
    TEST(identical(fill(d, c(1, 5)), data.frame("a" = 1L, "V5" = NA)))

    # vector
    TEST(identical(fill(v, "d"), NA_integer_))
    TEST(identical(fill(v, "d", FALSE), c("d" = NA_integer_)))
    TEST(identical(fill(v, c("a", "d")), c("a" = 1L, "d" = NA_integer_)))

    TEST(identical(fill(v, 5), NA_integer_))
    TEST(identical(fill(v, 5, FALSE), c("V5" = NA_integer_)))
    TEST(identical(fill(v, c(1, 5)), c("a" = 1L, "V5" = NA_integer_)))

    # list
    TEST(identical(fill(l, "d"), NULL))
    TEST(identical(fill(l, "d", FALSE), list("d" = NULL)))
    TEST(identical(fill(l, c("a", "d")), list("a" = 1L, "d" = NULL)))

    TEST(identical(fill(l, 5), NULL))
    TEST(identical(fill(l, 5, FALSE), list("V5" = NULL)))
    TEST(identical(fill(l, c(1, 5)), list("a" = 1L, "V5" = NULL)))
    })
