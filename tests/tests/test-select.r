TEST_SET("select, deselect and fill correctly subsets", {
    d = data.frame(a = 1, b = 2, c = 3)
    v = (seq_along(letters) |> setNames(letters))[1:3]
    l = as.list(v)

    # select on data.frame
    for(o in list(d, v, l)){
        msg = paste0("is not TRUE for ", class(o))
        TEST(identical(select(o, "a"), o[["a"]]), msg)
        TEST(identical(select(o, "a", TRUE), o[["a"]]), msg)
        TEST(identical(select(o, "a", FALSE), o["a"]), msg)
        TEST(identical(select(o, c("a","b")), o[c("a", "b")]), msg)

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


    })
