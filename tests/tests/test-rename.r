TEST_SET("rename and frename", {
    # test vector and data.frame, these cover most functionality
    # and differ in their `names()` and `names()<-` generics
    v = c("a" = 1, "b" = 2, "c" = 3)
    df = data.frame("a" = 1, "b" = 2, "c" = 3)
    old = c("a", "b"); new = c("foo", "bar")

    # basic functionality
    TEST(identical(rename(v, "a", "foo"), setNames(v, c("foo", "b", "c"))))
    TEST(identical(rename(v, c("a", "b"), c("foo", "bar")), setNames(v, c("foo", "bar", "c"))))
    TEST(identical(rename(v, old, new), setNames(v, c("foo", "bar", "c"))))

    TEST(identical(rename(df, "a", "foo"), setNames(df, c("foo", "b", "c"))))
    TEST(identical(rename(df, c("a", "b"), c("foo", "bar")), setNames(df, c("foo", "bar", "c"))))
    TEST(identical(rename(df, old, new), setNames(df, c("foo", "bar", "c"))))

    # errors
    TEST_ERROR(rename(v, old, "foo"))
    TEST_ERROR(rename(v, "a", new))
    TEST_ERROR(rename(df, old, "foo"))

    TEST_ERROR(rename(v, "d", "foo"))
    TEST_ERROR(rename(df, "d", "foo"))
    TEST_ERROR(rename(v, "d", "foo", strict = TRUE))

    # no error with strict = FALSE
    TEST(identical(rename(v, "d", "foo", strict = FALSE), v))
    TEST(identical(rename(df, "d", "foo", strict = FALSE), df))

    # frename -- what can go wrong?
    TEST(identical(frename(v, \(x) c("foo", "bar", "baz")), setNames(v, c("foo", "bar", "baz"))))
    TEST(identical(frename(v, paste0, c("a", "b", "c")), setNames(v, c("aa", "bb", "cc"))))

    # errors:
    TEST_ERROR(frename(x, \() "foo"))
    })
