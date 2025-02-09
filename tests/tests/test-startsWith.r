TEST_SET("startsWith and endsWith", {
    x = c("foo", "bar", "baz")
    TEST(identical(base::startsWith(x, "b"), startsWith(x, "b")))
    TEST(identical(base::startsWith("foo", x), startsWith("foo", x)))

    TEST(identical(base::endsWith(x, "b"), endsWith(x, "b")))
    TEST(identical(base::endsWith("foo", x), endsWith("foo", x)))

    # value = TRUE
    TEST(identical(startsWith(x, "b", TRUE), c("bar", "baz")))
    TEST(identical(endsWith(x, "o", TRUE), "foo"))

    TEST(identical(startsWith("foo", x, TRUE), "foo"))
    TEST(identical(endsWith("foo", x, TRUE), "foo"))
    })
