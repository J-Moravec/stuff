TEST_SET("pad correctly pads a vector", {
    TEST(identical(pad(1:3, 5), c(NA, NA, 1:3)))
    TEST(identical(pad(1:3, 5, side = "right"), c(1:3, NA, NA)))
    TEST(identical(pad(1:3, 2), 1:2))
    TEST(identical(pad(1:3, 5, "foo"), c("foo", "foo", 1:3)))
    TEST_ERROR(pad(1:3, 5, NULL))
    })


TEST_SET("strpad correctly pads a character vector", {
    x = c("a", "aa", "aaa")

    TEST(identical(strpad(x), c("  a", " aa", "aaa")))
    TEST(identical(strpad(x, 4), c("   a", "  aa", " aaa")))
    TEST(identical(strpad(x, side = "right"), c("a  ", "aa ", "aaa")))
    TEST(identical(strpad(x, value = "_"), c("__a", "_aa", "aaa")))
    TEST(identical(strpad(x, 1), c("a", "a", "a")))
    TEST(identical(strpad(x, 1, truncate = FALSE), x))
    })
