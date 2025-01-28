TEST_SET("replace2 replaces values", {
    v = c("foo", "bar", "baz")

    TEST(identical(replace2(v, "bar", "spam"), c("foo", "spam", "baz")))
    TEST(identical(replace2(v, c("bar", "foo"), c("spam", "beans")), c("beans", "spam", "baz")))

    dft = data.frame("foo" = c(1, 2), "bar" = c(3, 4))
    dfe = data.frame("foo" = c(2, 3), "bar" = c(4, 5))
    TEST(identical(replace2(dft, 1:4, 2:5), dfe))

    TEST_ERROR(replace2(dft, c("foo", "bar"), "spam"))
    })
