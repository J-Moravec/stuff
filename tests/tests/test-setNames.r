TEST_SET("setNames and set_names", {
    v = c("a" = 1, "b" = 2, "c" = 3)
    TEST(identical(setNames(c(1, 2, 3), letters[1:3]), v))
    TEST(identical(set_names(c(1, 2, 3), letters[1:3]), v))

    mt = matrix(1:4, 2, 2)
    me = matrix(1:4, 2, 2, dimnames = list(NULL, c("foo", "bar")))
    TEST(!identical(setNames(mt, c("foo", "bar")), me))
    TEST(identical(setNames(mt, c("foo", "bar"), 2), me))

    dft = data.frame("foo" = 1:2, "bar" = 3:4)
    dfe = dft
    rownames(dfe) = c("a", "b")
    # dimnames
    TEST(identical(names(setNames(dft, c("f", "b"), 2)), names(setNames(dft, c("f", "b")))))
    TEST(identical(setNames(dft, c("a", "b"), 1), dfe))
    })
