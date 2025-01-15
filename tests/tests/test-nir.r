TEST_SET("nir properly rectangulate", {
    TEST(identical(nir(11), c(4, 4)))
    TEST(identical(nir(11, 3), c(3, 4)))
    TEST(identical(nir(11, col = 3), c(4, 3)))
    TEST(identical(nir(as.list(1:11), 3), c(3, 4)))
    })
