TEST_SET("blend mixes colours correctly", {
    gray = rgb(0.5, 0.5, 0.5, 1)
    lightgray = rgb(0.75, 0.75, 0.75, 1)
    white = rgb(1, 1, 1, 1)

    TEST(identical(blend("black", "white"), gray))
    TEST(identical(blend("white", "black"), gray))
    TEST(identical(blend(c("black", "white")), gray))
    TEST(identical(blend(c("white", "black")), gray))

    TEST(identical(blend("black", "white", c(1, 1)), gray))
    TEST(identical(blend("black", "white", c(2, 2)), gray))

    TEST(identical(blend(c("black", "white"), "white"), c(gray, white)))
    TEST(identical(blend(c("black", "white"), "white", c(1, 3)), c(lightgray, white)))
    })
