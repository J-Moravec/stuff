TEST_SET("sort.data.frame sorts data.frames", {
    d = data.frame(
        "a" = 1:3,
        "b" = c("a", "b", "c"),
        "c" = 3:1
        )

    TEST(all_identical(d, sort(d), sort(d, by = 1), sort(d, by = "a")))
    TEST(all_identical(d[3:1,], sort(d, by = 3), sort(d, by = "c")))

    TEST_ERROR(sort(d, "b"))
    })
