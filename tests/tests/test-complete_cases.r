TEST_SET("complete_cases filters out NAs", {
    d = data.frame(
        "foo" = c(1, NA, 3),
        "bar" = c(NA, 2, 3)
        )

    TEST(identical(complete_cases(d), d[stats::complete.cases(d), ]))
    TEST(identical(complete_cases(d, 1), d[stats::complete.cases(d[1]), ]))
    TEST(identical(complete_cases(d, "foo"), d[stats::complete.cases(d["foo"]), ]))
    })
