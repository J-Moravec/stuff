TEST_SET("alpha adds oppacity", {
    vt = c("red", "blue", "green")
    ve = adjustcolor(vt, alpha.f = 0.5)
    TEST(identical(alpha(vt, 0.5), ve))

    # preserve names
    nm = c("foo", "bar", "baz")
    TEST(identical(names(alpha(set_names(vt, nm), 0.5)), nm))
    })
