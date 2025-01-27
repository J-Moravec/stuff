TEST_SET("rntc moves rownames to first column", {
    dft = data.frame(
        row.names = c("foo", "bar", "baz"),
        "a" = 1:3,
        "b" = 4:6
        )
    dfe = data.frame(
        "name" = c("foo", "bar", "baz"),
        "a" = 1:3,
        "b" = 4:6
        )
    dfe_nm = set_names(dfe, dfe[["name"]], 1)

    TEST(identical(rntc(dft), dfe))
    TEST(identical(rntc(dft, "biz"), rename(dfe, "name", "biz")))
    TEST(identical(rntc(dft, "biz", FALSE), rename(dfe_nm, "name", "biz")))
    TEST(identical(rntc(dft, remove = FALSE), dfe_nm))
    })
