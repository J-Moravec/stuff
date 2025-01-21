test_mkdir = function(x, y = x){
    # short curcuit
    TEST(all(mkdir(x)) && all(dir.exists(x)))
    dir.remove(y)
    }


TEST_SET("mkdir creates directories", {
    tmp = tempdir()

    test_mkdir(file.path(tmp, "foo"))
    test_mkdir(file.path(tmp, "foo/bar"), file.path(tmp, "foo")) # recursive
    test_mkdir(file.path(tmp, c("foo", "bar"))) # vectorized
    })
