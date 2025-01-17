TEST_SET("normalize and standardize", {

    control = data.frame(
        "foo" = c(1, 2, 3), # mean = 2
        "bar" = c(5, 6, 7),
        "baz" = rep("control", 3)
        )

    treatment = data.frame(
        "foo" = c(2, 3, 4),
        "bar" = c(5, 6, 7),
        "baz" = rep("treatment", 3)
        )
    data = rbind(control, treatment)
    nm = c(-1,0,1)

    ###############################################################
    # test select
    ###############################################################
    TEST(identical(normalize(control["foo"]), data.frame("foo" = nm) ))
    TEST(identical(normalize(control[c("foo", "bar")]), data.frame("foo" = nm, "bar" = nm)))
    TEST(identical(
        normalize(control, "foo"),
        data.frame("foo" = nm, "bar" = control$bar, "baz" = control$baz)
        ))
    TEST(identical(
        normalize(control, c("foo", "bar")),
        data.frame("foo" = nm, "bar" = nm, "baz" = control$baz)
        ))

    ###############################################################
    # test "where"
    ###############################################################
    TEST(identical(
        normalize(control, "foo", baz == "control"),
        data.frame("foo" = nm, "bar" = control$bar, "baz" = control$baz)
        ))
    TEST_ERROR(normalize(control, "foo", baz == "treatment"))
    TEST(identical(
        normalize(data, c("foo", "bar"), baz == "control"),
        data.frame("foo" = data$foo - 2, "bar" = data$bar - 6, "baz" = data$baz)
        ))

    ###############################################################
    # test non-standard evaluation
    ###############################################################
    item = "control"
    TEST(normalize(control, foo) |> select("foo") |> mean() == 0)
    TEST(normalize(control, foo, baz == item) |> select("foo") |> mean() == 0)
    TEST(identical(normalize(data, foo, baz == item) |> select("foo"), data$foo - 2))

    # test different FUN
    TEST(identical(
        normalize(control["foo"], FUN = `/`),
        data.frame("foo" = control$foo / 2)
        ))

    ###############################################################
    # test STATS
    ###############################################################
    TEST(identical(
        normalize(control["foo"], STATS = `sum`),
        data.frame("foo" = control$foo - sum(control$foo))
        ))

    TEST(identical(
        normalize(control["foo"], STATS = 1),
        data.frame("foo" = control$foo - 1)
        ))

    TEST(identical(
        normalize(control["foo"], STATS = c(1, 5)),
        data.frame("foo" = control$foo - 1)
        ))

    TEST(identical(
        normalize(control[c("foo", "bar")], STATS = c(1, 5)),
        data.frame("foo" = control$foo - 1, "bar" = control$bar - 5)
        ))

    TEST(identical(
        normalize(control[c("foo", "bar")], STATS = 5),
        data.frame("foo" = control$foo - 5, "bar" = control$bar - 5)
        ))

    ###############################################################
    # test SPLIT
    ###############################################################
    TEST(identical(
        normalize(data, c("foo", "bar"), STATS = 0, SPLIT = ~ baz),
        data
        ))

    TEST(identical(
        normalize(data, c("foo", "bar"), SPLIT = ~ baz),
        data.frame(foo = rep(nm, 2), bar = rep(nm, 2), baz = data$baz)
        ))

    TEST(identical(
        normalize(data, "foo", baz == "control", SPLIT = ~bar),
        data.frame(foo = rep(c(0,1), each=3), bar = data$bar, baz = data$baz)
        ))

    TEST(identical(
        normalize(data, "foo", SPLIT = ~bar, STATS = list(1, 2, 3)),
        data.frame(foo = rep(c(0,1), each=3), bar = data$bar, baz = data$baz)
        ))

    ###############################################################
    # test standardize
    ###############################################################
    TEST(all.equal(
        normalize(data, "foo", rep(TRUE, 6), FUN=`-`, STATS=`mean`, na.rm=TRUE)$foo |> mean(),
        0))
    TEST(all.equal(
        normalize(data, "foo", rep(TRUE, 6), FUN=`/`, STATS=`sd`, na.rm=TRUE)$foo |> sd(),
        1))
    TEST(all.equal(
        normalize(data, "foo", FUN=`-`, STATS=`mean`, na.rm=TRUE)$foo |> mean(),
        0))
    TEST(all.equal(
        normalize(data, "foo", FUN=`/`, STATS=`sd`, na.rm=TRUE)$foo |> sd(),
        1))
    TEST(all.equal(
        standardize(data, "foo")$foo |> mean(),
        0))
    TEST(all.equal(
        standardize(data, "foo")$foo |> sd(),
        1))

    TEST(all.equal(
        standardize(data, "foo", baz == "control")$foo |> mean(),
        mean(data$foo - mean(control$foo))
        ))
    TEST(all.equal(
        standardize(data, "foo", baz == "control")$foo |> sd(),
        sd(data$foo / sd(control$foo) )
        ))
    })
