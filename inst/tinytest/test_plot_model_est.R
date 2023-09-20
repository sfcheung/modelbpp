if (interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

    suppressMessages(library(lavaan))
    suppressMessages(library(igraph))

    mod <-
    "
    m1 ~ x
    m2 ~ m1
    y ~ m2
    "

    fit <- sem(mod,
              dat_serial_4_weak,
              fixed.x = TRUE)

    out1 <- model_set(fit,
                      df_change_add = 1,
                      df_change_drop = 1,
                      must_not_add = c("m1~~y",
                                      "m1~y"),
                      progress = FALSE,
                      parallel = FALSE)
    g1 <- model_graph(out1)

    chk_g1 <- structure(c("drop: m1~x", "drop: m2~m1", "drop: y~m2", "original",
    "original", "original", "original", "original", "original", "add: m2~x",
    "add: y~m1", "add: y~x"), dim = c(6L, 2L))

    expect_identical(as_edgelist(g1),
                      chk_g1,
                      info = "model_graph: df diff = 1")

    out2 <- model_set(fit,
                      df_change_add = 2,
                      df_change_drop = 2,
                      must_not_add = c("m1~~y",
                                      "m1~y"),
                      progress = FALSE,
                      parallel = FALSE)
    g2 <- model_graph(out2)

    chk_g2 <- structure(c("add: m2~x", "add: m2~x", "add: y~m1", "add: y~m1",
    "add: y~x", "add: y~x", "drop: m1~x", "drop: m2~m1", "drop: y~m2",
    "drop: m1~x;m2~m1", "drop: m1~x;m2~m1", "drop: m1~x;y~m2", "drop: m1~x;y~m2",
    "drop: m2~m1;y~m2", "drop: m2~m1;y~m2", "original", "original",
    "original", "add: m2~x;y~m1", "add: m2~x;y~x", "add: m2~x;y~m1",
    "add: y~m1;y~x", "add: m2~x;y~x", "add: y~m1;y~x", "original",
    "original", "original", "drop: m1~x", "drop: m2~m1", "drop: m1~x",
    "drop: y~m2", "drop: m2~m1", "drop: y~m2", "add: m2~x", "add: y~m1",
    "add: y~x"), dim = c(18L, 2L))

    expect_identical(as_edgelist(g2),
                      chk_g2,
                      info = "model_graph: df diff = 2")

    expect_true(is_igraph(g1),
                info = "model_graph")
    expect_true(is_igraph(g2),
                info = "model_graph")

    plot(g1)
    plot(g2)

    out3 <- model_set(fit,
                      prior_sem_out = .30,
                      df_change_add = 1,
                      df_change_drop = 1,
                      must_not_add = c("m1~~y",
                                       "m1~y"),
                      progress = FALSE,
                      parallel = FALSE)

    g3 <- model_graph(out3)
    plot(g3)

}
