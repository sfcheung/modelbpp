# WIP

if (FALSE) {

    suppressMessages(library(lavaan))

    data(dat_sem)
    set.seed(45142)
    dat_sem$group <- sample(c("gp1", "gp2", "gp3"), size = nrow(dat_sem), replace = TRUE)
    mod <-
    "
    f1 =~ x1 + x2 + x3 + x4
    f2 =~ x5 + x6 + x7 + x8
    f3 =~ x9 + x10 + x11 + x12
    f4 =~ x13 + x14 + x15 + x16
    "

    fit <- cfa(mod, dat_sem, group = "group", group.equal = "loadings")
    pt <- parameterTable(fit)

    all_loadings <- sapply(paste0("f", 1:4),
                           function(x) {
                              paste0(x,
                                     "=~",
                                     paste0("x", 1:16))
                            })
    mod_to_add <- get_add(fit,
                          must_not_add = all_loadings,
                          df_change = 1)
    length(mod_to_add)
    head(mod_to_add)
    names(mod_to_add)
    attributes(mod_to_add[[10]])
    attributes(mod_to_add[[35]])
    attributes(mod_to_add[[36]])
    attributes(mod_to_add[[45]])

    tmp1 <- sapply(mod_to_add, attr, "df_expected")
    tmp2 <- sapply(mod_to_add, attr, "df_actual")
    all.equal(tmp1, tmp2)

    mod_to_add <- get_add(fit,
                          must_not_add = all_loadings,
                          df_change = 4)
    length(mod_to_add)
    head(mod_to_add)
    names(mod_to_add)
    attributes(mod_to_add[[128]])
    attributes(mod_to_add[[78]])

    # Test Print

    mod_to_add
    print(mod_to_add,
          max_tables = 25)
  }

