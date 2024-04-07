# WIP

if (FALSE) {

    suppressMessages(library(lavaan))

    data(dat_path_model)
    set.seed(45142)
    dat_path_model$group <- sample(c("gp1", "gp2", "gp3"), size = nrow(dat_path_model), replace = TRUE)

    mod_pa <-
    "
    x2 ~ c(a1, a1, a3)*x1
    x3 ~ c(b1, b2, b3)*x2
    x4 ~ c(a, d, e)*x3
    ab1 := a*b1
    b1d := b1*d
    "

    fit_pa <- sem(mod_pa, dat_path_model, fixed.x = TRUE,
                  group = "group")
    mod_to_add_pa <- get_add(fit_pa,
                          must_add = "x4 ~ x2",
                          must_not_add = c("x3 ~ x1", "x2 ~~ x4"),
                          df_change = 2)
    names(mod_to_add_pa)

    data(dat_sem)
    set.seed(45142)
    dat_sem$group <- sample(c("gp1", "gp2", "gp3"), size = nrow(dat_sem), replace = TRUE)
    mod_cfa <-
    "
    f1 =~ x1 + x2 + x3 + x4
    f2 =~ x5 + x6 + x7 + x8
    "

    fit_cfa <- cfa(mod_cfa, dat_sem, group = "group", group.equal = "loadings")
    pt_cfa <- parameterTable(fit_cfa)

    all_loadings <- sapply(paste0("f", 1:4),
                           function(x) {
                              paste0(x,
                                     "=~",
                                     paste0("x", 1:16))
                            })
    mod_to_add_cfa <- get_add(fit_cfa,
                          must_not_add = all_loadings,
                          df_change = 2)
    length(mod_to_add_cfa)
    head(mod_to_add_cfa)
    names(mod_to_add_cfa)
    attributes(mod_to_add_cfa[[10]])
    attributes(mod_to_add_cfa[[35]])
    attributes(mod_to_add_cfa[[36]])
    attributes(mod_to_add_cfa[[45]])

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

