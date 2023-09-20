if (interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

    library(lavaan)

    dat <- dat_path_model_p06

    mod <- attr(dat, "mod")

    fit <- sem(mod,
               dat_path_model_p06,
               fixed.x = TRUE)
    pt <- parameterTable(fit)
    pt_no_user <- pt[pt$op != ":=", ]
    pt0 <- parameterTable(fit)
    mod_to_add <- get_add(fit)
    fit_add <- lapply(mod_to_add,
                      function(x) update(fit, x))
    anova_add <- lapply(fit_add,
                        function(x) anova(x, fit))

    expect_true(
        all(names(mod_to_add) %in%
          c("add: y4~~y6", "add: y4~y6", "add: y5~x2", "add: y6~y4", "add: y6~x1",
            "add: y6~x2", "add: (y4~x1),(y4~x2)")),
        info = "Parameters to add as expected"
      )

    expect_true(
        all(sapply(anova_add,
                   function(x) x[2, "Df diff"]) == 1),
        info = "All df differences are one"
      )

    expect_true(
        all(sapply(mod_to_add,
                   modelbpp:::get_diff, pt_no_user) == names(mod_to_add)),
        info = "Generated difference matches the names"
      )
  }
