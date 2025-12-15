suppressMessages(library(lavaan))

mod <-
"
fx1 =~ x1 + x2 + x3
fx2 =~ x4 + x5 + x6
fy1 =~ x7 + x8 + x9
fy2 =~ x10 + x11 + x12
fy3 =~ x13 + x14 + x15
fy1 ~ fx1 + fx2
fy2 ~ fx2
fy3 ~ fy1 + fy2
"

fit <- sem(mod,
           dat_sem,
           warn = FALSE)

pt <- parameterTable(fit)
lvnames <- lavNames(fit, "lv")
indnames <- lavNames(fit, "ov.ind")
all_loadings <- expand.grid(lvnames, indnames)
all_loadings <- apply(all_loadings,
                      MARGIN = 1,
                      paste0,
                      collapse = "=~")
mod_loadings <- paste0(pt[pt$op == "=~", "lhs"],
                       "=~",
                       pt[pt$op == "=~", "rhs"])
all_loadings <- setdiff(all_loadings,
                        mod_loadings)
all_must_not_add <- c(all_loadings,
                      c("fy1~fy2", "fy2~fy1",
                        "fx1~~fy2", "fx1~~fy3",
                        "fy2~~fx1", "fy3~~fx1"))
all_must_not_drop <- c(mod_loadings,
                       "fx1~~fx2")

pts <- suppressWarnings(model_set(fit,
                 must_not_add = all_must_not_add,
                 must_not_drop = all_must_not_drop,
                 fit_models = FALSE,
                 compute_bpp = FALSE,
                 progress = FALSE))
pts <- pts$models
expect_true(length(intersect(all_must_not_add, names(pts))) == 0)
