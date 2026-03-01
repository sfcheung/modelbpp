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
out1 <- modelbpp:::xy_lav(fit)

expect_true(nrow(out1) == 7)
expect_true(modelbpp:::lav_y_on_x("fy1", "fy3", out1))
expect_true(modelbpp:::lav_y_on_x("fx2", "fy3", out1))
expect_false(modelbpp:::lav_y_on_x("fx2", "fx1", out1))

# TOOD:
# - Do we need to exclude `fx1 =~ x10`.
out2 <- modelbpp:::all_nonxy_loadings(fit)
tmp <- unique(out1$x)[1]
fx1_inds <- pt[(pt$lhs == tmp) & (pt$op == "=~"), "rhs"]
fx1to <- out1[out1$x == tmp, "y"]
fx1to_inds <- pt[(pt$lhs %in% fx1to) & (pt$op == "=~"), "rhs"]
expect_true(length(intersect(out2[out2$lhs == tmp, "rhs"],
                             fx1to_inds)) == 0)

out3 <- modelbpp:::lav_cross_set(fit, type = "pure_x")
expect_true(!any(out3$lhs %in% out1$y))
expect_true(all(out3$lhs %in% out1$x))

out4 <- modelbpp:::lav_cross_set(fit, type = "pure_y")
expect_true(nrow(out4) == 0)

out5 <- modelbpp:::lav_cross_set(fit, type = "user", user_lav = c("fx1", "fy1", "fy3"))
fuser_inds <- pt[(pt$lhs %in% c("fx1", "fy1", "fy3")) & (pt$op == "=~"), "rhs"]
expect_true(setequal(fuser_inds, unique(out5$rhs)))

# modelbpp:::lav_loadings(fit, lav = c("fx1", "fy3", "fy1"))
