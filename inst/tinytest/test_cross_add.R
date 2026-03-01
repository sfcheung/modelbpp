suppressMessages(library(lavaan))
mod <-
"
fx1 =~ x1 + x2 + x3
fx2 =~ x4 + x5 + x6
fm1 =~ x7 + x8 + x9
fy2 =~ x10 + x11 + x12
fy3 =~ x13 + x14 + x15
fm1 ~ fx1 + fx2
fy2 ~ fm1
fy3 ~ fm1
"

# set.seed(2134)
# n <- nrow(dat_sem)
# fy4 <- rnorm(n)
# dat_sem$x16 <- .6 * fy4 + rnorm(n)
# dat_sem$x17 <- .6 * fy4 + rnorm(n)
# dat_sem$x18 <- .6 * fy4 + rnorm(n)

fit <- sem(mod,
           dat_sem,
           warn = FALSE)

chk_pure_x <- "add: fx1=~x4"
chk_pure_y <- "add: fy2=~x14"

mod_to_add_a <- get_add(fit, cross_add = c("pure_y", "pure_x"))
expect_true(chk_pure_x %in% names(mod_to_add_a))
expect_true(chk_pure_y %in% names(mod_to_add_a))

mod_to_add_b <- get_add(fit, cross_add = NULL)
expect_false(chk_pure_x %in% names(mod_to_add_b))
expect_false(chk_pure_y %in% names(mod_to_add_b))

mod_to_add_c <- get_add(fit, cross_add = "pure_x")
expect_true(chk_pure_x %in% names(mod_to_add_c))
expect_false(chk_pure_y %in% names(mod_to_add_c))

mod_to_add_d <- get_add(fit, cross_add = "pure_y")
expect_false(chk_pure_x %in% names(mod_to_add_d))
expect_true(chk_pure_y %in% names(mod_to_add_d))

mod_to_add_e <- get_add(fit, cross_add = "user", cross_sets = c("fx1", "fy2"))
expect_true("add: fx1=~x11" %in% names(mod_to_add_e))
expect_true("add: fy2=~x1" %in% names(mod_to_add_e))

mod_to_add_f <- get_add(fit, cross_add = NULL,
                        must_add = "fm1 =~ x1")
expect_true("add: fm1=~x1" %in% names(mod_to_add_f))
