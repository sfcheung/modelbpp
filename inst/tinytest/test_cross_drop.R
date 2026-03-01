suppressMessages(library(lavaan))
mod <-
"
fx1 =~ x1 + x2 + x3
fx2 =~ x4 + x5 + x6
fm1 =~ x7 + x8 + x9
fy2 =~ x10 + x11 + x12
fy3 =~ x13 + x14 + x15 + x11
fm1 ~ fx1 + fx2
fy2 ~ fm1
fy3 ~ fm1
"

fit <- sem(mod,
           dat_sem,
           warn = FALSE)

outa <- get_drop(fit, loadings_to_exclude = "all")
expect_true(!any(grepl("=", names(outa))))

outb <- get_drop(fit, loadings_to_exclude = "single")
expect_false("drop: fx1=~x2" %in% names(outb))
expect_true("drop: fy3=~x11" %in% names(outb))

outc <- get_drop(fit, loadings_to_exclude = "none")
expect_true("drop: fx1=~x2" %in% names(outc))

