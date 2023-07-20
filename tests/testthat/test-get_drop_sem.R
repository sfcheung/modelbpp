skip("WIP")

library(lavaan)

dat <- dat_sem

mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f3 =~ x9 + x10 + x11 + x12
f4 =~ x13 + x14 + x15 + x16
f2 ~ a * f1
f3 ~ a * f2
f4 ~ f3
"

fit <- sem(mod, dat_sem)
pt <- parameterTable(fit)

mod_to_drop <- get_drop(fit,
                      df_change = 3)
length(mod_to_drop)
head(mod_to_drop)
names(mod_to_drop)
attributes(mod_to_drop[[92]])
