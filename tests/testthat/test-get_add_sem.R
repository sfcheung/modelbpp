skip("WIP")

library(lavaan)

dat <- dat_sem

mod <-
"
f1 =~ x1 + x2 + x3 + x4
f2 =~ x5 + x6 + x7 + x8
f3 =~ x9 + x10 + x11 + x12
f4 =~ x13 + x14 + x15 + x16
f3 ~ f1 + f2
f4 ~ f3
"

fit <- sem(mod, dat_sem)
pt <- parameterTable(fit)
mod_to_add <- get_add(fit)
length(mod_to_add)
head(mod_to_add)
names(mod_to_add)

mod_to_add <- get_add(fit, df_change = 3)
