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
f4 ~ a * f3
"

fit <- sem(mod, dat_sem)
pt <- parameterTable(fit)

all_loadings <- sapply(paste0("f", 1:4),
                 function(x) {paste0(x, "=~", paste0("x", 1:16))})
mod_to_add <- get_add(fit,
                      must_not_add = all_loadings,
                      df_change = 2)
length(mod_to_add)
head(mod_to_add)
names(mod_to_add)
attributes(mod_to_add[[10]])
attributes(mod_to_add[[35]])
attributes(mod_to_add[[36]])
attributes(mod_to_add[[45]])

mod_to_add <- get_add(fit,
                      must_not_add = all_loadings,
                      df_change = 4)
length(mod_to_add)
head(mod_to_add)
names(mod_to_add)
attributes(mod_to_add[[128]])
attributes(mod_to_add[[78]])
