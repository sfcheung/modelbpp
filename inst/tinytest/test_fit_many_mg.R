if (interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

suppressMessages(library(lavaan))

data(dat_path_model)
set.seed(1234)
dat_path_model$group <- sample(c("gp1", "gp2", "gp3"),
                               size = nrow(dat_path_model),
                               replace = TRUE)
mod1 <-
"
x3 ~ x1 + x2
x4 ~ x1
"
fit1 <- sem(mod1,
           dat_path_model,
           fixed.x = FALSE,
           group = "group")
mod2 <-
"
x3 ~ x1 + x2
x4 ~ x1 + x2
"
fit2 <- sem(mod2,
           dat_path_model,
           fixed.x = FALSE,
           group = "group")
out <- fit_many(list(parameterTable(fit1),
                     parameterTable(fit2)),
                fit,
                progress = FALSE)

}