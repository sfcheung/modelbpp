suppressMessages(library(lavaan))

# Test c()

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)

moda <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fita <- sem(moda, dat_path_model, fixed.x = TRUE)

outa <- model_set(fita,
                  progress = FALSE,
                  parallel = FALSE)

modb <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x2
ab := a*b
"

fitb <- sem(modb, dat_path_model, fixed.x = TRUE)

outb <- model_set(fitb,
                  progress = FALSE,
                  parallel = FALSE)

tmp <- c(outa$models, outb$models)

expect_true(inherits(tmp, "partables"))
expect_identical(setdiff(names(tmp),
                         unique(c(names(outa$models),
                                  names(outb$models)))),
                 character(0))

tmp <- c(outb, outa$models)

expect_true(inherits(tmp, "partables"))
expect_identical(setdiff(names(tmp),
                         unique(c(names(outa$models),
                                  names(outb$models)))),
                 character(0))

mod2 <-
"
x2 ~ x1
x3 ~ x2
x4 ~ x3
"

mod3 <-
"
x2 ~ x4
x3 ~ x4
x1 ~ x2 + x3
"

fit2 <- sem(mod2, dat_path_model)
fit3 <- sem(mod3, dat_path_model)

pt2 <- parameterTable(fit2)

tmp <- c(outa, pt2, fit3, outb$models)

expect_true(inherits(tmp, "partables"))
expect_true(all(c("pt2", "fit3") %in% names(tmp)))

tmp <- c(outb, user2 = pt2, user3 = fit3, outa$models)

expect_true(inherits(tmp, "partables"))
expect_true(all(c("user2", "user3") %in% names(tmp)))

out_all <- model_set(sem_out = fit,
                     partables = tmp,
                     progress = FALSE,
                     parallel = FALSE)
expect_equal(length(out_all$fit),
             9)
out_all2 <- model_set(sem_out = fit,
                      partables = partables_drop(tmp,
                                                 c("user3",
                                                   "add: x4~x1")),
                      progress = FALSE,
                      parallel = FALSE)

expect_false(any(c("user3", "add: x4~x1") %in% out_all2))
