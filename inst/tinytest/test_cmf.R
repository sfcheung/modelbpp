suppressMessages(library(lavaan))

mod1 <-
"
x2 ~ x1 + x4
x3 ~ x2
"

mod2 <-
"
x3 ~ x1
x3 ~ x2
x4 ~ x3
"

fit1 <- sem(mod1, HolzingerSwineford1939, fixed.x = FALSE)
fit2 <- sem(mod2, HolzingerSwineford1939, fixed.x = FALSE)

out1 <- model_set(fit1,
                  progress = FALSE,
                  parallel = FALSE)
out2 <- model_set(fit2,
                  progress = FALSE,
                  parallel = FALSE)

# Need to rename the fits due to name conflicts.
out1b <- out1
out2b <- out2
names(out1b$fit) <- paste0("fit1_", names(out1b$fit))
names(out2b$fit) <- paste0("fit2_", names(out2b$fit))

fit_all <- c(out1b$fit,
             out2b$fit)
names(fit_all)

outa <- model_set(fit_all,
                  progress = FALSE,
                  parallel = FALSE)

outa

outb <- model_set_combined(
            list(fit1 = out1,
                 fit2 = out2),
            progress = FALSE,
            parallel = FALSE)

outb

names(outa)
names(outb)

expect_equal(outa$bpp,
             outb$bpp)
