suppressMessages(library(lavaan))

# Update bpp

dat <- dat_path_model

mod <-
"
x2 ~ x3 + 0*x4
x1 ~ x3
"

fit <- sem(mod,
           dat_path_model,
           fixed.x = TRUE)

out <- model_set(fit,
                 progress = FALSE,
                 parallel = FALSE)

out2 <- model_set(fit,
                  prior_sem_out = .40,
                  parallel = FALSE,
                  progress = FALSE)

out3 <- model_set(model_set_out = out,
                  fit_models = FALSE,
                  prior_sem_out = .40,
                  progress = FALSE,
                  parallel = FALSE)

expect_equal(out2$bpp,
              out3$bpp,
              info = "BPP update")
