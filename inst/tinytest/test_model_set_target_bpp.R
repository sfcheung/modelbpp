suppressMessages(library(lavaan))

# Target BPP

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

postprob0 <- .75

e <- exp(-.5 * (out$bic - out$bic[1]))
e1 <- e[5]
estar <- sum(e[-5])
k <- length(e)
p1 <- (1 / (k - 1)) * postprob0 * estar / (e1 * (1 - postprob0) + postprob0 * estar / (k - 1))

p1_out <- min_prior(out$bic,
                    bpp_target = .75)

out <- model_set(fit,
                 prior_sem_out = unname(p1),
                 progress = FALSE,
                 parallel = FALSE)

expect_equal(p1_out,
              unname(p1),
              info = "Target BPP")
expect_equal(unname(out$bpp["original"]),
              postprob0,
              info = "Target BPP")
