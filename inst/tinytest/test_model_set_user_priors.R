suppressMessages(library(lavaan))

# User Prior

dat <- dat_path_model

mod <-
"
x2 ~ x3 + 0*x4
x1 ~ x3
"

fit <- sem(mod,
           dat_path_model,
           fixed.x = TRUE)

prior0 <- .50
out <- model_set(fit,
                 prior_sem_out = prior0,
                 parallel = FALSE,
                 progress = FALSE)
bic <- out$bic
i <- which(names(bic) == "original")
p <- length(bic)
prior <- rep((1 - prior0) / (p - 1), p)
prior[i] <- prior0
d <- exp(-.5 * (bic - bic[1])) * prior
chk_bpp <- d / sum(d)

expect_equivalent(
    out$bpp,
    chk_bpp,
    info = "Posterior probability as expected: User prior"
  )

expect_error(model_set(fit,
                       prior_sem_out = c(original = .80,
                                         `add: x1~x4` = .30),
                       parallel = FALSE,
                       progress = FALSE),
             info = "Invalid priors")
