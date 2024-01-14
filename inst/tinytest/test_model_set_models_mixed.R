suppressMessages(library(lavaan))

# Test user models

if (interactive() &&
    length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

# Slow tests

dat <- dat_path_model

mod <-
"
x3 ~ a*x1 + b*x2
x4 ~ a*x1
ab := a*b
"

fit <- sem(mod, dat_path_model, fixed.x = TRUE)

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

out <- model_set(fit,
                 progress = FALSE)

pt_user <- c(out$models,
             user2 = parameterTable(fit2),
             user3 = fit3)

# pt_user[["user2"]] <-
# pt_user[["user3"]] <- parameterTable(fit3)
# pt_user

out_user <- model_set(sem_out = fit,
                      partables = pt_user,
                      progress = FALSE,
                      parallel = FALSE)
out_user_prior <- model_set(sem_out = fit,
                            partables = pt_user,
                            prior_sem_out = .50,
                            progress = FALSE,
                            parallel = FALSE)

expect_true(all(c("user2", "user3") %in% names(out_user_prior$models)),
            info = "Add user models")

out_user_prior2 <- model_set(sem_out = fit,
                             partables = pt_user,
                             prior_sem_out = c(original = .30,
                                               user2 = .20,
                                               user3 = .10),
                             progress = FALSE,
                             parallel = FALSE)
tmp <- out_user_prior2$prior
names(tmp) <- names(out_user_prior2$models)
expect_equal(unname(tmp[c("user2", "original", "user3")]),
             c(.20, .30, .10),
             info = "Add user models, with priors")

out_user_prior3 <- model_set(sem_out = fit,
                             partables = pt_user,
                             prior_sem_out = c(original = .31,
                                               `add: x4~x2` = .21,
                                               user2 = .10),
                             progress = FALSE,
                             parallel = FALSE)

tmp <- out_user_prior3$prior
names(tmp) <- names(out_user_prior3$models)
expect_equal(unname(tmp[c("add: x4~x2", "original")]),
             c(.21, .31),
             info = "Add user models, with priors and nonstandard names")

expect_stdout(print(out_user_prior3,
                    bpp_target = .95,
                    target_name = "add: x1~x4"),
              pattern = "Target Model: add: x1~x4",
              info = "Add user models, print method")

# User models with graph

suppressMessages(library(igraph))

mod <-
"
x2 ~ x3 + 0*x4
x1 ~ x3
"

fit <- sem(mod,
           dat_path_model,
           fixed.x = FALSE)
out <- model_set(fit,
                 fit_models = FALSE,
                 progress = FALSE,
                 parallel = FALSE)

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

fit2 <- sem(mod2, dat_path_model, fixed.x = FALSE)
fit3 <- sem(mod3, dat_path_model, fixed.x = FALSE)

mod4 <-
"
x2 ~ 0*x3 + 0*x4
x1 ~ 0*x3
"
fit4 <- sem(mod4, dat_path_model)

pt_user <- get_partables(out)

pt_user <- c(pt_user,
             user2 = fit2,
             user3 = fit3,
             user4 = fit4)

# pt_user[["user2"]] <- parameterTable(fit2)
# pt_user[["user3"]] <- parameterTable(fit3)
# pt_user[["user4"]] <- parameterTable(fit4)

out_user_prior5 <- model_set(sem_out = fit,
                             partables = pt_user,
                             prior_sem_out = c(original = .11,
                                               user2 = .50),
                             progress = FALSE,
                             parallel = FALSE)
g <- model_graph(out_user_prior5)
# expect_warning(g <- model_graph(out_user_prior5),
#                pattern = "One or more")
expect_true(all(c("user2", "user4", "user3") %in%
                names(V(g))))
if (interactive()) {
    plot(g)
  }
}
