suppressMessages(library(lavaan))
suppressMessages(library(igraph))

mod1 <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + x
"
fit1 <- sem(mod1, dat_serial_4_weak, fixed.x = FALSE)

mod2 <-
"
x ~ m1 + m2
y ~ x
"
fit2 <- sem(mod2, dat_serial_4_weak, fixed.x = FALSE)

mod3 <-
"
x ~ m1 + m2
y ~ x + m2
"
fit3 <- sem(mod3, dat_serial_4_weak, fixed.x = FALSE)

mod4 <-
"
m1 ~ y
m2 ~ m1
x ~ m2 + y + m1
"
fit4 <- sem(mod4, dat_serial_4_weak, fixed.x = FALSE)

fits <- list(original = fit1,
             fit2 = fit2,
             fit3 = fit3,
             fit4 = fit4)
pts <- lapply(fits, parameterTable)
class(pts) <- c("partables", class(pts))

out1_df2 <- model_set(fit1,
                      partables = pts,
                      progress = FALSE,
                      prior_sem_out = c(original = .001,
                                        fit2 = .0005,
                                        fit4 = .97))
# Check short names
expect_stdout(print(out1_df2, short_names = TRUE),
              out1_df2$short_names[1])

g <- model_graph(out1_df2,
                 progress = FALSE,
                 short_names = TRUE)
expect_true(setequal(out1_df2$short_names,
                     V(g)$label))
if (interactive()) {
plot(g)
}
expect_equal(g$layout$layout[, 2],
             c(2, 2, 1, 1))
