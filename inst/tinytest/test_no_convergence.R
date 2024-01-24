if (length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

suppressMessages(library(lavaan))

mod <-
"
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
"
fit <- cfa(mod, HolzingerSwineford1939[HolzingerSwineford1939$school == "Pasteur", ])
suppressWarnings(out_nc <- model_set(fit,
                    parallel = FALSE,
                    progress = FALSE))
expect_true(all(is.na(out_nc$bpp)))
expect_stdout(print(out_nc),
              "not converged")
expect_error(model_graph(out_nc))
}