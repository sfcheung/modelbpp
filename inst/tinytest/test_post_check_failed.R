if (length(unclass(packageVersion("modelbpp"))[[1]]) == 4) {

# WIP

suppressMessages(library(lavaan))

mod <-
"
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
"
fit <- cfa(mod, HolzingerSwineford1939[HolzingerSwineford1939$school == "Grant-White", ])
suppressWarnings(out_pc <- model_set(fit,
                                     parallel = FALSE,
                                     progress = FALSE))
expect_stdout(print(out_pc),
              "failed")
}