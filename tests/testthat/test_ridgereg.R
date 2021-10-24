context("ridgereg_unittest")
library(MASS)
data("mtcars")

test_that("Compare coefficients", {
    regmodel <- reg_model(mpg ~ disp + hp + drat + wt + qsec,data = mtcars)
    lmridge_mode <- lm.ridge(mpg ~ disp + hp + drat + wt + qsec,data = mtcars)
    expect_equal(round(regmodel$coef()[-1],1), round(lmridge_mode$coef,1))
})
