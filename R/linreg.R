#' A reference class representing av linear regression model
#'
#' @field beta_hat A matrix containing the estimated coefficients
#' @field y_hat A matrix containing the fitted values
#' @field res A matrix containing the residuals
#' @field df A numeric vector containg the degrees of freedom
#' @field res_var A numeric vector containing the residual variance
#' @field beta_hat_var A matrix containing the variance of the estimated coefficients
#' @field t_values A matrix containg the t-values of the estimated coefficients
#' @field p_values A matrix containg the p-values of the estimated coefficients
#' @field resstd A matrix containg the standardized residuals
#' @method print Prints out the coefficients and the coefficient names
#' @method plot_res Plots the residuals versus the fitted values
#' @method plot_resstd Plots the square root of the standardized residuals versus the fitted values
#' @method resid Returns a vector with the residuals
#' @method pred Returns a vector with the fitted values
#' @method coef Returns the estimated coefficients
#' @method summary Returns a summary of the linear regression model
#' @return An object of class linreg
#' @examples
#' linreg(iris, as.formula("Petal.Length ~ Species"))
#' linreg(iris, as.formula("Petal.Length ~ Sepal.Width + Sepal.Length"))
#' @references \url{https://en.wikipedia.org/wiki/Least_squares}
linreg <- setRefClass("linreg",
                      fields = list(
                        beta_hat = "matrix",
                        y_hat = "matrix",
                        res = "matrix",
                        df = "numeric",
                        res_var = "numeric",
                        beta_hat_var = "matrix",
                        t_values = "matrix",
                        p_values = "matrix",
                        resstd = "matrix"
                      ),
                      methods = list(
                        initialize = function(data, formula) {
                          X <- model.matrix(formula, data = data)
                          y <- iris[,all.vars(formula)[1]]
                          beta_hat <<- solve(t(X) %*% X) %*% t(X) %*% y
                          y_hat <<- X %*% beta_hat
                          res <<- y - y_hat
                          df <<- nrow(X) - ncol(X)
                          res_var <<- as.numeric((t(res) %*% res) / df)
                          beta_hat_var <<- res_var * (solve(t(X) %*% X))
                          t_values <<- beta_hat / sqrt(diag(beta_hat_var))
                          p_values <<- 2*pt(-abs(t_values), df = df)
                          resstd<<- na.omit(sqrt(res/res_var),0)
                        },
                        print = function() {
                          cat("test!")
                        },
                        plot_res = function() {
                          ggplot(data = data.frame(y_hat, res), aes(x = y_hat, y = res)) +
                            geom_point(shape = 21, colour = "black", fill = "white") +
                            geom_path(data = as.data.frame(with(data.frame(y_hat, res), lowess(x = y_hat, y = res))),
                                      aes(x = x, y = y), col = "red") +
                            ggtitle("Residuals vs Fitted") +
                            labs(x = "Fitted values", y = "Residuals") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  plot.title = element_text(hjust = 0.5))
                        },
                        plot_resstd = function() {
                          ggplot(data = data.frame(y_hat, resstd), aes(x = y_hat, y = resstd)) +
                            geom_point(shape = 21, colour = "black", fill = "white") +
                            geom_path(data = as.data.frame(with(data.frame(y_hat,resstd ), lowess(x = y_hat, y = resstd))),
                                      aes(x = x, y = y), col = "red") +
                            ggtitle("Scale−Location") +
                            labs(x = "Fitted values", y = "Standardized residuals") +
                            theme_bw() +
                            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  plot.title = element_text(hjust = 0.5))
                        },
                        resid = function() {
                          return(c(res))
                        },
                        pred = function() {
                          return(c(y_hat))
                        },
                        coef = function() {
                          coef_vec <- c(beta_hat)
                          names(coef_vec) <- row.names(beta_hat)
                          cat(colnames(beta_hat))
                          return(coef_vec)
                        },
                        summary = function() {
                          summary_df <- data.frame(beta_hat,
                                                   sqrt(diag(beta_hat_var)),
                                                   t_values,
                                                   p_values)
                          colnames(summary_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

                          print.data.frame(summary_df)
                          cat("\nResidual standard error: ", res_var, "\nDegrees of freedom: ", df)
                        }
                      ))
