#' Ridge regression using QR decomposition
#'
#' @field formula A formula object.
#' @field data A data.frame.
#' @field lambda Penalty parameter (>=0).
#' @export ridgereg
#' @exportClass ridgereg
ridgereg <- setRefClass("ridgereg",
                        fields = list(
                          formula = "formula",
                          data    = "data.frame",
                          lambda  = "numeric",
                          data_name = "character",
                          coefficients = "numeric",
                          fitted_values = "numeric",
                          x_means = "numeric",
                          x_sds = "numeric"
                        ),
                        methods = list(
                          initialize = function(formula, data, lambda = 0, ...){
                            stopifnot(inherits(formula, "formula"),
                                      is.data.frame(data), is.numeric(lambda),
                                      lambda >= 0)
                            formula <<- formula
                            data <<- data
                            lambda <<- lambda
                            data_name <<- deparse(substitute(data))

                            # 1. Designmatrix without intercept-collumn, and response
                            X <- model.matrix(formula, data)
                            X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
                            y <- model.response(model.frame(formula, data))

                            # 2. Normalize X (avg 0, sd 1) center y
                            x_means <<- colMeans(X)
                            x_sds <<- apply(X, 2, sd)
                            X_norm <- scale(X, center = x_means, scale = x_sds)
                            y_mean <- mean(y)
                            y_c <- y - y_mean

                            # 3. Utökat system: stapla sqrt(lambda) * I under X, nollor under y
                            p <- ncol(X_norm)
                            X_aug <- rbind(X_norm, sqrt(lambda) * diag(p))
                            y_aug <- c(y_c, rep(0, p))

                            # 4. QR on the extended system: R beta = Q^T y_aug
                            qr_aug <- qr(X_aug)
                            beta <- backsolve(qr.R(qr_aug), t(qr.Q(qr_aug)) %*% y_aug)
                            beta <- as.vector(beta)
                            names(beta) <- colnames(X)

                            coefficients <<- c("(Intercept)" = y_mean, beta)
                            fitted_values <<- as.vector(y_mean + X_norm %*% beta)

                          },

                          print = function(){
                            "Prints the call and the ridge regression coefficients."
                            cat("Call:\n")
                            cat("ridgereg(formula = ", deparse(formula), ", data = ", data_name,
                                ", lambda = ", lambda, ")\n\n", sep = "")
                            cat("Coefficients:\n")
                            base::print(coefficients)
                          },

                          predict = function(newdata = NULL){
                            "Returns fitted values, or predictions for newdata if supplied."
                            if(is.null(newdata)) return(fitted_values)
                            X_new <- model.matrix(delete.response(terms(formula, data = data)), newdata)
                            X_new <- X_new[, colnames(X_new) != "(Intercept)", drop = FALSE]
                            X_new_norm <- scale(X_new, center = x_means, scale = x_sds)
                            as.vector(coefficients[1] + X_new_norm %*% coefficients[-1])
                          },

                          coef = function(){
                            "Returns the ridge regression coefficients"
                            coefficients
                          }
                        )
)
