#' Coeficientes de regresión lineal
#' 
#' @param y Vector que representa una variable de respuesta
#' @param x1 Vector que representa la primera variable dependiente
#' @param x2 Vector que representa la segunda variable dependiente
#' @return Coeficientes de la regresión de \code{y} con \code{x1} y \code{x2}
#' @export
#' @examples
#' y <- rnorm(100)
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' lm_coef(y, x1, x2)
lm_coef = function(y, x1, x2) {

    fit = lm(y ~ x1 + x2 + 0)
    x1.coef = fit$coefficients[1]
    x2.coef = fit$coefficients[2]

    res = c(x1.coef, x2.coef)

    return(res)

}