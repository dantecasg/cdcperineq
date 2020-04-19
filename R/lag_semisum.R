#' Promedio desfasado
#' 
#' Esta función realiza una semisuma con el dato en una posición anterior
#' @param x Vector tipo numeric o integer
#' @keywords semisum
#' @export
#' @examples
#' lag_semisum()

lag_semisum = function(x) {

    n = length(x)
    xi = c(0, x[1:(n-1)])
    res = (xi + x) / 2

    return(res)

}