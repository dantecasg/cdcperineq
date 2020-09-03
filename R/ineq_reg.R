#' Grafica de desigualdades - versión de regresion
#' 
#' Cálculo de los coeficientes de una regresión lineal múltiple
#' @param data_ineq Data frame o Tibble. Datos obtenidos de la función 'data_proc' con la opción tipo = 'desigualdad'
#' @param xlab String. Nombre del eje X de la gráfica. NULL por defecto.
#' @param ylab String. Nombre del eje Y de la gráfica. NULL por defecto.
#' @param leyenda String. Nombre de la leyenda. NULL por defecto.
#' @export
#' @examples
#' ineq_reg()

ineq_reg = function(
        data_ineq, 
        xlab = NULL,
        ylab = NULL,
        leyenda = 'Años') {

    lineas = data_reg(data_ineq)

    g = ggplot() +
        geom_point(
            data = data_ineq,
            aes(
                x = CWpop,
                y = CWhealth, 
                colour = as.character(year), 
                shape = as.character(year))) +
        geom_line(
            data = lineas,
            aes(
                x = x,
                y = lf, 
                colour = as.character(year),
                linetype = as.character(year)
            )
        ) +
        geom_abline() +
        labs(
            x = xlab,
            y = ylab,
            colour = leyenda,
            linetype = leyenda,
            shape = leyenda) +
        theme_minimal() +
        panel_border()

    return(g)

}

