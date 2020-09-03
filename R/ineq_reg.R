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

ccurve_f = function(k, mydata) {
    sum( (mydata$y - (exp(mydata$x / (k - mydata$x)) - 1) / (exp(1 / (k - 1)) - 1)) )^2
}

ineq_reg = function(
        data_ineq, 
        xlab = NULL,
        ylab = NULL,
        leyenda = 'Años') {

    anos = data_ineq %>% distinct(year) %>% pull()

    lineas_data = data_ineq %>%
        select(year, CWpop, CWhealth) %>%
        bind_rows(data.frame('year' = anos, 'CWpop' = 0, 'CWhealth' = 0)) %>%
        arrange(year, CWpop, CWhealth) %>%
        rename(x = CWpop, y = CWhealth)

    x = seq(0, 1, 0.01)
    lineas = data.frame(
        year = numeric(0),
        x = numeric(0),
        lf = numeric(0)
    )

    for (i in 1:length(anos)) {

        ccurve = optimx(
            par = -1.5, 
            # fn = ccurve_f,
            fn = function(k, mydata) { sum( (mydata$y - (exp(mydata$x / (k - mydata$x)) - 1) / (exp(1 / (k - 1)) - 1)) )^2 }, 
            mydata = lineas_data %>% filter(year == anos[i]), 
            control = list(all.methods = TRUE, save.failures = TRUE, maxit = 2500)
        )

        k = ccurve[1,1]
        lf = (exp(x / (k - x)) - 1) / (exp(1 / (k - 1)) - 1)

        df = data.frame(year = anos[i], x = x, lf = lf)

        lineas = bind_rows(lineas, df)

    }

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

