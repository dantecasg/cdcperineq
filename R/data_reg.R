#' Función para generar las curvas de regresión
#' 
#' Permite estimar las curvas que se ajustan a la data social. Se calcula a partir de 
#' la data de desigualdades.
#' @param data_ineq Data frame o Tibble. Datos obtenidos de la función 'data_proc' con la opción tipo = 'desigualdad'
#' @export
#' @examples
#' data_reg()

data_reg = function(data_ineq) {

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
            fn = function(k, mydata) { sum( (mydata$y - (exp(mydata$x / (k - mydata$x)) - 1) / (exp(1 / (k - 1)) - 1)) )^2 }, 
            mydata = lineas_data %>% filter(year == anos[i]), 
            control = list(all.methods = TRUE, save.failures = TRUE, maxit = 2500)
        )

        k = ccurve[1,1]
        lf = (exp(x / (k - x)) - 1) / (exp(1 / (k - 1)) - 1)

        df = data.frame(year = anos[i], x = x, lf = lf)

        lineas = bind_rows(lineas, df)

    }

    return(lineas)

}
