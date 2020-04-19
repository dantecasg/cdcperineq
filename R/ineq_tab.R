#' Grafica de brechas
#' 
#' Cálculo de los coeficientes de una regresión lineal múltiple
#' @param data_ineq Data frame o Tibble. Datos obtenidos de la función 'data_proc' con la opción tipo = 'desigualdad'.
#' @export
#' @examples
#' ineq_tab()

ineq_tab = function(data_ineq) {

    data_tab = data_ineq %>%
        group_by(year) %>%
        summarise(slope = mean(XiWi_coef) %>% round(., 2)) %>%
        ungroup()

    return(data_tab)

}
