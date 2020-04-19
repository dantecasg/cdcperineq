#' Grafica de brechas
#' 
#' Cálculo de los coeficientes de una regresión lineal múltiple
#' @param data_ineq Data frame o Tibble. Datos obtenidos de la función 'data_proc' con la opción tipo = 'brecha'
#' @export
#' @examples
#' brecha_tab()

brecha_tab = function(data_gap) {

    data_idx = data_gap %>%
        group_by(year, cuartil) %>%
        mutate(regional_mean_rate = wpopg * meang / n()) %>%
        ungroup() %>%
        group_by(year) %>%
        summarise(
            regional_mean_rate = sum(regional_mean_rate) %>% round(., 2),
            absolute_Kuznets_index = max(meang) - min(meang) %>% round(.,2),
            relative_Kuznets_index = max(meang) / min(meang) %>% round(.,2)) %>%
        ungroup()

    return(data_idx)

}
