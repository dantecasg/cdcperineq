#' Grafica de brechas
#' 
#' Cálculo de los coeficientes de una regresión lineal múltiple
#' @param data_ineq Data frame o Tibble. Datos obtenidos de la función 'data_proc' con la opción tipo = 'brecha'
#' @param xlab String. Nombre del eje X de la gráfica. NULL por defecto.
#' @param ylab String. Nombre del eje Y de la gráfica. NULL por defecto.
#' @param leyenda String. Nombre de la leyenda. NULL por defecto.
#' @export
#' @examples
#' brecha_gra()

brecha_gra = function(
        data_gap,
        xlab = NULL,
        ylab = NULL,
        leyenda = 'Quantile') {

    data.q = data_gap %>%
        group_by(year, cuartil) %>%
        summarise(meang = mean(meang)) %>%
        ungroup() %>%
        mutate(meang = round(meang, 2))

    g = ggplot(
            data = data.q,
            aes(x = as.character(year), y = meang, fill = cuartil)) +
        geom_col(position = 'dodge') +
        geom_text(
            aes(label = meang),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
        labs(
            x = xlab,
            y = ylab,
            fill = leyenda) +
        theme_minimal() +
        panel_border()

    return(g)

}
