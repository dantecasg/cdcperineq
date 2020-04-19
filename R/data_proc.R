#' Calculando índices para las gráficas y tablas
#' 
#' Cálculo de los coeficientes de una regresión lineal múltiple
#' @param data Data frame o Tibble. De contener regiones, población por regiones, años, una cantidad de casos e incidencia de alguna enfermedad, y alguna variable social
#' @param tipo Character. Define el tipo de procesamiento que se hará. 'desigualdad' o 'brecha'.
#' @param periodo Integer. Vector con los años que se utilizarán para el procesamiento.
#' @param vari.x Character. Nombre de la columna que tiene la determinante social.
#' @param vari.ir Character. Nombre de la columna con la incidiencia de alguna enfermedad.
#' @param vari.num Character. Nombre de la columna con la cantidad de casos de una enfermedad.
#' @param anhos Character. Nombre de la columna que tiene los años. 'year' por defecto.
#' @param regiones Character. Nombre de la columna con las regiones. 'country' por defecto.
#' @param poblacion Character. Nombre de la columan con la poblacion. 'population' por defecto.
#' @export
#' @examples
#' data_proc()

data_proc = function(
        data, 
        tipo,
        periodo, 
        vari.x, 
        vari.ir,
        vari.num,
        anhos = 'year', 
        regiones = 'country', 
        poblacion = 'population') {

    # Generando símbolos de las variables para ser usadas por dplyr
    vax = sym(vari.x)
    vir = sym(vari.ir)
    vnm = sym(vari.num)
    yrs = sym(anhos)
    cty = sym(regiones)
    pob = sym(poblacion)

    # Adecuando base de datos
    data.fil = data %>%
        select(!!cty, !!yrs, !!pob, !!vax, !!vir, !!vnm) %>%
        filter(!!yrs %in% periodo) %>%
        group_by(!!yrs) %>%
        arrange(!!yrs, !!vax)

    # Hará un proceso distinto dependiendo del parámetro [tipo]
    # .................................................................. #

    # Tipo 1 (desigualdad?)
    if (tipo == 'desigualdad') {

        data.res = data.fil %>%
            mutate(
                Wpop = !!pob / sum(!!pob),
                CWpop = cumsum(Wpop),
                Whealth = !!vnm / sum(!!vnm), # parecido a Wpop
                CWhealth = cumsum(Whealth),
                ridit = lag_semisum(CWpop), # qué significa ridit?
                logridit = log10(ridit),
                Wi = sqrt(!!pob),
                XiWi = Wi * logridit,
                YiWi = Wi * !!vir,
                Wi_coef = lm_coef(YiWi, Wi, XiWi)[1],
                XiWi_coef = lm_coef(YiWi, Wi, XiWi)[2],
                Y = !!vir,
                predict = Wi_coef + XiWi_coef * logridit) %>%
            ungroup()

    # Brecha (?)
    } else if (tipo == 'brecha') {

        data.res = data.fil %>%
            mutate(
                cuartil = cut(
                    !!vax, 
                    quantile(!!vax),
                    include.lowest = TRUE,
                    labels = c('Q1', 'Q2', 'Q3', 'Q4')),
                sum_pop = sum(!!pob)) %>%
            group_by(!!yrs, cuartil) %>%
            mutate(
                qpg = sum(!!pob),
                wpopg = qpg / sum_pop,
                wpop = !!pob / qpg,
                wrate = wpop * !!vir,
                meang = sum(wrate)) %>%
            ungroup()

    # En caso no se cumpla ninguna condición
    } else {
        stop('Parámetro [tipo] debe ser "desigualdad" o "brecha"')
    }

    # Cambiando nombre a algunas columnas
    names(data.res)[names(data.res) == anhos] = 'year'

    return(data.res)

}
