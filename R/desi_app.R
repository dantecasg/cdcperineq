#' App para el analisis de las desigualdades y brechas sociales en salud.
#' 
#' Cálculo de los coeficientes de una regresión lineal múltiple
#' @param data Data frame o Tibble. De contener regiones, población por regiones, años, una cantidad de casos e incidencia de alguna enfermedad, y alguna variable social. La variable de años debe llamarse "year", la población "population" y la de regiones "country"
#' @param salud List. Lista con nombres genéricos respecto a diversos parámetros de salud. Cada elemento de la lista conténdrá dos parámetros: "IR" hace referencia a la variable en "data" que contenga la incidencia de una efermedad; mienrtas que "Num" será la variable con la cantidad total de casos.
#' @param social Vector. Vector con las variables de "data" que representen alguna característica social.
#' @param years Integer. Vector que contiene todo los años a analizar.
#' @param years_sel Integer. Vector con los años que serán seleccionados al iniciar el aplicativo. Por defecto, se seleccionará el primer elemnto de "years".
#' @export
#' @examples
#' data_proc()
#' 
#' # Variables sociales
#' social = c(
#'    'Esperanza de vida' = 'lexp',
#'    'Gasto en salud' = 'hexp',
#'    'Access to improved sanitation facilities' = 'impsfac',
#'    'TB detection rate' = 'pnotified')
#' # Variables de salud
#' salud = list(
#'    'Tuberculosis' = c(IR = 'ir_tb', Num = 'num_tb_cases')
#'    )
#'
#' # Años
#' years = c(2000, 2005, 2010)
#' 
#' desi_app(ineq_data, salud, social, years)

desi_app = function(data, salud, social, years, years_sel = years[1]) {

    # ================================================================== #
    # UI
    # ================================================================== #

    ui <- fluidPage(
      
        titlePanel('Desigualdades sociales'),

        sidebarLayout(

            # ---------------------------------------------------------- #

            sidebarPanel(

                selectInput(
                    inputId = 'social',
                    label = 'Determinante social',
                    choices = social
                ),

                selectInput(
                    inputId = 'enfer',
                    label = 'Enfermedad',
                    choices = names(salud)
                ),

                selectInput(
                    inputId = 'anhos',
                    label = 'Años',
                    choices = years,
                    selected = years_sel,
                    multiple = TRUE
                )

            ) ,

            # ---------------------------------------------------------- #

            mainPanel(

                tabsetPanel(

                    tabPanel(

                        title = 'Desigualdad',

                        plotOutput('des_gra'),

                        h4('Gradiente social'),

                        tableOutput('des_tab')

                    ), 

                    tabPanel(

                        title = 'Brechas',

                        plotOutput('bre_gra'),

                        h4('Metricas a nivelde de paises de acuerdo a la estratificacion social y años'),

                        tableOutput('bre_tab')

                    )

                )

            )

        )

    )

    # ================================================================== #
    # SERVER
    # ================================================================== #

    server <- function(input, output) {

        # Gráfica de desigualdad
        output$des_gra = renderPlot({

            sal_var = salud[[input$enfer]]

            tabla = data_proc(
                data = data, 
                tipo = 'desigualdad',
                periodo = as.integer(input$anhos),
                vari.x = input$social,
                vari.ir = sal_var['IR'],
                vari.num = sal_var['Num'],
                anhos = 'year',
                region = 'country',
                poblacion = 'population')

            ineq_gra(
                data = tabla, 
                xlab = 'Gradiente de población a nivel de país definido por esperanza de vida al nacer',
                ylab = 'Razón de incidencia de tuberculosis por 100,000 peronas')

        })

        # Tabla de desigualdades
        output$des_tab = renderTable({

            sal_var = salud[[input$enfer]]
            
            tabla = data_proc(
                data = data, 
                tipo = 'desigualdad',
                periodo = as.integer(input$anhos),
                vari.x = input$social,
                vari.ir = sal_var['IR'],
                vari.num = sal_var['Num'],
                anhos = 'year',
                region = 'country',
                poblacion = 'population')

            ineq_tab(tabla)

        })

        # Gráfuca de brechas
        output$bre_gra = renderPlot({

            sal_var = salud[[input$enfer]]

            tabla = data_proc(
                data = data, 
                tipo = 'brecha',
                periodo = as.integer(input$anhos),
                vari.x = input$social,
                vari.ir = sal_var['IR'],
                vari.num = sal_var['Num'],
                anhos = 'year',
                region = 'country',
                poblacion = 'population')

            brecha_gra(
                data = tabla,
                xlab = 'Cuartiles de esperanza de vida al nacer',
                ylab = 'Razón de incidencia estimada promedio de tuberculosis')

        })

        # Tabla de índices de brecha
        output$bre_tab = renderTable({

            sal_var = salud[[input$enfer]]

            tabla = data_proc(
                data = data, 
                tipo = 'brecha',
                periodo = as.integer(input$anhos),
                vari.x = input$social,
                vari.ir = sal_var['IR'],
                vari.num = sal_var['Num'],
                anhos = 'year',
                region = 'country',
                poblacion = 'population')

            brecha_tab(tabla)

        })

    }

    # ================================================================== #
    # APP
    # ================================================================== #

    shinyApp(ui = ui, server = server)

}

