library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)



#Datos

datos <- readRDS("data/indicadores_laborales.rds")

datos <- datos %>%
  mutate(año = as.integer(año),
         valor = as.numeric(valor))

#Interfaz

orden_indicadores <- c(
  "Tasa de actividad",
  "Tasa de empleo",
  "Tasa de desocupación",
  "Tasa de subocupación demandante",
  "Tasa de ocupación demandante",
  "Tasa de asalarización",
  "Tasa de no registro de asalariados"
)

ordenar_indicadores <- function(x) {
  
  x <- unique(x)
  
  c(
    intersect(orden_indicadores, x),
    sort(setdiff(x, orden_indicadores))
  )
}

colores_app <- c(
  "Total" = "#2FA4E7",
  "Varones" = "#2FA4E7",
  "Mujeres" = "#C7EBB1",
  "14 a 29 años" = "#2FA4E7",
  "30 a 64 años" = "#ADB17D"
)


fuente_grafico <- "Arial"

estilos_app <- tags$style(
  HTML("
    /* Título principal de la aplicación */
    .navbar-brand {
      font-size: 24px !important;
      font-weight: 500;
    }

    /* Nombres de las pestañas */
    .navbar-nav > li > a {
      font-size: 17px !important;
      font-weight: 400;
    }

    /* Pestaña activa */
    .navbar-nav > .active > a {
      font-weight: 500 !important;
    }

    /* Títulos internos de las pestañas */
    .tab-content h3 {
      font-size: 25px;
      font-weight: 500;
    }

    /* Título Filtros */
    .sidebar h4,
    .well h4 {
      font-size: 22px;
    }
  ")
)

ui <- navbarPage(
  
  title = "Indicadores laborales en Argentina y Mendoza",
  
  theme = shinythemes::shinytheme("cerulean"),
  
  header = estilos_app,
  
  # ---------------------------------------------------------------
  # Pestaña de evolución
  # ---------------------------------------------------------------
  
  tabPanel(
    title = "Indicadores (2016-2025)",
    
    sidebarLayout(
      
      sidebarPanel(
        
        h4("Filtros"),
        
        selectInput(
          inputId = "dominio",
          label = "Aglomerado",
          choices = sort(unique(datos$dominio)),
          selected = unique(datos$dominio)[1]
        ),
        
        selectInput(
          inputId = "indicador",
          label = "Indicador laboral",
          choices = ordenar_indicadores(datos$indicador),
          selected = ordenar_indicadores(datos$indicador)[1]
        ),
        
        selectInput(
          inputId = "desagregacion",
          label = "Desagregación",
          choices = c(
            "Total",
            "Sexo",
            "Edad"
          ),
          selected = "Total"
        ),
        
        tags$div(
          style = "
    margin-top: 22px;
    padding: 15px;
    background-color: #e6e6e6;
    border: 1px solid #cccccc;
    border-radius: 6px;
    text-align: left;
  ",
          
          tags$div(
            style = "
      margin-bottom: 8px;
      font-size: 15px;
      line-height: 1.3;
    ",
            
            HTML("<strong>¿Te resulta útil este tablero?</strong>")
          ),
          
          tags$p(
            style = "
      margin: 0 0 12px 0;
      font-size: 14px;
      line-height: 1.4;
    ",
            
            "Este proyecto se desarrolla de manera independiente y se encuentra disponible de forma abierta.",
            "Podés colaborar con su mantenimiento y actualización."
          ),
          
          tags$a(
            href = "https://cafecito.app/valuledda",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-primary",
            
            style = "
      display: inline-block;
      width: auto;
      margin: 0;
      padding: 8px 12px;
      float: none;
    ",
            
            "Invitame un Cafecito ☕ "
          ),
          
        ),  # Acá termina el recuadro gris
      
      # Botón para compartir, por fuera del recuadro
      tags$div(
        style = "
    margin-top: 12px;
    text-align: left;
  ",
        
        tags$button(
          id = "compartir_tablero",
          type = "button",
          class = "btn",
          style = "
      display: inline-block;
      width: auto;
      margin: 0;
      padding: 8px 13px;
      color: #2FA4E7;
      background-color: #ffffff;
      border: 1px solid #2FA4E7;
      border-radius: 5px;
    ",
          HTML("🔗 Compartir tablero")
        ),
        
        tags$span(
          id = "estado_compartir_tablero",
          style = "
      display: block;
      margin-top: 6px;
      font-size: 13px;
      color: #6c757d;
    "
        )
      ),
      
      width = 3
      ),
      
      mainPanel(
        
        plotlyOutput(
          outputId = "grafico_serie",
          height = "600px"
        ),
        
        width = 9
      )
    )
  ), 
  
  tabPanel(
    title = "Análisis del mercado de trabajo",
    
    fluidRow(
      column(
        width = 10,
        offset = 1,
        
        br(),
        
        wellPanel(
          h3("Informe sobre el mercado de trabajo"),
          
          p(
            "Esta aplicación permite explorar de manera interactiva ",
            "los datos elaborados para el informe ",
            tags$strong ("El mercado de trabajo en Argentina y Mendoza durante 2025.")
            ),
          
          p(
            "El informe ",
            "analiza la evolución reciente de las tasas básicas, ",
            "los indicadores de presión laboral y algunas dimensiones de las condiciones ",
            "de trabajo, tanto para los 31 aglomerados urbanos como para Gran Mendoza. ",
            "También examina las desigualdades según sexo y grupos de edad e incorpora ",
            "los nuevos indicadores de informalidad laboral difundidos por el INDEC."
          ),
          
          p(
            "Como complemento, este tablero permite consultar las series completas de ",
            "los promedios anuales del",
            tags$strong("período 2016-2025,"),
            "incluso según sexo y edad, aunque en el informe estas desagregaciones se ",
            "analizan específicamente para 2024 y 2025. Los nuevos indicadores de ",
            "informalidad se excluyen de este tablero porque solo se encuentran ",
            "disponibles a partir del cuarto trimestre de 2024 y, por lo tanto, ",
            "no es posible construir una serie temporal para el conjunto del período."
          ),
          
          br(),
          
          tags$a(
            href = "https://valuledda.github.io/trabajo_final_aset/",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-primary btn-lg",
            "Leer el informe completo"
          )
        )
      )
    ),
    
    tags$head(
      tags$script(
        HTML("
      async function copiarEnlaceTablero(url, estado) {
        try {
          await navigator.clipboard.writeText(url);

          if (estado) {
            estado.textContent = 'Enlace copiado';
          }
        } catch (error) {
          window.prompt('Copiá este enlace:', url);
        }
      }

      document.addEventListener('click', async function(event) {
        const boton = event.target.closest('#compartir_tablero');

        if (!boton) return;

        const estado = document.getElementById(
          'estado_compartir_tablero'
        );

        const datos = {
          title: 'Indicadores laborales en Argentina y Mendoza',
          text: 'Tablero interactivo con indicadores laborales elaborados a partir de la EPH.',
          url: 'https://valuledda-indicadores-laborales.share.connect.posit.cloud/'
        };

        try {
          if (navigator.share) {
            await navigator.share(datos);
          } else {
            await copiarEnlaceTablero(datos.url, estado);
          }
        } catch (error) {
          if (error.name !== 'AbortError') {
            await copiarEnlaceTablero(datos.url, estado);
          }
        }

        window.setTimeout(function() {
          if (estado) {
            estado.textContent = '';
          }
        }, 2500);
      });
    ")
      )
    )
),
  
  # ---------------------------------------------------------------
  # Pestaña metodológica
  # ---------------------------------------------------------------
  
  tabPanel(
    title = "Metodología",
    
    fluidRow(
      
      column(
        width = 10,
        offset = 1,
        
        br(),
        
        wellPanel(
          
          h3("Fuente de información y aspectos metodológicos"),
          
          p(
            "Fuente: Elaboración propia a partir de las bases de ",
            "microdatos de la Encuesta Permanente de Hogares (EPH) del ",
            "Instituto Nacional de Estadística y Censos (INDEC)."
          ),
          
          p(
            "La EPH releva las características sociodemográficas y laborales",
            "de la población residente en 31 aglomerados urbanos del país, entre",
            "los que se encuentra Gran Mendoza. En consecuencia, los resultados",
            "corresponden a este dominio de estimación y no representan la totalidad",
            "del territorio nacional ni el conjunto de la provincia de Mendoza."
          ),
          p("Los datos se presentan como promedios anuales elaborados a partir",
            "de los trimestres disponibles, criterio que permite reducir la incidencia",
            "de las fluctuaciones estacionales y ofrecer una lectura más estable de",
            "las tendencias.")
        )
      )
    )
  ),

tabPanel(
  title = "Cómo citar este tablero",
  
  fluidRow(
    column(
      width = 10,
      offset = 1,
      
      br(),
      
      wellPanel(
        h3("Cómo citar este tablero"),
        
        tags$blockquote(
          p(
            "Ledda, Valentina (2026). ",
            tags$em(
              "Indicadores laborales en Argentina y Mendoza: ",
              "2016-2025. Tablero interactivo [Shiny app]."
            ),
          " Disponible en: ",
          tags$a(
            href = "https://valuledda-indicadores-laborales.share.connect.posit.cloud",
            target = "_blank",
            "https://valuledda-indicadores-laborales.share.connect.posit.cloud"
          )
        )
      )
          )
        )
      )
    ),

tabPanel(
  title = "Información sobre el tablero",
  
  fluidRow(
    column(
      width = 10,
      offset = 1,
      
      br(),
      
      wellPanel(
        
        h3("Sobre el tablero"),
        
        p(
          "El desarrollo, mantenimiento y actualización de este tablero ",
          "constituyen un trabajo independiente que no cuenta actualmente ",
          "con financiamiento institucional. Su elaboración requiere el ",
          "procesamiento periódico de bases de datos, la construcción de ",
          "indicadores, la programación de visualizaciones y la revisión ",
          "de los resultados publicados."
        ),
        
        p(
          "Si considerás valiosa la disponibilidad de herramientas ",
          "interactivas y de acceso abierto para analizar información ",
          "social y laboral de Argentina y Mendoza, podés contribuir a ",
          "sostener este proyecto mediante Cafecito."
        ),
        
        tags$a(
          href = "https://cafecito.app/valuledda",
          target = "_blank",
          rel = "noopener noreferrer",
          class = "btn btn-primary",
          style = "
    display: inline-block;
    margin-top: 14px;
    padding: 10px 15px;
    font-size: 16px;
    text-decoration: none;
  ",
          HTML("Invitame un Cafecito&nbsp; ☕")
        ),
        
        tags$hr(),
        
        h3("Sobre la autora"),
        
        p(
          tags$strong("Valentina Ledda"),
          " es doctora en Ciencias Sociales, especialista en Métodos y ",
          "Técnicas de Investigación Social y licenciada en Sociología. ",
          "Se dedica a la investigación social aplicada, el análisis de ",
          "datos y la construcción de indicadores, especialmente en temas ",
          "vinculados con el mercado de trabajo, la desigualdad y las ",
          "condiciones de vida. Actualmente desarrolla este proyecto como ",
          "investigadora independiente."
        ),
        
        div(
          style = "
            width: 100%;
            text-align: left;
            margin-top: 18px;
          ",
          
          tags$a(
            href = "https://www.linkedin.com/in/valentina-ledda/",
            target = "_blank",
            
            style = "
              display: inline-block;
              margin: 0;
              padding: 8px 14px;
              color: #2FA4E7;
              background-color: transparent;
              border: 1px solid #2FA4E7;
              border-radius: 5px;
              text-decoration: none;
            ",
            
            "Ver perfil en LinkedIn"
          )
        )
      )
    )
  )
)
)


#Servidor

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------
  # Actualizar indicadores disponibles
  # ---------------------------------------------------------------
  
  observe({
    
    req(input$dominio, input$desagregacion)
    
    indicadores_disponibles <- datos %>%
      filter(
        dominio == input$dominio,
        desagregacion == input$desagregacion
      ) %>%
      distinct(indicador) %>%
      pull(indicador) %>%
      ordenar_indicadores()
    
    req(length(indicadores_disponibles) > 0)
    
    indicador_seleccionado <- isolate(input$indicador)
    
    if (
      is.null(indicador_seleccionado) ||
      !indicador_seleccionado %in% indicadores_disponibles
    ) {
      indicador_seleccionado <- indicadores_disponibles[[1]]
    }
    
    updateSelectInput(
      session = session,
      inputId = "indicador",
      choices = indicadores_disponibles,
      selected = indicador_seleccionado
    )
  })
  
  
  # ---------------------------------------------------------------
  # Base filtrada
  # ---------------------------------------------------------------
  
  datos_filtrados <- reactive({
    
    req(
      input$dominio,
      input$desagregacion,
      input$indicador
    )
    
    datos %>%
      filter(
        dominio == input$dominio,
        desagregacion == input$desagregacion,
        indicador == input$indicador
      ) %>%
      arrange(categoria, año)
  })
  
  
  # ---------------------------------------------------------------
  # Gráfico interactivo
  # ---------------------------------------------------------------
  
  output$grafico_serie <- renderPlotly({
    
    df <- datos_filtrados() %>%
      filter(!is.na(valor)) %>%
      mutate(
        etiqueta = format(
          round(valor, 1),
          decimal.mark = ",",
          nsmall = 1,
          trim = TRUE
        )
      )
    
    validate(
      need(
        nrow(df) > 0,
        "No hay datos disponibles para esta selección."
      )
    )
    
    
    # -------------------------------------------------------------
    # Título y subtítulo
    # -------------------------------------------------------------
    
    periodo <- paste0(
      min(df$año, na.rm = TRUE),
      "-",
      max(df$año, na.rm = TRUE)
    )
    
    if (input$desagregacion == "Total") {
      
      titulo <- paste0(
        input$indicador,
        ". ",
        input$dominio
      )
      
      subtitulo <- paste0(
        "Promedios anuales. ",
        periodo
      )
      
    } else {
      
      nombre_desagregacion <- case_when(
        input$desagregacion == "Sexo" ~ "sexo",
        input$desagregacion == "Edad" ~ "grupo de edad",
        TRUE ~ tolower(input$desagregacion)
      )
      
      titulo <- paste0(
        input$indicador,
        " según ",
        nombre_desagregacion,
        ". ",
        input$dominio
      )
      
      subtitulo <- paste0(
        "Población de 14 años y más\n",
        "Promedios anuales. ",
        periodo
      )
    }
    
    
    # -------------------------------------------------------------
    # Escala del eje vertical
    # -------------------------------------------------------------
    
    if (input$indicador %in% c(
      "Tasa de actividad",
      "Tasa de empleo"
    )) {
      
      if (input$desagregacion == "Total") {
        limite_superior <- 60
      } else {
        # Misma escala para sexo y edad
        limite_superior <- 90
      }
      
      paso <- 10
      
    } else if (
      input$indicador == "Tasa de ocupación demandante"
    ) {
      
      if (input$desagregacion == "Total") {
        limite_superior <- 30
      } else {
        # Misma escala para sexo y edad
        limite_superior <- 40
      }
      
      paso <- 5
      
    } else if (input$indicador %in% c(
      "Tasa de desocupación",
      "Tasa de subocupación demandante"
    )) {
      
      limite_superior <- 30
      paso <- 5
      
    } else if (
      input$indicador == "Tasa de asalarización"
    ) {
      
      limite_superior <- 90
      paso <- 10
      
    } else if (
      input$indicador == "Tasa de no registro de asalariados"
    ) {
      
      if (input$desagregacion == "Total") {
        limite_superior <- 60
      } else {
        # Misma escala para sexo y edad
        limite_superior <- 80
      }
      
      paso <- 10
      
    } else {
      
      limite_superior <- ceiling(
        max(df$valor, na.rm = TRUE) / 10
      ) * 10
      
      paso <- 10
    }
    
    espacio_etiqueta <- paso * 0.15
    
    
    # -------------------------------------------------------------
    # Gráfico base
    # -------------------------------------------------------------
    
    grafico <- ggplot(
      df,
      aes(
        x = factor(año),
        y = valor
      )
    )
    
    
    # -------------------------------------------------------------
    # Valores totales
    # -------------------------------------------------------------
    
    if (input$desagregacion == "Total") {
      
      grafico <- grafico +
        geom_col(
          fill = "#2FA4E7",
          alpha = 0.6,
          width = 0.6
        ) +
        geom_text(
          aes(
            y = valor + espacio_etiqueta,
            label = etiqueta
          ),
          size = 4,
          family = fuente_grafico
        )
      
      
      # -------------------------------------------------------------
      # Barras agrupadas por sexo o edad
      # -------------------------------------------------------------
      
    } else {
      
      posicion_barras <- position_dodge2(
        width = 0.7,
        padding = 0.1,
        preserve = "single"
      )
      
      posicion_texto <- position_dodge(
        width = 0.5
      )
      
      grafico <- grafico +
        geom_col(
          aes(
            fill = categoria,
            group = categoria
          ),
          position = posicion_barras,
          alpha = 0.6,
          width = 0.5
        ) +
        geom_text(
          aes(
            y = valor + espacio_etiqueta,
            label = etiqueta,
            group = categoria
          ),
          position = posicion_texto,
          size = 4,
          family = fuente_grafico
        ) +
        scale_fill_manual(
          values = colores_app,
          name = input$desagregacion
        )
    }
    
    
    # -------------------------------------------------------------
    # Diseño común
    # -------------------------------------------------------------
    
    grafico <- grafico +
      scale_y_continuous(
        limits = c(0, limite_superior),
        breaks = seq(
          0,
          limite_superior,
          by = paso
        ),
        labels = scales::label_number(
          accuracy = 0.1,
          decimal.mark = ","
        ),
        expand = expansion(
          mult = c(0, 0)
        )
      ) +
      labs(
        title = NULL,
        subtitle = NULL,
        x = "Año",
        y = "Tasa (%)",
        caption = paste0(
          "Elaboración propia a partir de ",
          "microdatos EPH-INDEC."
        )
      ) +
      theme_minimal(
        base_size = 12,
        base_family = fuente_grafico
      ) +
      theme(
        legend.position = "bottom",
        
        legend.title = element_text(
          face = "bold"
        ),
        
        panel.grid.minor = element_blank(),
        
        plot.caption = element_text(
          hjust = 0,
          size = 9,
          margin = margin(t = 12)
        ),
        
        axis.title = element_text(
          size = 11
        ),
        
        axis.text = element_text(
          size = 10
        ),
        
        plot.margin = margin(
          t = 15,
          r = 20,
          b = 15,
          l = 15
        )
      )
    
    
    # -------------------------------------------------------------
    # Conversión a Plotly
    # -------------------------------------------------------------
    
    subtitulo_plotly <- gsub(
      "\n",
      "<br>",
      subtitulo
    )
    
    valores_eje_y <- seq(
      0,
      limite_superior,
      by = paso
    )
    
    etiquetas_eje_y <- format(
      valores_eje_y,
      decimal.mark = ",",
      nsmall = 1,
      trim = TRUE)
    
    ggplotly(
      grafico,
      tooltip = "none" ) %>%
      style(
        hoverinfo = "skip"
      ) %>%
      config(
        displaylogo = FALSE,
        locale = "es",
        modeBarButtonsToRemove = c(
          "select2d",
          "lasso2d"
        )
      ) %>%
      layout(
        font = list(
          family = fuente_grafico,
          size = 12,
          color = "#333333"
        ),
        
        title = list(
          text = paste0(
            titulo,
            "<br>",
            "<span style='font-size:14px'>",
            subtitulo_plotly,
            "</span>"
          ),
          x = 0.03,
          xanchor = "left",
          y = 0.97,
          yanchor = "top",
          pad = list(
            l = 10,
            t = 5,
            b = 5
          ),
          font = list(
            family = fuente_grafico,
            size = 20,
            color = "#333333"
          )
        ),
        
        yaxis = list(
          range = c(0, limite_superior),
          tickmode = "array",
          tickvals = valores_eje_y,
          ticktext = etiquetas_eje_y,
          fixedrange = TRUE
        ),
        
        hovermode = FALSE,
        
        margin = list(
          l = 90,
          r = 30,
          t = 135,
          b = 90
        )
      )
  })
}

#Ejecución app

shinyApp(ui = ui,
         server = server)

