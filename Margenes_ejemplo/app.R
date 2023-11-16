
pacman::p_load(shiny, tidyverse, lubridate, readxl, tidyquant, scales, googlesheets4, ggforce, rvest, writexl, shinipsum,
               shinydashboard, shinyWidgets, DT, gt, jsonlite, httr, shinyjs, waiter, fresh, shinydashboardPlus)

`%not_in%` <- purrr::negate(`%in%`)


IndicadorAnual <- function(moneda, anio){
  urlAPIMonedaAnio<- paste0('https://mindicador.cl/api/',moneda,'/',anio)
  GETAPIMonedaAnio <- GET(urlAPIMonedaAnio)
  MonedaJsonAnio <- fromJSON(content(GETAPIMonedaAnio, "text"), simplifyVector = FALSE)
  MonedaJsonAnio
}


generador_base_tc <- function(años, moneda){
  
  if (moneda %not_in% c("uf", "ivp", "dolar", "dolar_intercambio", "euro", "ipc", "utm", 
                        "imacec", "tpm", "libra_cobre", "tasa_desempleo", "bitcoin")) {
    
    print(paste0("Elemento ",moneda ," no considerado"))
    
    stop()
    
  }
  
  
  
  lista_años <- list()
  
  for (i in 0:(años - 1)) {
    
    a <- IndicadorAnual(moneda, year(today()) - i)[["serie"]]
    
    lista_años <- c(lista_años, a)
    
  }

  años <- c()
  tc <- c()
  
  for (i in 1:length(lista_años)) {
    
    a <- lista_años[[i]][["fecha"]]
    años <- c(años, a)
    
    b <- lista_años[[i]][["valor"]]
    tc <- c(tc, b)
    
  }
  
  TC <- tibble(
    "fecha" = ymd(substr(años, 1, 10)),
    "TC_BC" = tc
  )
}


base_dolar <- generador_base_tc(3, "dolar") %>% mutate(moneda = "USD")


value_box_t_f <- function(x, y, z) {
  
  if(y == "V") {
    
    infoBox(
      "",
      value = x,
      icon = icon(name = z),
      color = "green")
      
  } else{
      
    infoBox(
      "",
      value = x,
      icon = icon(name = z),
      color = "red")
    }
}


colores_margenes <- function(x) {
  
  case_when(
    x < 0 ~ "red",
    x < 250000 ~ "orange",
    x < 600000 ~ "lightgreen",
    TRUE ~ "gold"
  )
}

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E",
    aqua = "#434C5E"
  ),
  adminlte_sidebar(
    width = "240px",
    dark_bg = "#2E3440",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    info_box_bg = "#FFF",
    box_bg = "#FFF"
  )
)

lista_roi <- c("ROI M126785", "ROI M126786", "ROI M126787")

casos <- read_xlsx("libro_ejemplo.xlsx",
                    sheet = "Caso_1")

ROD <- read_xlsx("libro_ejemplo.xlsx",
                 sheet = "ROD") 


header <- dashboardHeader()

sidebar <- dashboardSidebar(
  pickerInput(
    inputId = "ROI",
    label = "Seleccionar ROI",
    choices = lista_roi,
    multiple = FALSE,
    selected = lista_roi[1]
  )
)


body <- dashboardBody(
  use_theme(mytheme),
  fluidRow(
    column(width = 6,
           fluidRow(valueBoxOutput("titulo_value_box", width = 12))),
    column(width = 6,
           infoBoxOutput("value_box_flete", width = 3),
           infoBoxOutput("value_box_seguro", width = 3),
           infoBoxOutput("value_box_internacion", width = 3),
           infoBoxOutput("value_box_tl", width = 3),)
    ), 
  fluidRow(
    valueBoxOutput("margen_op", width = 6),
    valueBoxOutput("dif_tc", width = 6)
  ),
  fluidRow(
    column(
      width = 6,
      valueBoxOutput("exenta_shippter", width = 5),
      valueBoxOutput("ganador", width = 2),
      valueBoxOutput("exenta_embarcador", width = 5)
    ),
    column(
      width = 6,
      fluidRow(
        infoBoxOutput("casilla_afecta_embarcador", width = 6),
        infoBoxOutput("casilla_exenta_embarcador", width = 6)
      ),
      fluidRow(
        infoBoxOutput("casilla_afecta_transportelocal", width = 6),
        infoBoxOutput("casilla_notadecredito", width = 6)
      )
    )
  ), 
  fluidRow(
    box(width = 12,
        title = HTML("<b>Movimientos</b>"),
        solidHeader = T,
        collapsible = T,
        collapsed = F,
        div(DTOutput("movimientos"), style = "font-size:80%; overflow-x: scroll")),
  )
)


ui <- dashboardPage(
  preloader = list(html = tagList(spin_4(), "Loading ..."), color = "#434C5E"),
  header,
  sidebar,
  body
)



server <- function(input, output, session) {
  
  

# Bases -------------------------------------------------------------------

  
  base_margenes <- reactive({
    casos  %>%
      mutate(
        TC = ifelse(is.na(TC), 1, TC),
        Monto = case_when(
          `Tipo de movimiento` %in% c("Seguro", "Margen seguro") ~ Monto / TC,
          TRUE ~ Monto
        )
      ) %>% 
     filter(`Tipo de movimiento` %not_in% c("AGA", "Saldo a favor", "Saldo en contra"),
            `Tipo de documento` !=  "Provisión") %>% 
     left_join(base_dolar, by = c("Fecha real del movimiento" = "fecha", "Moneda de pago" = "moneda"))
 })
 
 

# Lista margenes individuales ---------------------------------------------
  
 
 margenes_individuales <- reactive({
   
   margen_op() %>%
     group_by(`Referencia (ROI)`) %>%
     summarise(margen_op = sum(monto_final)) %>%
     arrange(desc(margen_op)) %>% 
     mutate(colores = colores_margenes(margen_op),
            margen_op = dollar(margen_op, big.mark = ".", decimal.mark = ","),
            union = paste0(`Referencia (ROI)`, " | ", margen_op)) 
   
 })
 
 
 observe({
   
   vector_a <- margenes_individuales() %>% pull(`Referencia (ROI)`)
   
   vector_b <- margenes_individuales() %>% pull(union)
   
   colores <- margenes_individuales() %>% pull(colores)
   colores <- paste0("background:", colores, ";", "color: black;", "font-weight: bold;")

   updatePickerInput(session = session,
                     inputId = "ROI",
                     choices = setNames(vector_a, vector_b),
                     choicesOpt = list(
                       style = colores
                     ))
   
 })
 
 
 output$pickerer_input <- renderUI({
   
   vector_a <- margenes_individuales() %>% pull(`Referencia (ROI)`)
   
   vector_b <- margenes_individuales() %>% pull(union)
   
   colores <- margenes_individuales() %>% pull(colores)
   colores <- paste0("background:", colores, ";", "color: black;", "font-weight: bold;")
   
   
   pickerer_input(
     inputId = "ROI",
     choices = setNames(vector_a, vector_b),
     choicesOpt = list(
       style = colores
     )
   )
   
 })
 
 

# margen operacional ------------------------------------------------------

 
 
margen_op <- reactive({
   
   base_margenes()  %>% 
    select(`Tipo de documento`, `Tipo de movimiento`, Monto, TC, `Referencia (ROI)`) %>% 
     mutate(monto_final = Monto * TC,
            monto_final = case_when(
              `Tipo de movimiento` == "Cliente" & `Tipo de documento` == "Nota de credito" ~ monto_final* -1,
              `Tipo de movimiento` %in% c("Cliente", "Margen seguro") ~ monto_final,
              `Tipo de movimiento` == "Embarcador" & `Tipo de documento` == "Nota de credito" ~ monto_final,
              TRUE ~ monto_final * -1
            ))
 })
 
 
output$margen_op <- renderValueBox({
  
  x <- margen_op() %>% filter(`Referencia (ROI)` == input$ROI)
  
  margen <- sum(x$monto_final)
  
  a <- icon("chart-line")
  
  b <- icon("thumbs-down")
  
  valueBox(
    scales::dollar(margen, big.mark = ".", decimal.mark = ","),
    subtitle = "Margen Operacional",
    color = ifelse(margen > 0, "green", "red"),
    icon = if (margen > 0) {
      a
    } else{
      b
    }
  )
  
})

 

# Diferencia TC -----------------------------------------------------------

 
 dif_tc <- reactive({
   
  base_margenes() %>% filter(`Referencia (ROI)` == input$ROI) %>% 
     filter(`Moneda de pago` == "USD") %>% 
     mutate(margen_bruto = Monto * TC,
            margen_tc = Monto * TC_BC,
            efecto_tc = margen_tc - margen_bruto,
            efecto_tc = case_when(
              `Tipo de movimiento` %in% c("Cliente", "Margen seguro") ~ efecto_tc,
              TRUE ~ efecto_tc * -1
            )) 
   

 })
 
 output$dif_tc <- renderValueBox({
   
   tc <- sum(dif_tc()$efecto_tc)
   
   valueBox(
     scales::dollar(round(tc, 0), big.mark = ".", decimal.mark = ","),
     subtitle = "Diferencia TC",
     color = ifelse(tc > 0, "green", "red"),
     icon = icon("calculator"))
   
 })
 

 

# Titulo ------------------------------------------------------------------

 
 output$titulo_value_box <- renderValueBox({
   
   tipo <- ROD %>% filter(ROI == input$ROI) %>% 
     select(TIPO) %>% pull()
   
   titulo <- input$ROI
   
   req <- ROD %>% filter(ROI == input$ROI) %>% 
     select(TIPO, ORIGEN, INCOTERM, EMBARCADOR, NAVIERA) %>% 
     unlist() %>% unname() %>% .[!is.na(.)] %>% paste0(. , collapse  = " - ")
   
   detalle <- if(tipo != "FCL") {
     
     ROD %>% filter(ROI == input$ROI) %>% 
       select(VOLUMEN, PESO) %>% 
       slice_head(n = 1) %>% unlist() %>% unname() %>% 
       paste0(. , collapse  = " - ")
     
   } else{
     
     contenedor <- c(" x 20'ST", " x 40'HC", " x 40' NOR", " x 40 'ST")
    
     det <- ROD %>% filter(ROI == input$ROI) %>%
       select(`Container 20' ST`:`Container 40' ST`) %>%
       slice_head(n = 1) %>%
       paste0(., contenedor)
     
     condicion <- str_detect(det, "NA")
  
     final <- det[!condicion]
     
     det_final <- paste(final, collapse = " + ")
     
   }
   
   
   icono <- if (tipo == "AIR"){
     icon("plane")
   } else if (tipo == "FCL") {
     icon("box")
   } else {
     icon("boxes-stacked")
   }
   
   
   valueBox(value = HTML(paste(titulo, icono)),
            subtitle = HTML(paste0(req, '<br>', detalle)),
            color = "black")
   
 })
 

 output$servicios <- renderDataTable({
   
   ser <- ROD %>% filter(ROI == input$ROI) %>% 
     select(FLETE, SEGURO, INTERNACION, TL)
   
   datatable(ser,
             rownames = F,
             options = 
               list(dom = "t"))
   
   
 })
 
 

# Servicios ---------------------------------------------------------------

 
 
 servicios <- reactive({
   ROD %>% filter(ROI == input$ROI) %>% 
     select(FLETE, SEGURO, INTERNACION, TL) %>% 
     slice_head(n = 1)
 })
 
 
 output$value_box_flete <- renderInfoBox({
  

   y <- servicios()[1]
   
   
   value_box_t_f("FLETE", y, "anchor")
 })
 
 output$value_box_seguro <- renderInfoBox({
   
   y <- servicios()[2]
   
   value_box_t_f("SEGURO", y, "user-shield")
 })
 
 output$value_box_internacion <- renderInfoBox({
   
   y <- servicios()[3]
   
   value_box_t_f("INT", y, "piggy-bank")
   
 })
 
 output$value_box_tl <- renderInfoBox({
   
   y <- servicios()[4]
   
   value_box_t_f("TL", y, "truck")
 })
 

 
 

# Comparacion facturas ----------------------------------------------------

  
 
 facturas_exentas <- reactive({
   
   margen_op() %>% filter(`Referencia (ROI)` == input$ROI) %>% 
     filter(`Tipo de documento` %in% c("Exenta", "Nota de credito")) %>% 
     mutate(Monto = ifelse(`Tipo de movimiento` == "Seguro", -Monto, Monto),
            Monto = ifelse(`Tipo de documento` == "Nota de credito", -Monto, Monto)) %>% 
     filter(TC != 1)
   
 })
 
 
 exenta_embarcador <- reactive({
   
   facturas_exentas() %>% 
     filter(`Tipo de movimiento` == "Embarcador") %>% 
     summarise(sum = sum(Monto)) %>% pull()
   
 })
 
 
 exenta_shippter <- reactive({
   
   facturas_exentas() %>% 
     filter(`Tipo de movimiento`  %in% c("Cliente", "Seguro")) %>% 
     summarise(sum = sum(Monto)) %>% pull()
   
 })
 
 
 output$exenta_embarcador <- renderValueBox({
   
   icono <- if_else(exenta_shippter() > exenta_embarcador(), "face-angry", "face-grin-squint")
   

   valueBox(
     dollar(exenta_embarcador(), big.mark = ".", decimal.mark = ","),
     "Exenta Embarcador",
     color = ifelse(exenta_shippter() > exenta_embarcador(), "aqua", "red"),
     icon = icon(icono)
   )
 }
 )
 
 output$exenta_shippter <- renderValueBox({
   
   icono <- if_else(exenta_shippter() > exenta_embarcador(), "face-laugh-wink", "face-sad-cry")
   
   valueBox(
     dollar(exenta_shippter(), big.mark = ".", decimal.mark = ","),
     "Exenta Shippter",
     color = ifelse(exenta_shippter() > exenta_embarcador(), "green", "aqua"),
     icon = icon(icono)
   )
 }
 )
 
 output$ganador <- renderValueBox({
   
   valueBox(
     value = tags$p(ifelse(exenta_shippter() > exenta_embarcador(), ">", "<"),  style = "font-size: 150%;text-align:center;height:73px"),
     subtitle = "",
     color = ifelse(exenta_shippter() > exenta_embarcador(), "green", "red")
   )
   
 })
 

# Casillas numero facturas ------------------------------------------------

TL <- reactive({
  servicios()[4] %>% pull()
  
})
 
nfacturas <- reactive({
  
  casos %>% filter(`Referencia (ROI)` == input$ROI) %>%
    distinct(`Nro documento`, .keep_all = T) %>%
    group_by(`Tipo de movimiento`,
             `Tipo de documento`) %>%
    summarise(n = n()) 
  
  
})


base_estimada <- reactive({
  
  tibble("Tipo de movimiento" = c("Embarcador", "Embarcador", "Embarcador", "Transporte Local", "Cliente", "Cliente", "Cliente"),
         "Tipo de documento" = c("Afecta", "Exenta", "Nota de credito", "Afecta", "Exenta", "Afecta", "Nota de credito"),
         "estimado" = c(1,1,0,if_else(TL() == "V", 1, 0), 1 , 1, 0)) %>% 
    left_join(nfacturas()) %>% 
    mutate(n = ifelse(is.na(n), 0, n),
           union = paste0(n, "/", estimado),
           correcto = ifelse(n < estimado, FALSE, TRUE)) 
  
})


output$casilla_afecta_embarcador <- renderInfoBox({
  
  a <- base_estimada() %>% 
    filter(`Tipo de movimiento` == "Embarcador",
           `Tipo de documento` == "Afecta") %>% 
    slice_head(n = 1)
  
  b <- base_estimada() %>% 
    filter(`Tipo de movimiento` == "Cliente",
           `Tipo de documento` == "Afecta") %>% 
    slice_head(n = 1)
  
  infoBox(
    title = HTML("Afecta"),
    value = HTML(paste0("Embarcador = ", a[5], "<br>",
                   "Shippter = ", b[5])),
    fill = T,
    icon = icon("money-bill"),
    color = ifelse(a[6] == TRUE, "green", "red")
  )
}
)

output$casilla_exenta_embarcador <- renderInfoBox({
  
  a <- base_estimada() %>% 
    filter(`Tipo de movimiento` == "Embarcador",
           `Tipo de documento` == "Exenta") %>% 
    slice_head(n = 1)
  
  b <- base_estimada() %>% 
    filter(`Tipo de movimiento` == "Cliente",
           `Tipo de documento` == "Exenta") %>% 
    slice_head(n = 1)
  
  infoBox(
    title = HTML(paste0("Exenta")),
    value = HTML(paste0("Embarcador = ", a[5], "<br>",
                        "Shippter = ", b[5])),
    fill = T,
    icon = icon("dollar-sign"),
    color = ifelse(a[6] == TRUE, "green", "red"),
  )
}
)

output$casilla_afecta_transportelocal <- renderInfoBox({
  
  
  a <- base_estimada() %>% 
    filter(`Tipo de movimiento` == "Transporte Local",
           `Tipo de documento` == "Afecta") %>% 
    slice_head(n = 1)
  
  
  infoBox(
    title = HTML("Transporte<br>Local"),
    value = a[5],
    fill = T,
    icon = icon("truck-front"),
    color = ifelse(a[6] == TRUE, "green", "red")
  )
}
)


output$casilla_notadecredito <- renderInfoBox({
  
  a <- base_estimada() %>% 
    filter(`Tipo de movimiento` == "Embarcador",
           `Tipo de documento` == "Nota de credito") %>% 
    slice_head(n = 1)
  
  
  b <- base_estimada() %>% 
    filter(`Tipo de movimiento` == "Cliente",
           `Tipo de documento` == "Nota de credito") %>% 
    slice_head(n = 1)
  
  infoBox(
    title = HTML("Nota Credito"),
    fill = T,
    value = HTML(paste0("Embarcador = ", a[4], "<br>",
                        "Shippter = ", b[4])),
    icon = icon("shuffle"),
    color = "aqua")
}
) 
 
 

# Tabla movimientos -------------------------------------------------------

 
 output$movimientos <- renderDataTable({
   
   
   base_final <- base_margenes() %>% filter(`Referencia (ROI)` == input$ROI) %>% 
     mutate(
       monto_final = Monto * TC,
       monto_final = case_when(
         `Tipo de movimiento` == "Cliente" & `Tipo de documento` == "Nota de credito" ~ monto_final* -1,
         `Tipo de movimiento` %in% c("Cliente", "Margen seguro") ~ monto_final,
         `Tipo de movimiento` == "Embarcador" & `Tipo de documento` == "Nota de credito" ~ monto_final,
         TRUE ~ monto_final * -1
       )
     ) %>% 
     select(`Razón social`, `Fecha real del movimiento`, `Fecha emisión del documento`, `Nro documento` ,`Tipo de movimiento`,`Tipo de documento`,
            Monto, TC, monto_final, `Forma de pago`, `Moneda de pago`, TC_BC, `Pago Parcial o Sobrepago`) %>% 
     arrange(`Tipo de documento`, desc(`Moneda de pago`)) %>% 
     mutate(`Fecha real del movimiento` = format(`Fecha real del movimiento`, "%d-%m-%Y"),
            `Fecha emisión del documento` = format(`Fecha emisión del documento`, "%d-%m-%Y"))
   
   
   color_from_middle <- function (data, color1,color2) 
   {
     max_val=max(abs(data))
     JS(sprintf("isNaN(parseFloat(value)) || value < 0 ? 'linear-gradient(90deg, transparent, transparent ' + (50 + value/%s * 50) + '%%, %s ' + (50 + value/%s * 50) + '%%,%s  50%%,transparent 50%%)': 'linear-gradient(90deg, transparent, transparent 50%%, %s 50%%, %s ' + (50 + value/%s * 50) + '%%, transparent ' + (50 + value/%s * 50) + '%%)'",
                max_val,color1,max_val,color1,color2,color2,max_val,max_val))
   } 
   
   
   datatable(base_final,
             extensions = c('ColReorder', 'RowGroup'),
             escape = FALSE,
             class = 'row-border',
             options = list(
               "autoWidth" = T,
               columnDefs = list(list(className = 'dt-center', targets = 1:13)),
               colReorder = list(realtime = FALSE),
               dom = "t",
               scrollY = "275",
               rowGroup = list(dataSrc = 6)
             )) %>% 
     formatCurrency(c("monto_final"), mark = ".", digits = 0) %>% 
     formatStyle("monto_final",
                 background=color_from_middle(base_final$monto_final,'orange','lightgreen')) %>% 
     formatStyle("Moneda de pago",
                 color = styleEqual(c("USD"), c("red")))
   
   })
}


shinyApp(ui = ui, server = server)
