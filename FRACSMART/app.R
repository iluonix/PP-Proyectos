library(shiny)
library(DT)
library(writexl)
library(readxl)
library(dplyr)
library(gmailr)

# ==============================================================================
# CONFIGURACIÓN GMAIL
# ==============================================================================
gm_auth_configure(path = "credentials.json")
gm_auth(email = "marco.montufar14@gmail.com", cache = ".secret", token = gm_token())

ui <- fluidPage(
  # SCRIPT PARA LA FECHA LOCAL (RELOJ DE TU DISPOSITIVO)
  tags$script('
    $(document).on("shiny:connected", function() {
      var hoy = new Date();
      var offset = hoy.getTimezoneOffset() * 60000;
      var localISOTime = (new Date(hoy - offset)).toISOString().slice(0,10);
      Shiny.setInputValue("fecha_navegador", localISOTime);
    });
  '),
  
  titlePanel("Registro de Ventas - Solares"),
  
  fluidRow(
    # COLUMNA 1: BÚSQUEDA Y CLIENTE
    column(width = 4, 
           wellPanel( 
             style = "background-color: #E6F7FF;", 
             h4("1. Búsqueda de Cliente 🔎"),
             radioButtons("modo_busqueda", "Método de Búsqueda:", 
                          choices = c("Nombre" = "nombre", "Dirección" = "direccion"),
                          selected = "direccion"),
             conditionalPanel(condition = "input.modo_busqueda == 'nombre'",
                              textInput("nombre_busqueda", "Nombre del Residente:", "")),
             conditionalPanel(condition = "input.modo_busqueda == 'direccion'",
                              selectInput("calle_busqueda", "Calle:", 
                                          choices=c("BAOBAB","SOLARES", "HAYA","EBANO","CEIBA","MANGLE","HIGUERA","HUIZACHE")),
                              numericInput("numero_busqueda", "Número:", value = NA))
           ), 
           wellPanel(
             h4("Datos del Cliente"),
             textInput("cliente", "Nombre del Cliente:", ""),
             textInput("num_recibo", "Número de Recibo:", ""), 
             selectInput("calle_select", "Calle (Registro):", 
                         choices = c("BAOBAB","SOLARES", "HAYA","EBANO","CEIBA","MANGLE","HIGUERA","HUIZACHE")),
             numericInput("numero", "Número casa:", value = NA) 
           )
    ),
    
    # COLUMNA 2: DATOS DE VENTA
    column(width = 5, 
           wellPanel(
             h4("2. Datos de la Venta 📝"),
             fluidRow(
               column(6, uiOutput("render_fecha")),
               column(6, selectInput("medpago", "Pago:", choices = c("Efectivo", "Transferencia")))
             ),
             
             fluidRow(
               column(6, selectInput("cantidad", "Cantidad marbetes:", 
                                     choices = list("Standard" = c("1", "2", "3", "4", "5"), 
                                                    "Otro" = c("Venta Especial")))), 
               column(6, numericInput("precio", "Monto Total ($):", value = 230, min = 0)) 
             ),
             
             conditionalPanel(
               condition = "input.cantidad == 'Venta Especial'",
               helpText(tags$b("⚠️ MODO MANUAL: Ingresa el monto acordado.", style="color:red;"))
             ),
             
             selectInput("vendedor", "Vendedor:", choices = c("Felix Nicolas", "José Alfonso", "Otro")),
             conditionalPanel(condition = "input.vendedor == 'Otro'", textInput("vendedor_otro", "Nombre:", "")),
             textAreaInput("nota_texto", "Notas:", rows = 2),
             tags$hr(),
             h5("Detalles de Marbetes"),
             uiOutput("campos_marbetes")
           )
    ),
    
    # COLUMNA 3: ACCIONES
    column(width = 3, 
           wellPanel(
             h4("3. Acciones"),
             actionButton("agregar", " + Agregar Venta", class = "btn-success", style = "width: 100%;"),
             br(), br(),
             actionButton("limpiar", " 󰇝 Borrar Pantalla", style = "width: 100%;"),
             br(), br(),
             actionButton("borrar", " 󰆴 Eliminar Fila", class = "btn-danger", style = "width: 100%;"),
             tags$hr(),
             downloadButton("exportar_excel", " 󰈄 Descargar Excel", style = "width: 100%;"),
             br(), br(),
             actionButton("enviar_email", " 󰇮 Enviar por Email", class = "btn-primary", style = "width: 100%;")
           )
    )
  ),
  DTOutput("tablaVentas")
)

server <- function(input, output, session) {
  
  ventas <- reactiveVal(data.frame())
  
  # RENDERIZA FECHA LOCAL
  output$render_fecha <- renderUI({
    fecha_val <- if(!is.null(input$fecha_navegador)) as.Date(input$fecha_navegador) else Sys.Date()
    dateInput("fecha", "Fecha:", format = "dd-mm-yyyy", value = fecha_val)
  })
  
  # LÓGICA DE PRECIO
  observe({
    if (input$cantidad == "Venta Especial") {
      updateNumericInput(session, "precio", value = NA) 
    } else {
      cant <- as.numeric(input$cantidad)
      updateNumericInput(session, "precio", value = 230 + (cant - 1) * 50)
    }
  })
  
  # BÚSQUEDA POR DIRECCIÓN
  numero_reactivo <- reactive({ input$numero_busqueda }) %>% debounce(800)
  observe({
    req(numero_reactivo(), input$modo_busqueda == "direccion")
    df <- tryCatch({ read_excel("solares.xlsx") }, error = function(e) NULL)
    req(df)
    resultado <- df %>% filter(as.character(Calle) == as.character(input$calle_busqueda),
                               as.character(Numero) == as.character(numero_reactivo()))
    if (nrow(resultado) > 0) {
      updateTextInput(session, "cliente", value = resultado$Nombre[1])
      updateSelectInput(session, "calle_select", selected = resultado$Calle[1])
      updateNumericInput(session, "numero", value = as.numeric(resultado$Numero[1]))
    }
  })
  
  # CAMPOS DINÁMICOS DE MARBETES
  output$campos_marbetes <- renderUI({
    cant <- if(input$cantidad == "Venta Especial") 1 else as.numeric(input$cantidad)
    lapply(1:cant, function(i) {
      fluidRow(
        column(6, textInput(paste0("marbete", i), paste0("Marbete ", i, ":"))),
        column(6, selectInput(paste0("tipo_marbete", i), "Tipo:", choices = c("Normal", "Tag")))
      )
    })
  })
  
  # --- AGREGAR VENTA (CORREGIDO: MARBETES Y LIMPIEZA DE NOTAS) ---
  observeEvent(input$agregar, {
    vendedor_f <- if(input$vendedor == "Otro") input$vendedor_otro else input$vendedor
    
    # Recopilar detalle de marbetes
    cant_num <- if(input$cantidad == "Venta Especial") 1 else as.numeric(input$cantidad)
    detalles_m <- sapply(1:cant_num, function(i) {
      paste0(input[[paste0("marbete", i)]], " (", input[[paste0("tipo_marbete", i)]], ")")
    })
    marbetes_texto <- paste(detalles_m, collapse = ", ")
    
    # Nota especial
    nota_final <- input$nota_texto
    if (input$cantidad == "Venta Especial") {
      nota_final <- paste("(Venta Especial)", nota_final)
    }
    
    nueva <- data.frame(
      Fecha = format(input$fecha, "%d-%m-%Y"),
      Cliente = input$cliente,
      Calle = input$calle_select,
      Numero = as.character(input$numero),
      Monto = input$precio,
      Pago = input$medpago,
      Marbetes = marbetes_texto, # COLUMNA RESTAURADA
      Vendedor = vendedor_f,
      Nota = nota_final,
      stringsAsFactors = FALSE
    )
    
    ventas(rbind(ventas(), nueva))
    
    # --- LIMPIAR NOTAS Y RECIBO DESPUÉS DE AGREGAR ---
    updateTextAreaInput(session, "nota_texto", value = "")
    updateTextInput(session, "num_recibo", value = "")
    showNotification("Venta registrada y notas limpias.")
  })
  
  # BOTÓN LIMPIAR PANTALLA
  observeEvent(input$limpiar, {
    updateTextInput(session, "cliente", value = "")
    updateNumericInput(session, "numero_busqueda", value = NA)
    updateNumericInput(session, "numero", value = NA)
    updateTextInput(session, "num_recibo", value = "")
    updateTextAreaInput(session, "nota_texto", value = "")
  })
  
  observeEvent(input$borrar, {
    req(input$tablaVentas_rows_selected)
    ventas(ventas()[-input$tablaVentas_rows_selected, ])
  })
  
  # RESUMEN PARA EXCEL
  generar_excel_con_resumen <- function(datos, ruta) {
    if(nrow(datos) == 0) return(NULL)
    resumen <- datos %>%
      group_by(Fecha, Pago) %>%
      summarise(Ventas = n(), Total = sum(as.numeric(Monto), na.rm = TRUE), .groups = 'drop') %>%
      mutate(Fecha = paste("TOTAL", Fecha))
    
    espacio_vacio <- data.frame(Fecha = "--- RESUMEN ---")
    
    # Asegurar que las columnas coincidan para unir el resumen
    final_df <- bind_rows(datos, espacio_vacio, resumen)
    writexl::write_xlsx(final_df, ruta)
  }
  
  output$exportar_excel <- downloadHandler(
    filename = function() { paste0("Reporte_", format(Sys.Date(), "%d-%m-%Y"), ".xlsx") },
    content = function(file) { generar_excel_con_resumen(ventas(), file) }
  )
  
  output$tablaVentas <- renderDT({ datatable(ventas(), selection = "single") })
}

shinyApp(ui, server)