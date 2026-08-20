library(shiny)
library(shinydashboard)
library(openxlsx)

# ==========================================
# 1. CONFIGURACIÓN DE PONDERACIONES OFICIALES
# ==========================================
ponderaciones <- c(
  "Memoria Técnica"         = 0.30,
  "Simulación Computacional" = 0.30,
  "Defensa Oral"             = 0.35,
  "Documentación y Código"   = 0.05
)

# ==========================================
# 2. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Evaluación Proyectiles 2D"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Panel de Evaluación", tabName = "evaluacion", icon = icon("calculator")),
      menuItem("Consolidado General", tabName = "consolidado", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Pestaña de evaluación por equipo
      tabItem(tabName = "evaluacion",
              fluidRow(
                box(title = "Configuración del Equipo", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("selector_equipo", "Seleccione el Equipo a Evaluar:",
                                choices = paste("Equipo", 1:7)),
                    p(strong("Ponderaciones Oficiales:")),
                    tags$ul(
                      tags$li("Memoria Técnica: 30%"),
                      tags$li("Simulación: 30%"),
                      tags$li("Defensa Oral: 35%"),
                      tags$li("Código y Documentación: 5%")
                    ),
                    hr(),
                    actionButton("guardar_notas", "Guardar Notas del Equipo", icon = icon("save"), class = "btn-success"),
                    hr(),
                    downloadButton("descargar_excel", "Descargar Consolidado Excel", class = "btn-primary")
                ),
                box(title = "Ingreso de Notas (Escala 0 - 100)", status = "warning", solidHeader = TRUE, width = 8,
                    h4(textOutput("titulo_dinamico")),
                    numericInput("nota_memoria", "Nota Memoria Técnica:", value = 100, min = 0, max = 100, step = 5),
                    numericInput("nota_simulacion", "Nota Simulación Computacional:", value = 100, min = 0, max = 100, step = 5),
                    numericInput("nota_defensa", "Nota Defensa Oral:", value = 100, min = 0, max = 100, step = 5),
                    numericInput("nota_codigo", "Nota Documentación y Código:", value = 100, min = 0, max = 100, step = 5),
                    textAreaInput("observaciones", "Observaciones / Comentarios Generales:", value = "", rows = 3)
                )
              ),
              fluidRow(
                box(title = "Vista Previa de la Boleta Actual", status = "info", solidHeader = TRUE, width = 12,
                    tableOutput("tabla_preview"),
                    h3(align = "right", textOutput("total_preview"))
                )
              )
      ),
      # Pestaña del Consolidado en tiempo real
      tabItem(tabName = "consolidado",
              fluidRow(
                box(title = "Estado Actual de todos los Equipos", status = "primary", solidHeader = TRUE, width = 12,
                    tableOutput("tabla_consolidado"),
                    p(tags$small("*Nota: Las notas finales mostradas aquí se calculan de manera reactiva en la app, pero el Excel incluirá las fórmulas nativas correspondientes."))
                )
              )
      )
    )
  )
)

# ==========================================
# 3. LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # Estructura de almacenamiento reactiva interna
  valores_reactivos <- reactiveValues(
    equipos = lapply(1:7, function(i) {
      data.frame(
        Componente = as.character(names(ponderaciones)),
        Ponderacion = as.numeric(ponderaciones),
        Nota_Obtenida = c(100, 100, 100, 100), 
        Observaciones = c("", "", "", ""),
        stringsAsFactors = FALSE
      )
    })
  )
  
  output$titulo_dinamico <- renderText({
    paste("Formulario de Calificaciones -", input$selector_equipo)
  })
  
  # Cargar notas y comentarios guardados al cambiar de equipo
  observeEvent(input$selector_equipo, {
    idx <- as.numeric(gsub("Equipo ", "", input$selector_equipo))
    datos_actuales <- valores_reactivos$equipos[[idx]]
    
    updateNumericInput(session, "nota_memoria", value = datos_actuales$Nota_Obtenida[1])
    updateNumericInput(session, "nota_simulacion", value = datos_actuales$Nota_Obtenida[2])
    updateNumericInput(session, "nota_defensa", value = datos_actuales$Nota_Obtenida[3])
    updateNumericInput(session, "nota_codigo", value = datos_actuales$Nota_Obtenida[4])
    updateTextAreaInput(session, "observaciones", value = datos_actuales$Observaciones[1])
  })
  
  # Guardar notas y comentarios en la estructura reactiva
  observeEvent(input$guardar_notas, {
    idx <- as.numeric(gsub("Equipo ", "", input$selector_equipo))
    
    valores_reactivos$equipos[[idx]]$Nota_Obtenida <- c(
      input$nota_memoria,
      input$nota_simulacion,
      input$nota_defensa,
      input$nota_codigo
    )
    
    # Aseguramos el almacenamiento del string en el vector de observaciones
    valores_reactivos$equipos[[idx]]$Observaciones <- c(input$observaciones, "", "", "")
    
    showNotification(paste("Notas y observaciones de", input$selector_equipo, "guardadas correctamente."), type = "message")
  })
  
  output$tabla_preview <- renderTable({
    idx <- as.numeric(gsub("Equipo ", "", input$selector_equipo))
    df <- valores_reactivos$equipos[[idx]]
    df$Puntaje_Parcial <- df$Ponderacion * df$Nota_Obtenida
    
    colnames(df) <- c("Componente", "Ponderación (A)", "Nota Cruda (B)", "Observaciones", "Puntaje Parcial (A x B)")
    df[, c("Componente", "Ponderación (A)", "Nota Cruda (B)", "Puntaje Parcial (A x B)", "Observaciones")]
  }, digits = 2)
  
  output$total_preview <- renderText({
    idx <- as.numeric(gsub("Equipo ", "", input$selector_equipo))
    df <- valores_reactivos$equipos[[idx]]
    nota_final <- sum(df$Ponderacion * df$Nota_Obtenida)
    paste("Calificación Final Estimada:", formatC(nota_final, format = "f", digits = 1), "/ 100")
  })
  
  output$tabla_consolidado <- renderTable({
    resumen <- data.frame(
      Equipo = paste("Equipo", 1:7),
      Memoria_30pct = sapply(valores_reactivos$equipos, function(x) x$Nota_Obtenida[1]),
      Simulacion_30pct = sapply(valores_reactivos$equipos, function(x) x$Nota_Obtenida[2]),
      Defensa_35pct = sapply(valores_reactivos$equipos, function(x) x$Nota_Obtenida[3]),
      Codigo_5pct = sapply(valores_reactivos$equipos, function(x) x$Nota_Obtenida[4]),
      Observaciones = sapply(valores_reactivos$equipos, function(x) x$Observaciones[1]),
      stringsAsFactors = FALSE
    )
    resumen$NOTA_FINAL <- (resumen$Memoria_30pct * 0.30) + (resumen$Simulacion_30pct * 0.30) + 
      (resumen$Defensa_35pct * 0.35) + (resumen$Codigo_5pct * 0.05)
    
    colnames(resumen) <- c("Equipo", "Memoria (30%)", "Simulación (30%)", "Defensa (35%)", "Código (5%)", "Comentarios", "Nota Final")
    resumen[, c("Equipo", "Memoria (30%)", "Simulación (30%)", "Defensa (35%)", "Código (5%)", "Nota Final", "Comentarios")]
  }, digits = 1)
  
  # ==========================================
  # 4. EXPORTACIÓN SEGURA DE DATOS Y TEXTOS
  # ==========================================
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0("Acta_Final_Proyectiles_2D_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      tmp_wb <- createWorkbook()
      
      # Estilos visuales homogéneos
      style_header <- createStyle(fontName = "Segoe UI", fontSize = 11, fontColour = "#FFFFFF", fgFill = "#2980B9", halign = "center", textDecoration = "bold")
      style_title  <- createStyle(fontName = "Segoe UI", fontSize = 14, fontColour = "#FFFFFF", fgFill = "#1B4F72", halign = "center", textDecoration = "bold")
      style_accent <- createStyle(fontName = "Segoe UI", fgFill = "#F2F4F4")
      style_total  <- createStyle(fontName = "Segoe UI", fontSize = 11, fgFill = "#EAEDED", textDecoration = "bold")
      style_num    <- createStyle(fontName = "Segoe UI", halign = "center") 
      style_text   <- createStyle(fontName = "Segoe UI", halign = "left", wrapText = TRUE)
      
      # Hoja del Consolidado General
      addWorksheet(tmp_wb, "Consolidado General")
      setColWidths(tmp_wb, "Consolidado General", cols = 1:7, widths = c(15, 18, 18, 18, 18, 16, 45))
      
      writeData(tmp_wb, "Consolidado General", "SISTEMA DE INTERCEPTACIÓN DE PROYECTILES 2D - CONSOLIDADO GENERAL", startCol = 1, startRow = 1)
      mergeCells(tmp_wb, "Consolidado General", cols = 1:7, rows = 1)
      addStyle(tmp_wb, "Consolidado General", style = style_title, rows = 1, cols = 1:7)
      
      headers_resumen <- c("Equipo", "Memoria (30%)", "Simulación (30%)", "Defensa (35%)", "Código (5%)", "NOTA FINAL", "Comentarios Generales")
      writeData(tmp_wb, "Consolidado General", as.data.frame(t(headers_resumen)), startCol = 1, startRow = 3, colNames = FALSE)
      addStyle(tmp_wb, "Consolidado General", style = style_header, rows = 3, cols = 1:7)
      
      # Generar las pestañas de los equipos
      for (i in 1:7) {
        nombre_pestana <- paste("Equipo", i)
        addWorksheet(tmp_wb, nombre_pestana)
        setColWidths(tmp_wb, nombre_pestana, cols = 1:5, widths = c(28, 16, 18, 20, 45))
        
        writeData(tmp_wb, nombre_pestana, paste("HOJA DE CALIFICACIÓN OFICIAL:", toupper(nombre_pestana)), startCol = 1, startRow = 1)
        mergeCells(tmp_wb, nombre_pestana, cols = 1:5, rows = 1)
        addStyle(tmp_wb, nombre_pestana, style = style_title, rows = 1, cols = 1:5)
        
        headers_eq <- c("Componente a Evaluar", "Ponderación (A)", "Nota Obtenida [0-100] (B)", "Puntaje Parcial (A x B)", "Observaciones")
        writeData(tmp_wb, nombre_pestana, as.data.frame(t(headers_eq)), startCol = 1, startRow = 3, colNames = FALSE)
        addStyle(tmp_wb, nombre_pestana, style = style_header, rows = 3, cols = 1:5)
        
        # Extraer copia limpia de los datos recolectados
        datos_equipo <- valores_reactivos$equipos[[i]]
        
        # Guardar la observación como texto estático para forzar que aparezca en la pestaña individual
        txt_observacion <- datos_equipo$Observaciones[1]
        
        # Estructura limpia para volcar números
        df_valores <- data.frame(
          Componente = datos_equipo$Componente,
          Ponderacion = round(datos_equipo$Ponderacion, 2),
          Nota_Obtenida = round(datos_equipo$Nota_Obtenida, 1),
          stringsAsFactors = FALSE
        )
        
        # Escribimos las primeras 3 columnas (Componente, Ponderación, Nota)
        writeData(tmp_wb, nombre_pestana, df_valores, startCol = 1, startRow = 4, colNames = FALSE)
        
        # Escribimos el comentario explícitamente en la celda E4 de la pestaña del equipo
        writeData(tmp_wb, nombre_pestana, txt_observacion, startCol = 5, startRow = 4)
        
        # Estilos y fórmulas por fila
        for (f in 4:7) {
          writeFormula(tmp_wb, nombre_pestana, x = paste0("B", f, "*C", f), startCol = 4, startRow = f)
          if (f %% 2 == 0) addStyle(tmp_wb, nombre_pestana, style = style_accent, rows = f, cols = 1:5, stack = TRUE)
          addStyle(tmp_wb, nombre_pestana, style = style_num, rows = f, cols = 2:4)
          addStyle(tmp_wb, nombre_pestana, style = style_text, rows = f, cols = 5)
        }
        
        writeData(tmp_wb, nombre_pestana, "CALIFICACIÓN FINAL TOTAL:", startCol = 1, startRow = 8)
        writeFormula(tmp_wb, nombre_pestana, x = "SUM(D4:D7)", startCol = 4, startRow = 8)
        addStyle(tmp_wb, nombre_pestana, style = style_total, rows = 8, cols = 1:5)
        addStyle(tmp_wb, nombre_pestana, style = style_num, rows = 8, cols = 4)
        
        # ----------------------------------------------------------------------
        # VOLCADO EN EL CONSOLIDADO GENERAL
        # ----------------------------------------------------------------------
        fila_resumen <- i + 3
        writeData(tmp_wb, "Consolidado General", nombre_pestana, startCol = 1, startRow = fila_resumen)
        
        # Fórmulas numéricas para arrastrar las notas
        writeFormula(tmp_wb, "Consolidado General", x = paste0("'", nombre_pestana, "'!C4"), startCol = 2, startRow = fila_resumen)
        writeFormula(tmp_wb, "Consolidado General", x = paste0("'", nombre_pestana, "'!C5"), startCol = 3, startRow = fila_resumen)
        writeFormula(tmp_wb, "Consolidado General", x = paste0("'", nombre_pestana, "'!C6"), startCol = 4, startRow = fila_resumen)
        writeFormula(tmp_wb, "Consolidado General", x = paste0("'", nombre_pestana, "'!C7"), startCol = 5, startRow = fila_resumen)
        writeFormula(tmp_wb, "Consolidado General", x = paste0("'", nombre_pestana, "'!D8"), startCol = 6, startRow = fila_resumen)
        
        # SOLUCIÓN CRÍTICA: Escribimos el comentario directo desde R como texto estático en la col 7 del Consolidado
        # Esto elimina por completo el error del '0' o celdas vacías por referencias cruzadas de cadenas de texto
        writeData(tmp_wb, "Consolidado General", txt_observacion, startCol = 7, startRow = fila_resumen)
        
        if (fila_resumen %% 2 == 0) addStyle(tmp_wb, "Consolidado General", style = style_accent, rows = fila_resumen, cols = 1:7, stack = TRUE)
        addStyle(tmp_wb, "Consolidado General", style = style_num, rows = fila_resumen, cols = 2:6)
        addStyle(tmp_wb, "Consolidado General", style = style_total, rows = fila_resumen, cols = 6)
        addStyle(tmp_wb, "Consolidado General", style = style_text, rows = fila_resumen, cols = 7)
      }
      
      saveWorkbook(tmp_wb, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

shinyApp(ui, server)