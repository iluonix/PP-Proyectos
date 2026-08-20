library(shiny)
library(shinydashboard)
library(googlesheets4)

#************************************************************************

#GOOGLE

google_email <- "mi463667@uaeh.edu.mx"

gs4_auth(email = google_email)


#************************************************************************

#RUBRIC

#change ts ltr w/actual rubric
#weights have to add up to 100

rubrica <- data.frame(

  Aspecto = c(
    rep("Planeacion y planteamiento",3),
    rep("Desarrollo tecnico",3),
    rep("Resultados y analisis",3),
    rep("Comunicacion academica",3),
    rep("Calidad y documentacion",3)
  ),

  Criterio = c(

    "Definicion clara del problema",
    "Objetivos y alcance del trabajo",
    "Metodologia propuesta",

    "Implementacion de la solucion",
    "Correctitud tecnica",
    "Uso de herramientas y recursos",

    "Presentacion de resultados",
    "Analisis de resultados",
    "Validacion de la solucion",

    "Organizacion de la presentacion",
    "Defensa oral y dominio del tema",
    "Recursos visuales y lenguaje academico",

    "Calidad del codigo o producto",
    "Documentacion, referencias y evidencias",
    "Presentacion final / reproducibilidad"
  ),

  Peso = c(

    7,7,6,       # 20
    9,8,8,       # 25
    7,7,6,       # 20
    7,7,6,       # 20
    5,5,5        # 15

  ),

  stringsAsFactors = FALSE
)

#comprobante
if(sum(rubrica$Peso) != 100){
  stop("La sumatoria de la rúbrica debe ser igual a 100")
}


aspectos <- unique(rubrica$Aspecto)

pesos_aspectos <- sapply(
  aspectos,
  function(x){
    sum(rubrica$Peso[rubrica$Aspecto == x])
  }
)



#empty tables for columns

empty_resumen <- data.frame(

  ID = character(),
  Fecha = character(),
  Profesor = character(),
  Materia_Grupo = character(),
  Evaluado = character(),
  Modo = character(),

  Aspecto_1 = numeric(),
  Aspecto_2 = numeric(),
  Aspecto_3 = numeric(),
  Aspecto_4 = numeric(),
  Aspecto_5 = numeric(),

  Nota_Final = numeric(),
  Comentarios = character(),

  stringsAsFactors = FALSE
)


empty_detalle <- data.frame(

  ID = character(),
  Fecha = character(),
  Profesor = character(),
  Materia_Grupo = character(),
  Evaluado = character(),
  Modo = character(),

  Aspecto = character(),
  Criterio = character(),
  Peso = numeric(),
  Nota = numeric(),
  Puntaje = numeric(),

  Comentarios = character(),

  stringsAsFactors = FALSE
)


#************************************************************************
# GOOGLE SHEET

#id gets saved here para que no se repita
google_id_file <- file.path(
  getwd(),
  "rubrica_google_sheet_id.txt"
)


google_sheet <- NULL


#se reusa si ya existe 
if(file.exists(google_id_file)){

  old_id <- readLines(
    google_id_file,
    warn = FALSE
  )

  if(length(old_id) > 0){

    old_id <- trimws(old_id[1])

    if(old_id != ""){
      google_sheet <- tryCatch({

        ss <- as_sheets_id(old_id)

        #access
        gs4_get(ss)

        ss

      },error=function(e){

        NULL

      })

    }

  }

}



#crear si no existe
if(is.null(google_sheet)){
  google_sheet <- gs4_create(

    "Evaluaciones Rubricas UAEH",

    sheets = list(
      Configuracion_Rubrica = rubrica
    )
  )


  #tabsa
  sheet_write(
    empty_resumen,
    ss = google_sheet,
    sheet = "Resumen_5_Aspectos"
  )


  sheet_write(
    empty_detalle,
    ss = google_sheet,
    sheet = "Rubrica_Completa"
  )


  #id local
  writeLines(
    as.character(google_sheet),
    google_id_file
  )

}



#por si alguien borra alfo o asi 
current_sheets <- sheet_names(google_sheet)


if(!("Resumen_5_Aspectos" %in% current_sheets)){
  sheet_write(
    empty_resumen,
    ss = google_sheet,
    sheet = "Resumen_5_Aspectos"
  )

}


if(!("Rubrica_Completa" %in% current_sheets)){

  sheet_write(
    empty_detalle,
    ss = google_sheet,
    sheet = "Rubrica_Completa"
  )

}


#rewrite
sheet_write(
  rubrica,
  ss = google_sheet,
  sheet = "Configuracion_Rubrica"
)


google_sheet_url <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  as.character(google_sheet),
  "/edit"
)


#************************************************************************
#FUNCTIONS

get_final <- function(notas){
  sum(
    notas * rubrica$Peso
  ) / 100
}



get_5 <- function(notas){
  x <- data.frame(
    Aspecto = aspectos,
    Peso = as.numeric(pesos_aspectos),
    Nota = 0,
    Aporte = 0
  )


  for(i in 1:length(aspectos)){
    ids <- which(
      rubrica$Aspecto == aspectos[i]
    )


    x$Nota[i] <-
      sum(
        notas[ids] *
          rubrica$Peso[ids]
      ) /
      sum(
        rubrica$Peso[ids]
      )


    x$Aporte[i] <-
      x$Nota[i] *
      x$Peso[i] /
      100
  }
  x
}



#read old evaluations google

read_old <- function(sheet){
  tryCatch({
    if(!(sheet %in% sheet_names(google_sheet))){
      return(data.frame())
    }

    x <- read_sheet(
      google_sheet,
      sheet = sheet
    )

    as.data.frame(x)

  },error=function(e){
    data.frame()
  })

}



#nueva evaluacion a google
save_google <- function(detalle,resumen){

  sheet_append(
    google_sheet,
    resumen,
    sheet = "Resumen_5_Aspectos"
  )


  sheet_append(
    google_sheet,
    detalle,
    sheet = "Rubrica_Completa"
  )

}


#************************************************************************
#UI

ui <- dashboardPage(

  skin = "blue",

  dashboardHeader(
    title = "Evaluacion de Rubricas"
  ),

  dashboardSidebar(

    sidebarMenu(
      menuItem(
        "Panel de Evaluacion",
        tabName = "evaluacion",
        icon = icon("calculator")
      ),

      menuItem(
        "Consolidado General",
        tabName = "consolidado",
        icon = icon("table")
      ),

      menuItem(
        "Rubrica Completa",
        tabName = "completa",
        icon = icon("list")
      )
    )
  ),


  dashboardBody(

    tabItems(
      #tab
      tabItem(
        tabName = "evaluacion",
        fluidRow(
          box(
            title = "Datos de Evaluacion",
            status = "primary",
            solidHeader = TRUE,
            width = 4,

            textInput(
              "profesor",
              "Profesor evaluador:"
            ),

            textInput(
              "materia",
              "Materia / grupo:"
            ),

            textInput(
              "evaluado",
              "Alumno, equipo o proyecto:"
            ),

            radioButtons(
              "modo",
              "Forma de evaluar:",

              choices = c(
                "Solo 5 aspectos" = "cinco",
                "Rubrica completa" = "completa"
              ),

              selected = "cinco"
            ),


            p(
              tags$small(
                "If you use 5 aspects, that score gets copied to the detailed rubric points inside that aspect."
              )
            ),


            textAreaInput(
              "comentarios",
              "Comentarios / observaciones:",
              rows = 4
            ),


            actionButton(
              "guardar",
              "Guardar Evaluacion",
              icon = icon("save"),
              class = "btn-success"
            ),


            br(),
            br(),


            tags$a(
              href = google_sheet_url,
              target = "_blank",
              class = "btn btn-primary",

              icon("table"),
              " Abrir Google Sheets"
            ),


            hr(),


            tags$small(
              paste(
                "Google account:",
                google_email
              )
            )

          ),



          box(
            title = "Ingreso de Notas (0 - 100)",
            status = "warning",
            solidHeader = TRUE,
            width = 8,

            uiOutput("inputs_notas")
          )
        ),



        fluidRow(
          box(
            title = "Resumen de 5 Aspectos",
            status = "info",
            solidHeader = TRUE,
            width = 5,

            tableOutput("preview5"),

            h3(
              align = "right",
              textOutput("preview_total")
            )
          ),


          box(
            title = "Rubrica Completa",
            status = "info",
            solidHeader = TRUE,
            width = 7,

            tableOutput("preview_completa")
          )
        )
      ),



      #resumen
      tabItem(

        tabName = "consolidado",
        fluidRow(

          box(
            title = "Evaluaciones Guardadas",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            actionButton(
              "actualizar",
              "Actualizar desde Google Sheets",
              icon = icon("refresh")
            ),

            br(),
            br(),
            tableOutput("tabla_resumen")
          )
        )
      ),



      #historial completo
      tabItem(

        tabName = "completa",

        fluidRow(
          box(
            title = "Historial de Todos los Criterios",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            tableOutput("tabla_detalle")

          )
        )
      )
    )
  )
)


#************************************************************************
#SERVER

server <- function(input,output,session){

  #valores de sheets
  valores <- reactiveValues(

    resumen = read_old(
      "Resumen_5_Aspectos"
    ),

    detalle = read_old(
      "Rubrica_Completa"
    )

  )

  #decide 5 inputs or  whole 
  output$inputs_notas <- renderUI({

    if(input$modo == "cinco"){
      cosas <- list()

      for(i in 1:length(aspectos)){

        cosas[[i]] <- div(

          h4(
            paste0(
              i,
              ". ",
              aspectos[i],
              " (",
              pesos_aspectos[i],
              "%)"
            )
          ),

          numericInput(

            paste0(
              "aspecto",
              i
            ),

            "Calificacion:",

            100,

            min = 0,
            max = 100,
            step = 1
          ),


          tags$small(
            paste(

              "Includes",
              sum(
                rubrica$Aspecto == aspectos[i]
              ),
              "rubric points"
            )
          ),


          hr()
        )
      }

      do.call(
        tagList,
        cosas
      )

    }else{
      cosas <- list()

      for(i in 1:nrow(rubrica)){
        # add the aspect title before its criteria
        if(
          i == 1 ||
          rubrica$Aspecto[i] != rubrica$Aspecto[i-1]
        ){

          cosas[[length(cosas)+1]] <- h4(

            paste0(

              rubrica$Aspecto[i],
              " (",
              pesos_aspectos[
                rubrica$Aspecto[i]
              ],
              "%)"

            )
          )
        }



        cosas[[length(cosas)+1]] <- numericInput(

          paste0(
            "criterio",
            i
          ),

          paste0(

            rubrica$Criterio[i],
            " [",
            rubrica$Peso[i],
            "%]"

          ),

          100,

          min = 0,
          max = 100,
          step = 1

        )
      }


      do.call(
        tagList,
        cosas
      )
    }
  })



  #para convertir a los 15
  notas_actuales <- reactive({

    notas <- rep(
      100,
      nrow(rubrica)
    )

    if(input$modo == "cinco"){

      for(i in 1:length(aspectos)){
        n <- input[[paste0("aspecto",i)]]

        if(is.null(n)){
          n <- 100
        }


        notas[
          rubrica$Aspecto == aspectos[i]
        ] <- n

      }


    }else{
      for(i in 1:nrow(rubrica)){
        n <- input[[paste0("criterio",i)]]
        if(is.null(n)){
          n <- 100
        }
        notas[i] <- n
      }
    }
    notas
  })



  #preview
  output$preview5 <- renderTable({


    x <- get_5(
      notas_actuales()
    )

    names(x) <- c(
      "Aspecto",
      "Peso (%)",
      "Nota",
      "Aporte Final"
    )

    x

  },digits = 2)

  #finakl
  output$preview_total <- renderText({
    paste(

      "Calificacion Final:",

      formatC(
        get_final(
          notas_actuales()
        ),
        format = "f",
        digits = 1
      ),

      "/ 100"
    )
  })



  #preview completa
  output$preview_completa <- renderTable({


    data.frame(
      Aspecto = rubrica$Aspecto,
      Criterio = rubrica$Criterio,
      `Peso (%)` = rubrica$Peso,
      Nota = notas_actuales(),
      check.names = FALSE
    )


  },digits = 1)


#************************************************************************
#SAVE

  observeEvent(input$guardar,{

    if(
      trimws(input$profesor) == "" ||
      trimws(input$evaluado) == ""
    ){
      showNotification(
        "Falta profesor o alumno/equipo.",
        type = "error"
      )
      return()
    }

    notas <- notas_actuales()

    if(
      any(
        notas < 0 |
        notas > 100 |
        is.na(notas)
      )
    ){

      showNotification(
        "Las notas deben estar entre 0 y 100.",
        type = "error"
      )
      return()
    }

    id <- paste0(

      format(
        Sys.time(),
        "%Y%m%d%H%M%S"
      ),

      "_",

      sample(
        100:999,
        1
      )
    )



    fecha <- format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )

    modo_txt <- if(
      input$modo == "cinco"
    ){
      "5 aspectos"
    }else{
      "rubrica completa"
    }

    r5 <- get_5(
      notas
    )

    #rows
    nuevo_detalle <- data.frame(

      ID = id,
      Fecha = fecha,
      Profesor = trimws(
        input$profesor
      ),
      Materia_Grupo = trimws(
        input$materia
      ),
      Evaluado = trimws(
        input$evaluado
      ),

      Modo = modo_txt,
      Aspecto = rubrica$Aspecto,
      Criterio = rubrica$Criterio,
      Peso = rubrica$Peso,
      Nota = notas,
      Puntaje =
        notas *
        rubrica$Peso /
        100,
      Comentarios = input$comentarios,
      stringsAsFactors = FALSE
    )



    nuevo_resumen <- data.frame(
      ID = id,
      Fecha = fecha,
      Profesor = trimws(
        input$profesor
      ),

      Materia_Grupo = trimws(
        input$materia
      ),

      Evaluado = trimws(
        input$evaluado
      ),
      Modo = modo_txt,

      Aspecto_1 = round(
        r5$Nota[1],
        2
      ),

      Aspecto_2 = round(
        r5$Nota[2],
        2
      ),

      Aspecto_3 = round(
        r5$Nota[3],
        2
      ),

      Aspecto_4 = round(
        r5$Nota[4],
        2
      ),

      Aspecto_5 = round(
        r5$Nota[5],
        2
      ),

      Nota_Final = round(
        get_final(notas),
        2
      ),

      Comentarios = input$comentarios,
      stringsAsFactors = FALSE

    )



    #envia a google sheets
    resultado <- tryCatch({

      save_google(
        nuevo_detalle,
        nuevo_resumen
      )

      TRUE

    },error=function(e){

      showNotification(

        paste(
          "Google Sheets error:",
          e$message
        ),
        type = "error",
        duration = 10
      )
      FALSE
    })

    if(!resultado){
      return()
    }

    # keep local copy updated too
    if(nrow(valores$detalle) == 0){
      valores$detalle <- nuevo_detalle
    }else{
      valores$detalle <- rbind(
        valores$detalle,
        nuevo_detalle
      )
    }

    if(nrow(valores$resumen) == 0){
      valores$resumen <- nuevo_resumen
    }else{
      valores$resumen <- rbind(
        valores$resumen,
        nuevo_resumen
      )
    }

    showNotification(
      "Evaluacion guardada en Google Sheets.",
      type = "message",
      duration = 5
    )
  })

  #reload
  observeEvent(input$actualizar,{
    valores$resumen <- read_old(
      "Resumen_5_Aspectos"
    )

    valores$detalle <- read_old(
      "Rubrica_Completa"
    )

    showNotification(
      "Datos actualizados desde Google Sheets.",
      type = "message"
    )

  })


#************************************************************************
  #SUMMARY TABLE

  output$tabla_resumen <- renderTable({

    if(nrow(valores$resumen) == 0){
      return(
        data.frame(
          Mensaje = "No hay evaluaciones guardadas todavia."
        )
      )
    }


    tail(
      valores$resumen,
      30
    )

  },digits = 2)


#************************************************************************

  #TABLA COMPLETA

  output$tabla_detalle <- renderTable({

    if(nrow(valores$detalle) == 0){
      return(
        data.frame(
          Mensaje = "No hay evaluaciones guardadas todavia."
        )
      )
    }

    tail(
      valores$detalle,
      100
    )

  },digits = 2)
}

shinyApp(
  ui,
  server
)