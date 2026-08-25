#shiny::runApp("C:/Users/PC/Documents/GitHub/PP-Proyectos/SISTEMA-RUBRICA")

library(shiny)
library(shinydashboard)
library(googlesheets4)

#************************************************************************
#GOOGLE

google_email <- "mi463667@uaeh.edu.mx"
gs4_auth(email = google_email)

#************************************************************************
#ESTUDIANTES

alumnos <- data.frame(
  Equipo = c(
    rep(1,5),
    rep(2,4),
    rep(3,5),
    rep(4,4),
    rep(5,4),
    rep(6,5),
    rep(7,4),
    rep(8,3)
  ),

  Alumno = c(
    #equipo 1
    "Cruz Ortiz",
    "Jimenez Bautista",
    "Martinez Quintero",
    "Oaxaca Valdez",
    "Pacheco Zamora",

    #equipo 2
    "Barajas Longoria",
    "Bautista Bojorges",
    "Hernandez Sanchez",
    "Moreno Badillo",

    #equipo 3
    "Cabrera Dorantes",
    "Hernandez Gonzales",
    "Rodriguez Torres",
    "Rojo Lopez",
    "Torres Hernandez",

    #equipo 4
    "De La Cruz Ventilla",
    "Noguez Perez",
    "Perez Moreno",
    "Reyes Lara",

    #equipo 5
    "Blancas Gomez",
    "Hernandez Aguilar",
    "Nuñez Espinoza",
    "Velasco Hernandez",

    #equipo 6
    "Gomez Cruz",
    "Hernandez Larrieta",
    "Martinez Gonzalez",
    "Mendoza Barrera",
    "Trujillo Ortega",

    #equipo 7
    "Dominguez Rivera",
    "Gomez Garcia",
    "Lopez Jacinto",
    "Monroy Jimenez",

    #equipo 8
    "Fonseca Duran",
    "Mendez Hernandez",
    "Vila Leonar"
  ),
  stringsAsFactors = FALSE
)

#************************************************************************
#RUBRICA

rubrica <- data.frame(
  Aspecto = c(
    "Extraer información y adaptar los problemas",

    "Aplica principios matemáticos y físicos",

    "Evaluación de las limitaciones",

    "Identifican las necesidades de formación continua",

    "Desarrollan un plan de formación continua"
  ),

  Descripcion = c(
    "Identifican y formulan problemas complejos estableciendo contexto, parámetros y restricciones.",

    "Desarrollan modelos a partir de la información dada para analizar problemas complejos de ingeniería",

    "Interpretan los resultados obtenidos para extraer conclusiones críticas y fundamentadas.",

    "Identifican las necesidades de formación continua.",

    "Desarrollan un plan de formación continua."
  ),

  Peso = c(
    40,
    20,
    20,
    10,
    10
  ),

  stringsAsFactors = FALSE
)

#comprobante
if(sum(rubrica$Peso)!=100){
  stop(
    "La suma de la rúbrica debe ser igual a 100"
  )
}

#************************************************************************
#TABLAS VACIAS

empty_equipos <- data.frame(
  ID = character(),
  Fecha = character(),
  Profesor = character(),
  Materia_Grupo = character(),
  Equipo = integer(),
  Integrantes = character(),

  Comprension = numeric(),
  Prototipo = numeric(),
  Modelo = numeric(),
  Tecnologia = numeric(),
  Comunicacion = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


#filas pa los alumnos
empty_alumnos <- data.frame(
  ID = character(),
  Fecha = character(),
  Profesor = character(),
  Materia_Grupo = character(),
  Equipo = integer(),
  Alumno = character(),

  Comprension = numeric(),
  Prototipo = numeric(),
  Modelo = numeric(),
  Tecnologia = numeric(),
  Comunicacion = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)

#promedio equipos
empty_prom_equipos <- data.frame(
  Equipo = as.character(1:8),

  Promedio = rep(
    NA_real_,
    8
  ),

  stringsAsFactors = FALSE
)

#todos los alumnos aunque no tengan calif
empty_prom_alumnos <- alumnos
empty_prom_alumnos$Promedio <- NA_real_

#************************************************************************
#GOOGLE SHEET EQUIPOS

google_equipos_id_file <- file.path(
  getwd(),
  "google_sheet_equipos_id.txt"
)

google_equipos <- NULL


if(file.exists(google_equipos_id_file)){
  old_id <- readLines(
    google_equipos_id_file,
    warn = FALSE
  )

  if(length(old_id)>0){
    old_id <- trimws(
      old_id[1]
    )

    if(old_id!=""){
      google_equipos <- tryCatch({
        ss <- as_sheets_id(
          old_id
        )

        #access
        gs4_get(
          ss
        )

        ss

      },error=function(e){
        NULL
      })
    }
  }
}

#crear libro equipo
if(is.null(google_equipos)){
  google_equipos <- gs4_create(
    "Evaluaciones rúbricas UAEH - Equipos",

    sheets = list(
      Calificaciones_Equipos = empty_equipos,
      Promedio_Equipos = empty_prom_equipos
    )
  )

  #save id
  writeLines(
    as.character(
      google_equipos
    ),
    google_equipos_id_file
  )
}

#por si alguien borra algo
current_sheets <- sheet_names(
  google_equipos
)

if(!("Calificaciones_Equipos" %in% current_sheets)){
  sheet_write(
    empty_equipos,
    ss = google_equipos,
    sheet = "Calificaciones_Equipos"
  )
}

if(!("Promedio_Equipos" %in% current_sheets)){
  sheet_write(
    empty_prom_equipos,
    ss = google_equipos,
    sheet = "Promedio_Equipos"
  )
}

#************************************************************************
#GOOGLE SHEET ALUMNOS

google_alumnos_id_file <- file.path(
  getwd(),
  "google_sheet_alumnos_id.txt"
)

google_alumnos <- NULL

if(file.exists(google_alumnos_id_file)){
  old_id <- readLines(
    google_alumnos_id_file,
    warn = FALSE
  )

  if(length(old_id)>0){
    old_id <- trimws(
      old_id[1]
    )

    if(old_id!=""){
      google_alumnos <- tryCatch({
        ss <- as_sheets_id(
          old_id
        )

        gs4_get(
          ss
        )

        ss

      },error=function(e){
        NULL
      })
    }
  }
}

#student book
if(is.null(google_alumnos)){
  google_alumnos <- gs4_create(
    "Evaluaciones rúbricas UAEH - Alumnos",

    sheets = list(
      Calificaciones_Alumnos = empty_alumnos,
      Promedio_Alumnos = empty_prom_alumnos
    )
  )

  writeLines(
    as.character(
      google_alumnos
    ),
    google_alumnos_id_file
  )
}

current_sheets <- sheet_names(
  google_alumnos
)

if(!("Calificaciones_Alumnos" %in% current_sheets)){
  sheet_write(
    empty_alumnos,
    ss = google_alumnos,
    sheet = "Calificaciones_Alumnos"
  )
}

if(!("Promedio_Alumnos" %in% current_sheets)){
  sheet_write(
    empty_prom_alumnos,
    ss = google_alumnos,
    sheet = "Promedio_Alumnos"
  )
}

#************************************************************************
#FUNCIONES

#calif finalshiny::runApp("C:/Users/Moonl/Documents/GitHub/PP-Proyectos/SISTEMA-RUBRICA")
get_final <- function(grado){
  sum(
    grado * rubrica$Peso
  ) / 100
}

#read sheets
read_old <- function(ss,sheet){

  tryCatch({
    if(!(sheet %in% sheet_names(ss))){
      return(
        data.frame()
      )
    }

    x <- read_sheet(
      ss,
      sheet = sheet,
      show_col_types = FALSE
    )

    as.data.frame(
      x
    )

  },error=function(e){
    data.frame()
  })
}

#************************************************************************
#PROMEDIOS
#mandar los datos directo
actualiza_promedios <- function(datos_eq=NULL,datos_al=NULL){

########################################################################
  #EQUIPOS

  if(is.null(datos_eq)){
    datos_eq <- read_old(
      google_equipos,
      "Calificaciones_Equipos"
    )
  }

  prom_eq <- data.frame(
    Equipo = as.character(
      1:8
    ),
    Promedio = rep(
      NA_real_,
      8
    ),
    stringsAsFactors = FALSE
  )


  if(nrow(datos_eq)>0){
    datos_eq$Nota_Final <- suppressWarnings(
      as.numeric(
        datos_eq$Nota_Final
      )
    )

    datos_eq$Equipo <- suppressWarnings(
      as.integer(
        datos_eq$Equipo
      )
    )

    for(i in 1:8){
      grado <- datos_eq$Nota_Final[
        datos_eq$Equipo == i
      ]

      grado <- grado[
        !is.na(grado)
      ]

      if(length(grado)>0){
        prom_eq$Promedio[i] <- round(
          mean(
            grado
          ),
          2
        )
      }
    }
  }

  #promedio de todos los equipos
  general_eq <- mean(
    prom_eq$Promedio,
    na.rm = TRUE
  )

  if(is.nan(general_eq)){
    general_eq <- NA_real_
  }

  prom_eq <- rbind(
    prom_eq,

    data.frame(
      Equipo = "PROMEDIO GENERAL",

      Promedio = round(
        general_eq,
        2
      ),
      stringsAsFactors = FALSE
    )
  )

  #rewrite la tab
  sheet_write(
    prom_eq,
    ss = google_equipos,
    sheet = "Promedio_Equipos"
  )

########################################################################
  #ALUMNOS

  if(is.null(datos_al)){
    datos_al <- read_old(
      google_alumnos,
      "Calificaciones_Alumnos"
    )
  }

  #empieza con todos aunque no tengan nada
  prom_al <- alumnos
  prom_al$Promedio <- NA_real_

  if(nrow(datos_al)>0){
    datos_al$Nota_Final <- suppressWarnings(
      as.numeric(
        datos_al$Nota_Final
      )
    )

    for(i in 1:nrow(prom_al)){

      estudiante <- prom_al$Alumno[i]

      grado <- datos_al$Nota_Final[
        trimws(
          datos_al$Alumno
        ) == trimws(
          estudiante
        )
      ]

      grado <- grado[
        !is.na(grado)
      ]

      if(length(grado)>0){

        prom_al$Promedio[i] <- round(
          mean(
            grado
          ),
          2
        )
      }
    }
  }

  #promedio DE TODOS los estudiantes que ya tengan calificacion
  general_al <- mean(
    prom_al$Promedio,
    na.rm = TRUE
  )

  if(is.nan(general_al)){
    general_al <- NA_real_
  }

  #row final
  prom_al <- rbind(
    prom_al,

    data.frame(
      Equipo = NA,
      Alumno = "PROMEDIO GENERAL",

      Promedio = round(
        general_al,
        2
      ),

      stringsAsFactors = FALSE
    )
  )

  sheet_write(
    prom_al,
    ss = google_alumnos,
    sheet = "Promedio_Alumnos"
  )


  list(
    equipos = prom_eq,
    alumnos = prom_al
  )
}

#************************************************************************
#UI

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = "Rubricas"
  ),

  dashboardSidebar(
    sidebarMenu(

      menuItem(
        "Evaluar equipos",
        tabName = "evaluacion",
        icon = icon("users")
      ),

      menuItem(
        "Resultados",
        tabName = "resultados",
        icon = icon("bar-chart")
      )
    )
  ),

  dashboardBody(
    tabItems(

      ######################################################################
      #EVALUACION

      tabItem(
        tabName = "evaluacion",

        fluidRow(

          box(
            title = "Datos de evaluación",
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

            selectInput(
              "equipo",
              "Equipo:",

              choices = setNames(
                as.character(
                  1:8
                ),

                paste(
                  "Equipo",
                  1:8
                )
              ),

              selected = "1"
            ),

            hr(),

            #estudiantes segun equipo
            uiOutput(
              "integrantes_ui"
            ),

            hr(),

            tags$small(
              "Todos los estudiantes seleccionados tendrán la misma calificación"
            ),

            br(),
            br(),

            actionButton(
              "guardar",
              "Enviar Calificaciones",

              icon = icon("paper-plane"),
              class = "btn-success"
            )
          ),


          box(
            title = "Rubrica",
            status = "warning",

            solidHeader = TRUE,
            width = 8,

            uiOutput(
              "inputs_grado"
            )
          )
        ),


        fluidRow(

          box(
            title = "Resultado",
            status = "info",
            solidHeader = TRUE,
            width = 12,

            tableOutput(
              "preview"
            ),

            h3(
              align = "right",

              textOutput(
                "preview_total"
              )
            )
          )
        )
      ),

      ######################################################################
      #RESULTADOS

      tabItem(
        tabName = "resultados",

        fluidRow(

          box(
            title = "Promedios",
            status = "primary",
            solidHeader = TRUE,
            width = 4,

            h4(
              "Promedio general"
            ),

            h2(
              textOutput(
                "promedio_general"
              )
            ),

            hr(),

            selectInput(
              "tipo_grafica",
              "Mostrar grafica de:",

              choices = c(
                "Equipos" = "equipos",
                "Alumnos" = "alumnos"
              ),

              selected = "equipos"
            )
          ),


          box(
            title = "Grafica de Calificaciones",
            status = "info",
            solidHeader = TRUE,
            width = 8,

            plotOutput(
              "grafica",
              height = "450px"
            )
          )
        ),


        fluidRow(

          box(
            title = "Promedio por Equipo",
            status = "success",
            solidHeader = TRUE,
            width = 12,

            tableOutput(
              "tabla_promedios"
            )
          )
        )
      )
    )
  )
)


#************************************************************************
#SERVER

server <- function(input,output,session){

#************************************************************************
#DATOS GUARDADOS

  #leer primero normal antes de hacerlo reactive
  datos_equipos_inicio <- read_old(
    google_equipos,
    "Calificaciones_Equipos"
  )

  datos_alumnos_inicio <- read_old(
    google_alumnos,
    "Calificaciones_Alumnos"
  )

  inicio <- actualiza_promedios(
    datos_equipos_inicio,
    datos_alumnos_inicio
  )

  #reactive
  guardados <- reactiveValues(
    equipos = datos_equipos_inicio,

    alumnos = datos_alumnos_inicio,

    prom_equipos = inicio$equipos,

    prom_alumnos = inicio$alumnos
  )

#************************************************************************

  output$integrantes_ui <- renderUI({

    req(
      input$equipo
    )

    equipo_actual <- as.integer(
      input$equipo
    )

    miembros <- alumnos$Alumno[
      alumnos$Equipo == equipo_actual
    ]

    checkboxGroupInput(
      "integrantes",
      "Integrantes del equipo:",
      choices = miembros,
      selected = miembros
    )
  })


#************************************************************************
#RUBRICA

  #nomas 5
  output$inputs_grado <- renderUI({

    cosas <- list()

    for(i in 1:nrow(rubrica)){
      cosas[[i]] <- div(
        h4(
          paste0(
            i,
            ". ",
            rubrica$Aspecto[i],
            " (",
            rubrica$Peso[i],
            "%)"
          )
        ),

        tags$small(
          rubrica$Descripcion[i]
        ),

        br(),
        br(),

        numericInput(
          paste0(
            "aspecto",
            i
          ),

          "Calificacion:",

          value = 10,
          min = 0,
          max = 10,
          step = 0.5
        ),

        hr()
      )
    }

    do.call(
      tagList,
      cosas
    )
  })


  #************************************************************************
  #GRADOS ACTUALES

  grado_actual <- reactive({

    grado <- rep(
      10,
      nrow(rubrica)
    )


    for(i in 1:nrow(rubrica)){
      n <- input[[
        paste0(
          "aspecto",
          i
        )
      ]]

      if(is.null(n)){
        n <- 10
      }

      grado[i] <- n
    }

    grado
  })

  #************************************************************************
  #PREVIEW

  output$preview <- renderTable({

    grado <- grado_actual()

    data.frame(
      Aspecto = rubrica$Aspecto,
      `Peso (%)` = rubrica$Peso,
      Calificacion = grado,
      Aporte = round(
        grado *
        rubrica$Peso /
        100,
        2
      ),

      check.names = FALSE
    )

  },digits = 2)

  output$preview_total <- renderText({
    paste(
      "Calificacion Final:",

      formatC(
        get_final(
          grado_actual()
        ),

        format = "f",
        digits = 2
      ),

      "/ 10"
    )
  })

#************************************************************************
#SAVE
  observeEvent(input$guardar,{

    if(trimws(input$profesor)==""){
      showNotification(
        "Falta el profesor evaluador.",
        type = "error"
      )
      return()
    }


    if(trimws(input$materia)==""){
      showNotification(
        "Falta la materia o grupo.",
        type = "error"
      )
      return()
    }


    if(
      is.null(input$integrantes) ||
      length(input$integrantes)==0
    ){

      showNotification(
        "Selecciona al menos un integrante.",
        type = "error"
      )

      return()
    }

    grado <- grado_actual()

    if(
      any(
        grado < 0 |
        grado > 10 |
        is.na(grado)
      )
    ){

      showNotification(
        "El grado debe estar entre 0 y 10.",
        type = "error"
      )
      return()
    }


############################################################################
    #id
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

############################################################################

    equipo_actual <- as.integer(
      input$equipo
    )

    miembros <- input$integrantes

    nota_final <- round(
      get_final(
        grado
      ),
      2
    )

    #EQUIPO
    nuevo_equipo <- data.frame(

      ID = id,
      Fecha = fecha,

      Profesor = trimws(
        input$profesor
      ),

      Materia_Grupo = trimws(
        input$materia
      ),

      Equipo = equipo_actual,

      Integrantes = paste(
        miembros,
        collapse = ", "
      ),

      Comprension = grado[1],

      Prototipo = grado[2],

      Modelo = grado[3],

      Tecnologia = grado[4],

      Comunicacion = grado[5],

      Nota_Final = nota_final,

      stringsAsFactors = FALSE
    )

    #ESTUDIANTES
    #todos los seleccionados tienen EXACTAMENTE la misma
    nuevos_alumnos <- data.frame(

      ID = rep(
        id,
        length(miembros)
      ),

      Fecha = rep(
        fecha,
        length(miembros)
      ),

      Profesor = rep(
        trimws(
          input$profesor
        ),

        length(miembros)
      ),

      Materia_Grupo = rep(
        trimws(
          input$materia
        ),

        length(miembros)
      ),

      Equipo = rep(
        equipo_actual,
        length(miembros)
      ),

      Alumno = miembros,

      Comprension = rep(
        grado[1],
        length(miembros)
      ),

      Prototipo = rep(
        grado[2],
        length(miembros)
      ),

      Modelo = rep(
        grado[3],
        length(miembros)
      ),

      Tecnologia = rep(
        grado[4],
        length(miembros)
      ),

      Comunicacion = rep(
        grado[5],
        length(miembros)
      ),

      Nota_Final = rep(
        nota_final,
        length(miembros)
      ),

      stringsAsFactors = FALSE
    )

###########################################################
#GOOGLE

    resultado <- tryCatch({

      #equipos
      sheet_append(
        google_equipos,
        nuevo_equipo,
        sheet = "Calificaciones_Equipos"
      )

      #alumnos
      sheet_append(
        google_alumnos,
        nuevos_alumnos,
        sheet = "Calificaciones_Alumnos"
      )

      ######################################################################
      #UPDATE LOCAL FIRST

      if(nrow(guardados$equipos)==0){
        guardados$equipos <- nuevo_equipo
      }else{
        guardados$equipos <- rbind(
          guardados$equipos,
          nuevo_equipo
        )
      }

      if(nrow(guardados$alumnos)==0){
        guardados$alumnos <- nuevos_alumnos
      }else{
        guardados$alumnos <- rbind(
          guardados$alumnos,
          nuevos_alumnos
        )
      }

      #PROMEDIOS

      nuevos_promedios <- actualiza_promedios(
        guardados$equipos,
        guardados$alumnos
      )

      guardados$prom_equipos <- nuevos_promedios$equipos
      guardados$prom_alumnos <- nuevos_promedios$alumnos

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

    showNotification(
      paste0(
        "Calificacion enviada. Equipo ",
        equipo_actual,
        ": ",
        nota_final
      ),
      type = "message",
      duration = 6
    )
  })


#************************************************************************
#RESULTADOS

  #tabla equipos
  output$tabla_promedios <- renderTable({
    req(
      guardados$prom_equipos
    )
    guardados$prom_equipos
  },digits = 2)

  #promedio TODOS los alumnos
  output$promedio_general <- renderText({

    req(
      guardados$prom_alumnos
    )

    x <- guardados$prom_alumnos[
      guardados$prom_alumnos$Alumno == "PROMEDIO GENERAL",
    ]

    if(nrow(x)==0 || is.na(x$Promedio[1])){
      return(
        "Sin calificaciones"
      )
    }

    paste0(
      formatC(
        x$Promedio[1],
        format = "f",
        digits = 2
      ),
      " / 10"
    )
  })

#************************************************************************
#GRAFICA

  output$grafica <- renderPlot({
    req(
      input$tipo_grafica
    )

    ########################################################################
    #EQUIPOS

    if(input$tipo_grafica=="equipos"){
      datos <- guardados$prom_equipos[
        guardados$prom_equipos$Equipo != "PROMEDIO GENERAL",
      ]

      datos$Promedio <- suppressWarnings(
        as.numeric(
          datos$Promedio
        )
      )

      datos <- datos[
        !is.na(datos$Promedio),
      ]

      if(nrow(datos)==0){
        plot.new()
        text(
          0.5,
          0.5,
          "No hay calificaciones todavia"
        )
        return()
      }

      barplot(
        datos$Promedio,
        names.arg = paste(
          "Equipo",
          datos$Equipo
        ),

        ylim = c(
          0,
          10
        ),

        main = "Promedio por Equipo",
        xlab = "Equipos",
        ylab = "Calificacion"
      )

      abline(
        h = mean(
          datos$Promedio,
          na.rm = TRUE
        ),
        lty = 2,
        lwd = 2
      )
    }

    ########################################################################
    #ALUMNOS

    if(input$tipo_grafica=="alumnos"){

      datos <- guardados$prom_alumnos[
        guardados$prom_alumnos$Alumno != "PROMEDIO GENERAL",
      ]

      datos$Promedio <- suppressWarnings(
        as.numeric(
          datos$Promedio
        )
      )

      datos <- datos[
        !is.na(datos$Promedio),
      ]

      if(nrow(datos)==0){

        plot.new()
        text(
          0.5,
          0.5,
          "No hay calificaciones todavia"
        )
        return()
      }

      #horizontal pq son muchos nombres
      par(
        mar = c(
          5,
          12,
          4,
          2
        )
      )

      barplot(
        datos$Promedio,

        names.arg = datos$Alumno,
        horiz = TRUE,
        las = 1,
        xlim = c(
          0,
          10
        ),
        main = "Promedio por Alumno",
        xlab = "Calificacion"
      )

      abline(
        v = mean(
          datos$Promedio,
          na.rm = TRUE
        ),
        lty = 2,
        lwd = 2
      )
    }
  })
}

shinyApp(
  ui,
  server
)