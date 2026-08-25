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
    "Comprension conceptual",

    "Calidad y funcionamiento del prototipo",

    "Modelo matematico y analisis",

    "Uso de tecnologia y recursos",

    "Comunicacion y trabajo en equipo"
  ),

  Descripcion = c(
    "Dominio del principio de conservacion de la energia y su aplicacion al sistema.",

    "Funcionamiento del dispositivo, estabilidad y coherencia con el objetivo.",

    "Relacion entre las ecuaciones teoricas y el comportamiento experimental.",

    "Uso de software, materiales y creatividad en el diseño.",

    "Claridad, organizacion y participacion equilibrada durante la presentacion."
  ),


  Peso = c(
    25,
    25,
    20,
    20,
    10
  ),
  stringsAsFactors = FALSE
)


#comprobante
if(sum(rubrica$Peso)!=100){
  stop(
    "La suma de la rubrica debe ser igual a 100"
  )
}


#************************************************************************
#TABLA PARA GUARAR LAS CALIFICACIONES

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

#filas pa los alumnas
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


#promedio
empty_prom_equipos <- data.frame(
  Equipo = as.character(1:8),

  Promedio = rep(
    NA_real_,
    8
  ),
  stringsAsFactors = FALSE
)

#promediar y que no esten vacias
empty_prom_alumnos <- alumnos
empty_prom_alumnos$Promedio <- NA_real_


#************************************************************************

#GOOGLE SHEET

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

    if(old_id!= ""){
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

#crear el libro
if(is.null(google_equipos)){
  google_equipos <- gs4_create(
    "Evaluaciones Rubricas UAEH - Equipos",
    sheets = list(

      Calificaciones_Equipos = empty_equipos,
      Promedio_Equipos = empty_prom_equipos
    )
  )


  #save id local
  writeLines(
    as.character(
      google_equipos
    ),
    google_equipos_id_file
  )
}

#deletes a tab smth
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

  if(length(old_id) > 0){
    old_id <- trimws(
      old_id[1]
    )

    if(old_id != ""){
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
    "Evaluaciones Rubricas UAEH - Alumnos",
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

#la calificación final
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

#recalculate both average tabs
actualiza_promedios <- function(){


#teams

  datos_eq <- read_old(
    google_equipos,
    "Calificaciones_Equipos"
  )

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

  if(nrow(datos_eq) > 0){

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

      if(length(grado) > 0){
        prom_eq$Promedio[i] <- round(
          mean(
            grado
          ),
          2
        )
      }
    }
  }

  #promedio ALL teams
  general <- mean(
    prom_eq$Promedio,
    na.rm = TRUE
  )

  if(is.nan(general)){
    general <- NA_real_
  }

  prom_eq <- rbind(
    prom_eq,
    data.frame(
      Equipo = "PROMEDIO GENERAL",

      Promedio = round(
        general,
        2
      ),
      stringsAsFactors = FALSE
    )
  )


  sheet_write(
    prom_eq,

    ss = google_equipos,
    sheet = "Promedio_Equipos"
  )

#estudiantes
  datos_al <- read_old(
    google_alumnos,

    "Calificaciones_Alumnos"
  )

  #always start with the complete student list
  prom_al <- alumnos
  prom_al$Promedio <- NA_real_

  if(nrow(datos_al) > 0){
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

      if(length(grado) > 0){

        prom_al$Promedio[i] <- round(
          mean(
            grado
          ),
          2
        )
      }
    }
  }

  sheet_write(
    prom_al,
    ss = google_alumnos,
    sheet = "Promedio_Alumnos"
  )
}

actualiza_promedios()

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

        "Evaluar Equipo",
        tabName = "evaluacion",
        icon = icon("users")
      )
    )
  ),

  dashboardBody(
    tabItems(
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
      )
    )
  )
)


#************************************************************************
#SERVER

server <- function(input,output,session){

  #TEAM STUDENTS
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
  #grado ACRTUALES

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


    #basic checks
    if(trimws(input$profesor) == ""){

      showNotification(

        "Falta el profesor evaluador.",

        type = "error"

      )

      return()

    }


    if(trimws(input$materia) == ""){

      showNotification(

        "Falta la materia o grupo.",

        type = "error"

      )

      return()

    }


    if(
      is.null(input$integrantes) ||
      length(input$integrantes) == 0
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

        "Las grado deben estar entre 0 y 10.",

        type = "error"

      )

      return()

    }



    #id for this evaluation
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



    #************************************************************************
    #TEAM ROW

    #only ONE row goes to the team book
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



    #************************************************************************
    #STUDENT ROWS

    #every selected student gets the SAME grades
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



    #************************************************************************
    #SEND TO GOOGLE

    resultado <- tryCatch({


      #one grade per team
      sheet_append(

        google_equipos,

        nuevo_equipo,

        sheet = "Calificaciones_Equipos"

      )


      #same grade repeated for every selected student
      sheet_append(

        google_alumnos,

        nuevos_alumnos,

        sheet = "Calificaciones_Alumnos"

      )


      #recalculate averages after saving
      actualiza_promedios()


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

}


shinyApp(
  ui,
  server
)