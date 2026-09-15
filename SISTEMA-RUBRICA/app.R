# shiny::runApp("C:/Users/PC/Documents/GitHub/PP-Proyectos/SISTEMA-RUBRICA")
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(readxl)

#************************************************************************
#GOOGLE

google_email <- "mi463667@uaeh.edu.mx"

gs4_auth(
  email = google_email
)


#************************************************************************
#SEMESTRE

#enero-junio / julio-diciembre

get_semestre <- function(fecha=Sys.Date()){

  mes <- as.integer(
    format(
      fecha,
      "%m"
    )
  )

  anio <- format(
    fecha,
    "%Y"
  )


  if(mes <= 6){

    paste(
      "Enero - Junio",
      anio
    )

  }else{

    paste(
      "Julio - Diciembre",
      anio
    )
  }
}


semestre <- get_semestre()


#************************************************************************
#RUBRICA ACTUAL

#esta SI se usa para calificar
#scale 0-100

rubrica <- data.frame(

  Aspecto = c(
    "Memoria Tecnica",
    "Simulacion Computacional",
    "Defensa Oral",
    "Documentacion y Codigo"
  ),

  Peso = c(
    30,
    30,
    35,
    5
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
#RUBRICA CORTA

#solo reference
#se puede ver desde admin

rubrica_corta <- data.frame(

  Numero = c(
    1,
    2,
    3,
    4,
    5
  ),

  Aspecto = c(
    "Comprension conceptual",
    "Calidad y funcionamiento del prototipo",
    "Modelo matematico y analisis",
    "Uso de tecnologia y recursos",
    "Comunicacion y trabajo en equipo"
  ),

  Peso = c(
    25,
    25,
    20,
    20,
    10
  ),

  Descripcion = c(

    "Dominio del principio de conservacion de la energia y su aplicacion al sistema.",

    "Funcionamiento del dispositivo, estabilidad y coherencia con el objetivo.",

    "Relacion entre las ecuaciones teoricas y el comportamiento experimental.",

    "Uso de software, materiales, herramientas y creatividad en el diseño.",

    "Claridad, organizacion y participacion equilibrada durante la presentacion."

  ),

  stringsAsFactors = FALSE
)


#************************************************************************
#RUBRICA LARGA

#PC + UT
#reference para admin

rubrica_larga <- data.frame(

  Numero = c(
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8
  ),

  Rubrica = c(
    "P.C.",
    "P.C.",
    "P.C.",
    "P.C.",
    "P.C.",
    "U.T.",
    "U.T.",
    "U.T."
  ),

  Aspecto = c(

    "Extraer informacion y adaptar los problemas",

    "Aplica principios matematicos y fisicos",

    "Evaluacion de las limitaciones",

    "Identifican las necesidades de formacion continua",

    "Desarrollan un plan de formacion continua",

    "Seleccion de la herramienta TIC",

    "Uso de herramientas TIC",

    "Evaluacion de las limitaciones de las herramientas TIC"

  ),

  Relacion_Corta = c(

    "1 - Comprension conceptual",

    "3 - Modelo matematico y analisis",

    "3 - Modelo matematico y analisis",

    "4 - Uso de tecnologia y recursos",

    "4 - Uso de tecnologia y recursos",

    "4 - Uso de tecnologia y recursos",

    "4 - Uso de tecnologia y recursos",

    "4 - Uso de tecnologia y recursos"

  ),

  stringsAsFactors = FALSE
)


#************************************************************************
#EMPTY TABLES

empty_equipos <- data.frame(

  ID = character(),
  Fecha = character(),
  Semestre = character(),

  Profesor = character(),
  Materia_Grupo = character(),

  Equipo = character(),
  Integrantes = character(),

  Memoria = numeric(),
  Simulacion = numeric(),
  Defensa = numeric(),
  Codigo = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


empty_alumnos <- data.frame(

  ID = character(),
  Fecha = character(),
  Semestre = character(),

  Profesor = character(),
  Materia_Grupo = character(),

  Equipo = character(),
  Alumno = character(),

  Memoria = numeric(),
  Simulacion = numeric(),
  Defensa = numeric(),
  Codigo = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


empty_prom_equipos <- data.frame(

  Equipo = character(),

  Memoria = numeric(),
  Simulacion = numeric(),
  Defensa = numeric(),
  Codigo = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


empty_prom_alumnos <- data.frame(

  Equipo = character(),
  Alumno = character(),

  Memoria = numeric(),
  Simulacion = numeric(),
  Defensa = numeric(),
  Codigo = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


#lista administrativa
empty_lista <- data.frame(

  Equipo = character(),
  Alumno = character(),
  Activo = character(),

  stringsAsFactors = FALSE
)


#registro de semestres
empty_semestres <- data.frame(

  Semestre = character(),

  Libro_Equipos_ID = character(),
  Libro_Alumnos_ID = character(),

  Fecha_Creacion = character(),

  stringsAsFactors = FALSE
)


#************************************************************************
#FUNCTIONS

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


#calificacion final
get_final <- function(grado){

  sum(
    grado *
    rubrica$Peso
  ) / 100
}


#average without nan

promedio <- function(x){

  x <- suppressWarnings(

    as.numeric(
      x
    )
  )


  x <- x[
    !is.na(x)
  ]


  if(length(x)==0){

    return(
      NA_real_
    )
  }


  round(
    mean(x),
    2
  )
}


#************************************************************************
#GOOGLE ADMIN BOOK

#este libro no cambia por semestre

google_config_id_file <- file.path(

  getwd(),

  "google_config_id.txt"
)


google_config <- NULL


if(file.exists(google_config_id_file)){

  old_id <- readLines(

    google_config_id_file,

    warn = FALSE
  )


  if(length(old_id)>0){

    old_id <- trimws(
      old_id[1]
    )


    if(old_id!=""){

      google_config <- tryCatch({

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


#create admin book

if(is.null(google_config)){

  google_config <- gs4_create(

    "Administracion Rubricas UAEH",

    sheets = list(

      Semestres = empty_semestres
    )
  )


  writeLines(

    as.character(
      google_config
    ),

    google_config_id_file
  )
}


#************************************************************************
#ADMIN SHEETS

current_config_sheets <- sheet_names(
  google_config
)


#student list for current semester

if(!(semestre %in% current_config_sheets)){

  sheet_write(

    empty_lista,

    ss = google_config,

    sheet = semestre
  )
}


if(!("Semestres" %in% sheet_names(google_config))){

  sheet_write(

    empty_semestres,

    ss = google_config,

    sheet = "Semestres"
  )
}


#************************************************************************
#CURRENT SEMESTER BOOKS

get_libros_semestre <- function(){


  registro <- read_old(

    google_config,

    "Semestres"
  )


  equipo_id <- NULL
  alumno_id <- NULL


  if(nrow(registro)>0){

    registro$Semestre <- as.character(
      registro$Semestre
    )


    fila <- which(

      trimws(
        registro$Semestre
      ) == semestre
    )


    if(length(fila)>0){

      fila <- fila[1]


      equipo_id <- as.character(
        registro$Libro_Equipos_ID[fila]
      )


      alumno_id <- as.character(
        registro$Libro_Alumnos_ID[fila]
      )


      equipo_ok <- tryCatch({

        gs4_get(
          as_sheets_id(
            equipo_id
          )
        )

        TRUE

      },error=function(e){

        FALSE
      })


      alumno_ok <- tryCatch({

        gs4_get(
          as_sheets_id(
            alumno_id
          )
        )

        TRUE

      },error=function(e){

        FALSE
      })


      if(!equipo_ok){

        equipo_id <- NULL
      }


      if(!alumno_ok){

        alumno_id <- NULL
      }
    }
  }


  #************************************************************************
  #TEAM BOOK

  if(
    is.null(equipo_id) ||
    is.na(equipo_id) ||
    equipo_id==""
  ){

    nuevo <- gs4_create(

      paste(
        "Evaluaciones Rubricas UAEH - Equipos -",
        semestre
      ),

      sheets = list(

        Calificaciones = empty_equipos,

        Promedios = empty_prom_equipos
      )
    )


    equipo_id <- as.character(
      nuevo
    )
  }


  #************************************************************************
  #STUDENT BOOK

  if(
    is.null(alumno_id) ||
    is.na(alumno_id) ||
    alumno_id==""
  ){

    nuevo <- gs4_create(

      paste(
        "Evaluaciones Rubricas UAEH - Alumnos -",
        semestre
      ),

      sheets = list(

        Calificaciones = empty_alumnos,

        Promedios = empty_prom_alumnos
      )
    )


    alumno_id <- as.character(
      nuevo
    )
  }


  #************************************************************************
  #SAVE IDS

  if(nrow(registro)==0){

    registro <- empty_semestres
  }


  fila <- which(

    as.character(
      registro$Semestre
    ) == semestre
  )


  if(length(fila)==0){

    registro <- rbind(

      registro,

      data.frame(

        Semestre = semestre,

        Libro_Equipos_ID = equipo_id,

        Libro_Alumnos_ID = alumno_id,

        Fecha_Creacion = as.character(
          Sys.Date()
        ),

        stringsAsFactors = FALSE
      )
    )


  }else{

    fila <- fila[1]

    registro$Libro_Equipos_ID[fila] <- equipo_id

    registro$Libro_Alumnos_ID[fila] <- alumno_id
  }


  sheet_write(

    registro,

    ss = google_config,

    sheet = "Semestres"
  )


  list(

    equipos = as_sheets_id(
      equipo_id
    ),

    alumnos = as_sheets_id(
      alumno_id
    )
  )
}


libros <- get_libros_semestre()


google_equipos <- libros$equipos

google_alumnos <- libros$alumnos


#************************************************************************
#CHECK BOOK TABS

if(!("Calificaciones" %in% sheet_names(google_equipos))){

  sheet_write(

    empty_equipos,

    ss = google_equipos,

    sheet = "Calificaciones"
  )
}


if(!("Promedios" %in% sheet_names(google_equipos))){

  sheet_write(

    empty_prom_equipos,

    ss = google_equipos,

    sheet = "Promedios"
  )
}


if(!("Calificaciones" %in% sheet_names(google_alumnos))){

  sheet_write(

    empty_alumnos,

    ss = google_alumnos,

    sheet = "Calificaciones"
  )
}


if(!("Promedios" %in% sheet_names(google_alumnos))){

  sheet_write(

    empty_prom_alumnos,

    ss = google_alumnos,

    sheet = "Promedios"
  )
}


#************************************************************************
#URLS

google_config_url <- paste0(

  "https://docs.google.com/spreadsheets/d/",

  as.character(
    google_config
  ),

  "/edit"
)


google_equipos_url <- paste0(

  "https://docs.google.com/spreadsheets/d/",

  as.character(
    google_equipos
  ),

  "/edit"
)


google_alumnos_url <- paste0(

  "https://docs.google.com/spreadsheets/d/",

  as.character(
    google_alumnos
  ),

  "/edit"
)


#************************************************************************
#GET CURRENT STUDENTS

get_alumnos <- function(){


  x <- read_old(

    google_config,

    semestre
  )


  if(nrow(x)==0){

    return(

      data.frame(

        Equipo = character(),

        Alumno = character(),

        stringsAsFactors = FALSE
      )
    )
  }


  if(!("Equipo" %in% names(x))){

    return(

      data.frame(

        Equipo = character(),

        Alumno = character(),

        stringsAsFactors = FALSE
      )
    )
  }


  if(!("Alumno" %in% names(x))){

    return(

      data.frame(

        Equipo = character(),

        Alumno = character(),

        stringsAsFactors = FALSE
      )
    )
  }


  if(!("Activo" %in% names(x))){

    x$Activo <- "SI"
  }


  x$Equipo <- as.character(
    x$Equipo
  )

  x$Alumno <- as.character(
    x$Alumno
  )

  x$Activo <- as.character(
    x$Activo
  )


  x <- x[

    toupper(
      trimws(
        x$Activo
      )
    ) == "SI",

  ]


  x <- x[
    ,
    c(
      "Equipo",
      "Alumno"
    )
  ]


  x$Equipo <- trimws(
    x$Equipo
  )

  x$Alumno <- trimws(
    x$Alumno
  )


  x <- x[

    !is.na(x$Equipo) &
    !is.na(x$Alumno) &
    x$Equipo != "" &
    x$Alumno != "",

  ]


  unique(
    x
  )
}


#************************************************************************
#READ STUDENT FILE

leer_lista <- function(path,name){


  ext <- tolower(

    tools::file_ext(
      name
    )
  )


  if(ext=="csv"){

    x <- read.csv(

      path,

      stringsAsFactors = FALSE,

      check.names = FALSE
    )


  }else{

    x <- as.data.frame(

      read_excel(
        path
      )
    )
  }


  nombres <- names(
    x
  )


  nombres_limpios <- tolower(

    gsub(

      "[^a-z0-9]",

      "",

      iconv(

        nombres,

        to = "ASCII//TRANSLIT"
      )
    )
  )


  col_equipo <- which(

    nombres_limpios %in% c(

      "equipo",

      "team"
    )
  )


  col_alumno <- which(

    nombres_limpios %in% c(

      "alumno",

      "estudiante",

      "nombre",

      "nombrealumno"
    )
  )


  col_activo <- which(

    nombres_limpios %in% c(

      "activo",

      "active"
    )
  )


  if(
    length(col_equipo)==0 ||
    length(col_alumno)==0
  ){

    stop(
      "El archivo debe tener las columnas Equipo y Alumno"
    )
  }


  lista <- data.frame(

    Equipo = as.character(
      x[[col_equipo[1]]]
    ),

    Alumno = as.character(
      x[[col_alumno[1]]]
    ),

    stringsAsFactors = FALSE
  )


  if(length(col_activo)>0){

    lista$Activo <- as.character(
      x[[col_activo[1]]]
    )

  }else{

    lista$Activo <- "SI"
  }


  lista$Equipo <- trimws(
    lista$Equipo
  )

  lista$Alumno <- trimws(
    lista$Alumno
  )


  lista <- lista[

    !is.na(lista$Equipo) &
    !is.na(lista$Alumno) &
    lista$Equipo != "" &
    lista$Alumno != "",

  ]


  lista$Activo[

    is.na(lista$Activo) |
    lista$Activo==""

  ] <- "SI"


  unique(
    lista
  )
}


#************************************************************************
#AVERAGES

actualiza_promedios <- function(
  datos_eq=NULL,
  datos_al=NULL,
  lista=NULL
){


  if(is.null(lista)){

    lista <- get_alumnos()
  }


  if(is.null(datos_eq)){

    datos_eq <- read_old(

      google_equipos,

      "Calificaciones"
    )
  }


  if(is.null(datos_al)){

    datos_al <- read_old(

      google_alumnos,

      "Calificaciones"
    )
  }


  #************************************************************************
  #NO STUDENTS YET

  if(
    is.null(lista) ||
    nrow(lista)==0
  ){

    prom_eq <- data.frame(

      Equipo = character(),

      Memoria = numeric(),

      Simulacion = numeric(),

      Defensa = numeric(),

      Codigo = numeric(),

      Nota_Final = numeric(),

      stringsAsFactors = FALSE
    )


    prom_al <- data.frame(

      Equipo = character(),

      Alumno = character(),

      Memoria = numeric(),

      Simulacion = numeric(),

      Defensa = numeric(),

      Codigo = numeric(),

      Nota_Final = numeric(),

      stringsAsFactors = FALSE
    )


    sheet_write(

      prom_eq,

      ss = google_equipos,

      sheet = "Promedios"
    )


    sheet_write(

      prom_al,

      ss = google_alumnos,

      sheet = "Promedios"
    )


    return(

      list(

        equipos = prom_eq,

        alumnos = prom_al
      )
    )
  }


  #************************************************************************
  #TEAMS

  equipos <- unique(

    as.character(
      lista$Equipo
    )
  )


  prom_eq <- data.frame(

    Equipo = equipos,

    Memoria = rep(
      NA_real_,
      length(equipos)
    ),

    Simulacion = rep(
      NA_real_,
      length(equipos)
    ),

    Defensa = rep(
      NA_real_,
      length(equipos)
    ),

    Codigo = rep(
      NA_real_,
      length(equipos)
    ),

    Nota_Final = rep(
      NA_real_,
      length(equipos)
    ),

    stringsAsFactors = FALSE
  )


  if(
    nrow(datos_eq)>0 &&
    length(equipos)>0
  ){

    datos_eq$Equipo <- as.character(
      datos_eq$Equipo
    )


    for(i in 1:length(equipos)){

      eq <- equipos[i]


      datos <- datos_eq[

        trimws(
          datos_eq$Equipo
        ) == trimws(
          eq
        ),

      ]


      if(nrow(datos)>0){

        prom_eq$Memoria[i] <- promedio(
          datos$Memoria
        )

        prom_eq$Simulacion[i] <- promedio(
          datos$Simulacion
        )

        prom_eq$Defensa[i] <- promedio(
          datos$Defensa
        )

        prom_eq$Codigo[i] <- promedio(
          datos$Codigo
        )

        prom_eq$Nota_Final[i] <- promedio(
          datos$Nota_Final
        )
      }
    }
  }


  if(nrow(prom_eq)>0){

    prom_eq <- rbind(

      prom_eq,

      data.frame(

        Equipo = "PROMEDIO GENERAL",

        Memoria = promedio(
          prom_eq$Memoria
        ),

        Simulacion = promedio(
          prom_eq$Simulacion
        ),

        Defensa = promedio(
          prom_eq$Defensa
        ),

        Codigo = promedio(
          prom_eq$Codigo
        ),

        Nota_Final = promedio(
          prom_eq$Nota_Final
        ),

        stringsAsFactors = FALSE
      )
    )
  }


  sheet_write(

    prom_eq,

    ss = google_equipos,

    sheet = "Promedios"
  )


  #************************************************************************
  #STUDENTS

  prom_al <- lista


  prom_al$Memoria <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Simulacion <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Defensa <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Codigo <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Nota_Final <- rep(
    NA_real_,
    nrow(prom_al)
  )


  if(
    nrow(datos_al)>0 &&
    nrow(prom_al)>0
  ){

    datos_al$Equipo <- as.character(
      datos_al$Equipo
    )

    datos_al$Alumno <- as.character(
      datos_al$Alumno
    )


    for(i in 1:nrow(prom_al)){

      estudiante <- prom_al$Alumno[i]

      equipo_est <- prom_al$Equipo[i]


      datos <- datos_al[

        trimws(
          datos_al$Alumno
        ) == trimws(
          estudiante
        ) &

        trimws(
          datos_al$Equipo
        ) == trimws(
          equipo_est
        ),

      ]


      if(nrow(datos)>0){

        prom_al$Memoria[i] <- promedio(
          datos$Memoria
        )

        prom_al$Simulacion[i] <- promedio(
          datos$Simulacion
        )

        prom_al$Defensa[i] <- promedio(
          datos$Defensa
        )

        prom_al$Codigo[i] <- promedio(
          datos$Codigo
        )

        prom_al$Nota_Final[i] <- promedio(
          datos$Nota_Final
        )
      }
    }
  }


  if(nrow(prom_al)>0){

    prom_al <- rbind(

      prom_al,

      data.frame(

        Equipo = "",

        Alumno = "PROMEDIO GENERAL",

        Memoria = promedio(
          prom_al$Memoria
        ),

        Simulacion = promedio(
          prom_al$Simulacion
        ),

        Defensa = promedio(
          prom_al$Defensa
        ),

        Codigo = promedio(
          prom_al$Codigo
        ),

        Nota_Final = promedio(
          prom_al$Nota_Final
        ),

        stringsAsFactors = FALSE
      )
    )
  }


  sheet_write(

    prom_al,

    ss = google_alumnos,

    sheet = "Promedios"
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

    title = "Evaluacion de Rubricas"
  ),


  dashboardSidebar(

    sidebarMenu(


      menuItem(

        "Evaluar Equipo",

        tabName = "evaluacion",

        icon = icon("users")
      ),


      menuItem(

        "Resultados",

        tabName = "resultados",

        icon = icon("bar-chart")
      ),


      menuItem(

        "Administracion",

        tabName = "admin",

        icon = icon("lock")
      )

    )

  ),


  dashboardBody(

    tabItems(


      #********************************************************************
      #EVALUATION

      tabItem(

        tabName = "evaluacion",


        fluidRow(


          box(

            title = paste(
              "Evaluacion -",
              semestre
            ),

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


            uiOutput(
              "equipo_ui"
            ),


            hr(),


            uiOutput(
              "integrantes_ui"
            ),


            hr(),


            tags$small(

              "Todos los alumnos seleccionados reciben la misma calificacion."
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


      #********************************************************************
      #RESULTS

      tabItem(

        tabName = "resultados",


        fluidRow(


          box(

            title = paste(
              "Resultados -",
              semestre
            ),

            status = "primary",

            solidHeader = TRUE,

            width = 4,


            selectInput(

              "tipo_grafica",

              "Mostrar:",

              choices = c(

                "Equipos" = "equipos",

                "Alumnos" = "alumnos"
              )

            ),


            selectInput(

              "criterio_grafica",

              "Calificacion:",

              choices = c(

                "Nota Final" = "Nota_Final",

                "Memoria Tecnica" = "Memoria",

                "Simulacion Computacional" = "Simulacion",

                "Defensa Oral" = "Defensa",

                "Documentacion y Codigo" = "Codigo"

              )

            ),


            hr(),


            h4(
              "Promedio general"
            ),


            h2(
              textOutput(
                "promedio_general"
              )
            )

          ),


          box(

            title = "Grafica",

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

            title = "Promedios",

            status = "success",

            solidHeader = TRUE,

            width = 12,


            tableOutput(
              "tabla_promedios"
            )

          )

        )

      ),


      #********************************************************************
      #ADMIN

      tabItem(

        tabName = "admin",


        fluidRow(


          box(

            title = "Administracion",

            status = "danger",

            solidHeader = TRUE,

            width = 12,


            uiOutput(
              "admin_ui"
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
  #LOAD

  lista_inicio <- get_alumnos()


  equipos_inicio <- read_old(

    google_equipos,

    "Calificaciones"
  )


  alumnos_inicio <- read_old(

    google_alumnos,

    "Calificaciones"
  )


  promedios_inicio <- actualiza_promedios(

    equipos_inicio,

    alumnos_inicio,

    lista_inicio
  )


  alumnos_actuales <- reactiveVal(
    lista_inicio
  )


  guardados <- reactiveValues(

    equipos = equipos_inicio,

    alumnos = alumnos_inicio,

    prom_equipos = promedios_inicio$equipos,

    prom_alumnos = promedios_inicio$alumnos
  )


  admin_ok <- reactiveVal(
    FALSE
  )


  #************************************************************************
  #TEAM SELECTOR

  output$equipo_ui <- renderUI({


    lista <- alumnos_actuales()


    if(nrow(lista)==0){

      return(

        tags$p(

          strong(
            "No hay alumnos cargados para este semestre."
          )

        )

      )
    }


    equipos <- unique(
      lista$Equipo
    )


    selectInput(

      "equipo",

      "Equipo:",

      choices = equipos,

      selected = equipos[1]

    )

  })


  #************************************************************************
  #STUDENTS

  output$integrantes_ui <- renderUI({


    lista <- alumnos_actuales()


    if(nrow(lista)==0){

      return(
        NULL
      )
    }


    req(
      input$equipo
    )


    miembros <- lista$Alumno[

      lista$Equipo == input$equipo

    ]


    checkboxGroupInput(

      "integrantes",

      "Integrantes del equipo:",

      choices = miembros,

      selected = miembros

    )

  })


  #************************************************************************
  #RUBRIC

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


        numericInput(

          paste0(
            "aspecto",
            i
          ),

          "Calificacion:",

          value = 100,

          min = 0,

          max = 100,

          step = 5

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
  #CURRENT GRADES

  grado_actual <- reactive({


    grado <- rep(

      100,

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

        n <- 100
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

      "/ 100"

    )

  })


#************************************************************************
#SAVE

  observeEvent(input$guardar,{


    if(nrow(alumnos_actuales())==0){

      showNotification(

        "Primero se debe cargar la lista de alumnos.",

        type = "error"

      )

      return()
    }


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
        grado > 100 |
        is.na(grado)

      )

    ){

      showNotification(

        "Las calificaciones deben estar entre 0 y 100.",

        type = "error"
      )

      return()
    }


############################################################################
#ID

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


    equipo_actual <- as.character(
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
    #TEAM

    nuevo_equipo <- data.frame(

      ID = id,

      Fecha = fecha,

      Semestre = semestre,

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

      Memoria = grado[1],

      Simulacion = grado[2],

      Defensa = grado[3],

      Codigo = grado[4],

      Nota_Final = nota_final,

      stringsAsFactors = FALSE
    )


    #************************************************************************
    #STUDENTS

    nuevos_alumnos <- data.frame(

      ID = rep(
        id,
        length(miembros)
      ),

      Fecha = rep(
        fecha,
        length(miembros)
      ),

      Semestre = rep(
        semestre,
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

      Memoria = rep(
        grado[1],
        length(miembros)
      ),

      Simulacion = rep(
        grado[2],
        length(miembros)
      ),

      Defensa = rep(
        grado[3],
        length(miembros)
      ),

      Codigo = rep(
        grado[4],
        length(miembros)
      ),

      Nota_Final = rep(
        nota_final,
        length(miembros)
      ),

      stringsAsFactors = FALSE
    )


#************************************************************************
#GOOGLE SAVE

    resultado <- tryCatch({


      sheet_append(

        google_equipos,

        nuevo_equipo,

        sheet = "Calificaciones"
      )


      sheet_append(

        google_alumnos,

        nuevos_alumnos,

        sheet = "Calificaciones"
      )


      #************************************************************
      #LOCAL

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


      #************************************************************
      #AVERAGES

      nuevos_promedios <- actualiza_promedios(

        guardados$equipos,

        guardados$alumnos,

        alumnos_actuales()
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
#RESULTS TABLE

  output$tabla_promedios <- renderTable({


    if(input$tipo_grafica=="equipos"){

      guardados$prom_equipos

    }else{

      guardados$prom_alumnos
    }


  },digits = 2)


#************************************************************************
#GENERAL AVERAGE

  output$promedio_general <- renderText({


    if(input$tipo_grafica=="equipos"){

      datos <- guardados$prom_equipos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        return(
          "Sin calificaciones"
        )
      }


      x <- datos[

        datos$Equipo == "PROMEDIO GENERAL",

      ]


    }else{


      datos <- guardados$prom_alumnos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        return(
          "Sin calificaciones"
        )
      }


      x <- datos[

        datos$Alumno == "PROMEDIO GENERAL",

      ]
    }


    if(
      nrow(x)==0 ||
      is.na(x$Nota_Final[1])
    ){

      return(
        "Sin calificaciones"
      )
    }


    paste0(

      formatC(

        x$Nota_Final[1],

        format = "f",

        digits = 2
      ),

      " / 100"
    )

  })


#************************************************************************
#GRAPH

  output$grafica <- renderPlot({


    req(
      input$tipo_grafica,
      input$criterio_grafica
    )


    columna <- input$criterio_grafica


    #************************************************************************
    #TEAMS

    if(input$tipo_grafica=="equipos"){

      datos <- guardados$prom_equipos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        plot.new()

        text(
          0.5,
          0.5,
          "No hay calificaciones todavia"
        )

        return()
      }


      datos <- datos[

        datos$Equipo != "PROMEDIO GENERAL",

      ]


      datos[[columna]] <- suppressWarnings(

        as.numeric(
          datos[[columna]]
        )
      )


      datos <- datos[

        !is.na(
          datos[[columna]]
        ),

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

        datos[[columna]],

        names.arg = paste(
          "Equipo",
          datos$Equipo
        ),

        ylim = c(
          0,
          100
        ),

        main = paste(
          "Promedio por Equipo -",
          semestre
        ),

        xlab = "Equipo",

        ylab = "Calificacion"
      )


      abline(

        h = mean(

          datos[[columna]],

          na.rm = TRUE
        ),

        lty = 2,

        lwd = 2
      )
    }


    #************************************************************************
    #STUDENTS

    if(input$tipo_grafica=="alumnos"){

      datos <- guardados$prom_alumnos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        plot.new()

        text(
          0.5,
          0.5,
          "No hay calificaciones todavia"
        )

        return()
      }


      datos <- datos[

        datos$Alumno != "PROMEDIO GENERAL",

      ]


      datos[[columna]] <- suppressWarnings(

        as.numeric(
          datos[[columna]]
        )
      )


      datos <- datos[

        !is.na(
          datos[[columna]]
        ),

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


      par(

        mar = c(
          5,
          12,
          4,
          2
        )
      )


      barplot(

        datos[[columna]],

        names.arg = datos$Alumno,

        horiz = TRUE,

        las = 1,

        xlim = c(
          0,
          100
        ),

        main = paste(
          "Promedio por Alumno -",
          semestre
        ),

        xlab = "Calificacion"
      )


      abline(

        v = mean(

          datos[[columna]],

          na.rm = TRUE
        ),

        lty = 2,

        lwd = 2
      )
    }

  })


#************************************************************************
#ADMIN UI

  output$admin_ui <- renderUI({


    if(!admin_ok()){


      tagList(


        h4(
          "Acceso del profesor encargado"
        ),


        passwordInput(

          "admin_password",

          "Contraseña:"
        ),


        actionButton(

          "login_admin",

          "Entrar",

          icon = icon("unlock")
        )

      )


    }else{


      lista <- alumnos_actuales()


      opciones <- character()


      if(nrow(lista)>0){

        opciones <- setNames(

          as.character(
            1:nrow(lista)
          ),

          paste(

            lista$Equipo,

            "-",

            lista$Alumno
          )
        )
      }


      tagList(


        h3(
          semestre
        ),


        tabsetPanel(

          id = "admin_tabs",


          #************************************************************
          #STUDENTS

          tabPanel(

            "Alumnos",

            br(),


            tags$p(

              "Los alumnos pueden editarse desde aqui o directamente en Google Sheets."
            ),


            hr(),


            h4(
              "Cargar lista completa"
            ),


            tags$p(

              "El archivo debe tener las columnas Equipo y Alumno. Activo es opcional."
            ),


            fileInput(

              "archivo_alumnos",

              "Archivo Excel o CSV:",

              accept = c(

                ".xlsx",

                ".xls",

                ".csv"
              )

            ),


            actionButton(

              "guardar_lista",

              "Reemplazar lista del semestre",

              icon = icon("upload"),

              class = "btn-success"
            ),


            hr(),


            h4(
              "Agregar alumno"
            ),


            textInput(

              "admin_equipo",

              "Equipo:"
            ),


            textInput(

              "admin_alumno",

              "Nombre del alumno:"
            ),


            actionButton(

              "agregar_alumno",

              "Agregar",

              icon = icon("plus")
            ),


            hr(),


            h4(
              "Desactivar alumno"
            ),


            selectInput(

              "quitar_alumno",

              "Alumno:",

              choices = opciones
            ),


            actionButton(

              "desactivar_alumno",

              "Desactivar",

              icon = icon("times")
            ),


            hr(),


            h4(
              "Google Sheets"
            ),


            tags$a(

              href = google_config_url,

              target = "_blank",

              class = "btn btn-primary",

              icon("table"),

              " Administracion"
            ),


            HTML("&nbsp;"),


            tags$a(

              href = google_equipos_url,

              target = "_blank",

              class = "btn btn-primary",

              icon("users"),

              " Equipos"
            ),


            HTML("&nbsp;"),


            tags$a(

              href = google_alumnos_url,

              target = "_blank",

              class = "btn btn-primary",

              icon("user"),

              " Alumnos"
            ),


            br(),
            br(),


            actionButton(

              "reload_alumnos",

              "Recargar desde Google Sheets",

              icon = icon("refresh")
            ),


            hr(),


            h4(
              "Lista actual"
            ),


            tableOutput(
              "lista_actual"
            ),


            hr(),


            h4(
              "Vista previa del archivo"
            ),


            tableOutput(
              "preview_lista"
            )

          ),


          #************************************************************
          #SHORT RUBRIC

          tabPanel(

            "Rubrica corta",

            br(),


            h3(
              "Rubrica corta"
            ),


            tags$p(

              "Rubrica resumida de cinco aspectos."
            ),


            tableOutput(
              "tabla_rubrica_corta"
            ),


            hr(),


            tags$strong(
              "Total: 100%"
            )

          ),


          #************************************************************
          #LONG RUBRIC

          tabPanel(

            "Rubrica larga",

            br(),


            h3(
              "Rubrica larga"
            ),


            tags$p(

              "Criterios P.C. y U.T. y su relacion con la rubrica corta."
            ),


            tableOutput(
              "tabla_rubrica_larga"
            ),


            hr(),


            tags$p(

              tags$strong(
                "Nota:"
              ),

              "Calidad y funcionamiento del prototipo y Comunicacion y trabajo en equipo no tienen una equivalencia directa identificada dentro de P.C. / U.T."

            )

          )

        ),


        br(),


        actionButton(

          "logout_admin",

          "Cerrar sesion",

          icon = icon("sign-out")
        )

      )

    }

  })


#************************************************************************
#RUBRIC TABLES ADMIN

  output$tabla_rubrica_corta <- renderTable({


    req(
      admin_ok()
    )


    data.frame(

      No = rubrica_corta$Numero,

      Aspecto = rubrica_corta$Aspecto,

      `Peso (%)` = rubrica_corta$Peso,

      Descripcion = rubrica_corta$Descripcion,

      check.names = FALSE

    )


  },digits = 2)


  output$tabla_rubrica_larga <- renderTable({


    req(
      admin_ok()
    )


    data.frame(

      No = rubrica_larga$Numero,

      Rubrica = rubrica_larga$Rubrica,

      Aspecto = rubrica_larga$Aspecto,

      `Pertenece a rubrica corta` = rubrica_larga$Relacion_Corta,

      check.names = FALSE

    )


  },digits = 2)


#************************************************************************
#ADMIN LOGIN

  observeEvent(input$login_admin,{


    password_real <- Sys.getenv(
      "ADMIN_PASSWORD"
    )


    if(password_real==""){

      showNotification(

        "Falta configurar ADMIN_PASSWORD.",

        type = "error"
      )

      return()
    }


    if(input$admin_password == password_real){

      admin_ok(
        TRUE
      )


      showNotification(

        "Acceso concedido.",

        type = "message"
      )


    }else{


      showNotification(

        "Contraseña incorrecta.",

        type = "error"
      )

    }

  })


  observeEvent(input$logout_admin,{

    admin_ok(
      FALSE
    )

  })


#************************************************************************
#ADMIN FILE

  lista_subida <- reactive({


    req(
      input$archivo_alumnos
    )


    tryCatch({


      leer_lista(

        input$archivo_alumnos$datapath,

        input$archivo_alumnos$name
      )


    },error=function(e){


      data.frame(

        Error = e$message
      )

    })

  })


  output$preview_lista <- renderTable({


    req(
      admin_ok()
    )


    if(is.null(input$archivo_alumnos)){

      return(
        NULL
      )
    }


    head(

      lista_subida(),

      20
    )

  })


  output$lista_actual <- renderTable({


    req(
      admin_ok()
    )


    alumnos_actuales()

  })


#************************************************************************
#REFRESH STUDENTS

  recargar_alumnos <- function(){


    nueva <- get_alumnos()


    alumnos_actuales(
      nueva
    )


    nuevos_promedios <- actualiza_promedios(

      guardados$equipos,

      guardados$alumnos,

      nueva
    )


    guardados$prom_equipos <- nuevos_promedios$equipos

    guardados$prom_alumnos <- nuevos_promedios$alumnos

  }


  observeEvent(input$reload_alumnos,{


    if(!admin_ok()){

      return()
    }


    recargar_alumnos()


    showNotification(

      "Lista actualizada desde Google Sheets.",

      type = "message"
    )

  })


#************************************************************************
#UPLOAD LIST

  observeEvent(input$guardar_lista,{


    if(!admin_ok()){

      return()
    }


    nueva <- lista_subida()


    if("Error" %in% names(nueva)){

      showNotification(

        nueva$Error[1],

        type = "error"
      )

      return()
    }


    if(nrow(nueva)==0){

      showNotification(

        "El archivo no contiene alumnos.",

        type = "error"
      )

      return()
    }


    resultado <- tryCatch({


      sheet_write(

        nueva,

        ss = google_config,

        sheet = semestre
      )


      TRUE


    },error=function(e){


      showNotification(

        paste(
          "Google Sheets error:",
          e$message
        ),

        type = "error"
      )


      FALSE
    })


    if(!resultado){

      return()
    }


    recargar_alumnos()


    showNotification(

      paste(

        nrow(nueva),

        "alumnos cargados para",
        semestre
      ),

      type = "message"
    )

  })


#************************************************************************
#ADD STUDENT

  observeEvent(input$agregar_alumno,{


    if(!admin_ok()){

      return()
    }


    equipo <- trimws(
      input$admin_equipo
    )


    alumno <- trimws(
      input$admin_alumno
    )


    if(
      equipo=="" ||
      alumno==""
    ){

      showNotification(

        "Escribe equipo y alumno.",

        type = "error"
      )

      return()
    }


    lista_completa <- read_old(

      google_config,

      semestre
    )


    if(nrow(lista_completa)==0){

      lista_completa <- empty_lista
    }


    if(!("Activo" %in% names(lista_completa))){

      lista_completa$Activo <- rep(

        "SI",

        nrow(lista_completa)
      )
    }


    existe <- which(

      trimws(

        as.character(
          lista_completa$Equipo
        )

      ) == equipo &

      trimws(

        as.character(
          lista_completa$Alumno
        )

      ) == alumno

    )


    if(length(existe)>0){

      lista_completa$Activo[existe] <- "SI"


    }else{


      lista_completa <- rbind(

        lista_completa,

        data.frame(

          Equipo = equipo,

          Alumno = alumno,

          Activo = "SI",

          stringsAsFactors = FALSE
        )

      )

    }


    sheet_write(

      lista_completa,

      ss = google_config,

      sheet = semestre
    )


    recargar_alumnos()


    showNotification(

      "Alumno agregado.",

      type = "message"
    )

  })


#************************************************************************
#DEACTIVATE STUDENT

  observeEvent(input$desactivar_alumno,{


    if(!admin_ok()){

      return()
    }


    lista <- alumnos_actuales()


    if(
      nrow(lista)==0 ||
      is.null(input$quitar_alumno) ||
      input$quitar_alumno==""
    ){

      return()
    }


    idx <- suppressWarnings(

      as.integer(
        input$quitar_alumno
      )

    )


    if(
      is.na(idx) ||
      idx > nrow(lista)
    ){

      return()
    }


    target_equipo <- lista$Equipo[idx]

    target_alumno <- lista$Alumno[idx]


    lista_completa <- read_old(

      google_config,

      semestre
    )


    if(!("Activo" %in% names(lista_completa))){

      lista_completa$Activo <- rep(

        "SI",

        nrow(lista_completa)
      )
    }


    fila <- which(

      trimws(

        as.character(
          lista_completa$Equipo
        )

      ) == trimws(
        target_equipo
      ) &

      trimws(

        as.character(
          lista_completa$Alumno
        )

      ) == trimws(
        target_alumno
      )

    )


    if(length(fila)>0){

      lista_completa$Activo[fila] <- "NO"


      sheet_write(

        lista_completa,

        ss = google_config,

        sheet = semestre
      )


      recargar_alumnos()


      showNotification(

        "Alumno desactivado.",

        type = "message"
      )

    }

  })

}


shinyApp(
  ui,
  server
)