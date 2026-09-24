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

get_semestre <- function(fecha=Sys.Date()){

  mes <- as.integer(
    format(fecha,"%m")
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
#RUBRICA CORTA ACTUAL
#ESTA ES LA QUE SI SE USA PARA CALIFICAR
#escala 0-10

rubrica <- data.frame(

  Aspecto = c(
    "Comprension conceptual",
    "Calidad y funcionamiento del prototipo",
    "Modelo matematico y analisis",
    "Uso de tecnologia y recursos",
    "Comunicacion y trabajo en equipo",
    "Formacion continua y aprendizaje autonomo"
  ),

  Descripcion = c(
    "Dominio de los conceptos y principios aplicados al proyecto.",
    "Funcionamiento, estabilidad y coherencia del prototipo con el objetivo.",
    "Relacion entre el modelo teorico, las ecuaciones y el comportamiento experimental.",
    "Uso adecuado de software, materiales, herramientas y recursos tecnologicos.",
    "Claridad, organizacion, comunicacion y participacion equilibrada del equipo.",
    "Identificacion de necesidades de formacion y propuesta de un plan para adquirir nuevos conocimientos o herramientas."
  ),

  Peso = c(
    20,
    20,
    20,
    20,
    10,
    10
  ),

  stringsAsFactors = FALSE
)


if(sum(rubrica$Peso)!=100){

  stop(
    "La suma de la rubrica debe ser igual a 100"
  )
}


#************************************************************************
#RUBRICA LARGA
#PC + UT
#solo para consulta del admin

rubrica_larga <- data.frame(

  Numero = c(
    1,2,3,4,5,6,7,8
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
    "6 - Formacion continua y aprendizaje autonomo",
    "6 - Formacion continua y aprendizaje autonomo",
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

  Comprension = numeric(),
  Prototipo = numeric(),
  Modelo = numeric(),
  Tecnologia = numeric(),
  Comunicacion = numeric(),
  Formacion = numeric(),

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

  Comprension = numeric(),
  Prototipo = numeric(),
  Modelo = numeric(),
  Tecnologia = numeric(),
  Comunicacion = numeric(),
  Formacion = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


empty_prom_equipos <- data.frame(

  Equipo = character(),

  Comprension = numeric(),
  Prototipo = numeric(),
  Modelo = numeric(),
  Tecnologia = numeric(),
  Comunicacion = numeric(),
  Formacion = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


empty_prom_alumnos <- data.frame(

  Equipo = character(),
  Alumno = character(),

  Comprension = numeric(),
  Prototipo = numeric(),
  Modelo = numeric(),
  Tecnologia = numeric(),
  Comunicacion = numeric(),
  Formacion = numeric(),

  Nota_Final = numeric(),

  stringsAsFactors = FALSE
)


#************************************************************************
#RUBRICA LARGA - GOOGLE SHEETS

#estas tablas son las que se escriben en la pestaña "Rubrica Larga"

empty_larga_equipos <- data.frame(

  ID = character(),
  Fecha = character(),
  Semestre = character(),
  Profesor = character(),
  Materia_Grupo = character(),

  Equipo = character(),
  Integrantes = character(),

  PC1_Extraer_Informacion = numeric(),
  PC2_Principios_Matematicos_Fisicos = numeric(),
  PC3_Evaluacion_Limitaciones = numeric(),
  PC4_Necesidades_Formacion = numeric(),
  PC5_Plan_Formacion = numeric(),

  UT1_Seleccion_Herramienta_TIC = numeric(),
  UT2_Uso_Herramientas_TIC = numeric(),
  UT3_Evaluacion_Limitaciones_TIC = numeric(),

  Promedio_PC = numeric(),
  Promedio_UT = numeric(),
  Nota_Larga = numeric(),
  Nota_Corta = numeric(),

  stringsAsFactors = FALSE
)


empty_larga_alumnos <- data.frame(

  ID = character(),
  Fecha = character(),
  Semestre = character(),
  Profesor = character(),
  Materia_Grupo = character(),

  Equipo = character(),
  Alumno = character(),

  PC1_Extraer_Informacion = numeric(),
  PC2_Principios_Matematicos_Fisicos = numeric(),
  PC3_Evaluacion_Limitaciones = numeric(),
  PC4_Necesidades_Formacion = numeric(),
  PC5_Plan_Formacion = numeric(),

  UT1_Seleccion_Herramienta_TIC = numeric(),
  UT2_Uso_Herramientas_TIC = numeric(),
  UT3_Evaluacion_Limitaciones_TIC = numeric(),

  Promedio_PC = numeric(),
  Promedio_UT = numeric(),
  Nota_Larga = numeric(),
  Nota_Corta = numeric(),

  stringsAsFactors = FALSE
)


empty_lista <- data.frame(

  Equipo = character(),
  Alumno = character(),
  Activo = character(),

  stringsAsFactors = FALSE
)


empty_semestres <- data.frame(

  Semestre = character(),
  Libro_Equipos_ID = character(),
  Libro_Alumnos_ID = character(),
  Fecha_Creacion = character(),

  stringsAsFactors = FALSE
)


#************************************************************************
#BASIC FUNCTIONS

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


get_final <- function(grado){

  sum(
    grado *
    rubrica$Peso
  ) / 100
}


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
#CONVERT SHORT -> LONG RUBRIC

#equivalencia acordada:
#corta 1 -> PC1
#corta 3 -> PC2 + PC3
#corta 6 -> PC4 + PC5
#corta 4 -> UT1 + UT2 + UT3
#corta 2 y 5 quedan solo en la rubrica corta

convertir_larga <- function(datos,tipo="equipo"){

  if(
    is.null(datos) ||
    nrow(datos)==0
  ){

    if(tipo=="equipo"){

      return(
        empty_larga_equipos
      )

    }else{

      return(
        empty_larga_alumnos
      )
    }
  }


  #make sure numeric columns exist
  columnas <- c(
    "Comprension",
    "Modelo",
    "Tecnologia",
    "Formacion",
    "Nota_Final"
  )


  for(c in columnas){

    if(!(c %in% names(datos))){

      datos[[c]] <- NA_real_
    }

    datos[[c]] <- suppressWarnings(
      as.numeric(
        datos[[c]]
      )
    )
  }


  #common info
  base <- data.frame(

    ID = if("ID" %in% names(datos)) as.character(datos$ID) else rep("",nrow(datos)),

    Fecha = if("Fecha" %in% names(datos)) as.character(datos$Fecha) else rep("",nrow(datos)),

    Semestre = if("Semestre" %in% names(datos)) as.character(datos$Semestre) else rep(semestre,nrow(datos)),

    Profesor = if("Profesor" %in% names(datos)) as.character(datos$Profesor) else rep("",nrow(datos)),

    Materia_Grupo = if("Materia_Grupo" %in% names(datos)) as.character(datos$Materia_Grupo) else rep("",nrow(datos)),

    Equipo = if("Equipo" %in% names(datos)) as.character(datos$Equipo) else rep("",nrow(datos)),

    stringsAsFactors = FALSE
  )


  if(tipo=="equipo"){

    base$Integrantes <- if("Integrantes" %in% names(datos)) as.character(datos$Integrantes) else rep("",nrow(datos))

  }else{

    base$Alumno <- if("Alumno" %in% names(datos)) as.character(datos$Alumno) else rep("",nrow(datos))
  }


  #long rubric
  base$PC1_Extraer_Informacion <- datos$Comprension

  base$PC2_Principios_Matematicos_Fisicos <- datos$Modelo

  base$PC3_Evaluacion_Limitaciones <- datos$Modelo

  base$PC4_Necesidades_Formacion <- datos$Formacion

  base$PC5_Plan_Formacion <- datos$Formacion


  base$UT1_Seleccion_Herramienta_TIC <- datos$Tecnologia

  base$UT2_Uso_Herramientas_TIC <- datos$Tecnologia

  base$UT3_Evaluacion_Limitaciones_TIC <- datos$Tecnologia


  base$Promedio_PC <- round(
    rowMeans(
      cbind(
        base$PC1_Extraer_Informacion,
        base$PC2_Principios_Matematicos_Fisicos,
        base$PC3_Evaluacion_Limitaciones,
        base$PC4_Necesidades_Formacion,
        base$PC5_Plan_Formacion
      ),
      na.rm = TRUE
    ),
    2
  )


  base$Promedio_UT <- round(
    rowMeans(
      cbind(
        base$UT1_Seleccion_Herramienta_TIC,
        base$UT2_Uso_Herramientas_TIC,
        base$UT3_Evaluacion_Limitaciones_TIC
      ),
      na.rm = TRUE
    ),
    2
  )


  base$Promedio_PC[
    is.nan(base$Promedio_PC)
  ] <- NA_real_


  base$Promedio_UT[
    is.nan(base$Promedio_UT)
  ] <- NA_real_


  base$Nota_Larga <- round(
    (
      base$Promedio_PC +
      base$Promedio_UT
    ) / 2,
    2
  )


  base$Nota_Corta <- datos$Nota_Final


  #same order every time
  if(tipo=="equipo"){

    base <- base[
      ,
      names(empty_larga_equipos),
      drop = FALSE
    ]

  }else{

    base <- base[
      ,
      names(empty_larga_alumnos),
      drop = FALSE
    ]
  }


  base
}


#************************************************************************
#WRITE LONG RUBRIC TO GOOGLE

actualiza_rubrica_larga <- function(datos_eq=NULL,datos_al=NULL){

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


  larga_eq <- convertir_larga(
    datos_eq,
    "equipo"
  )


  larga_al <- convertir_larga(
    datos_al,
    "alumno"
  )


  sheet_write(
    larga_eq,
    ss = google_equipos,
    sheet = "Rubrica Larga"
  )


  sheet_write(
    larga_al,
    ss = google_alumnos,
    sheet = "Rubrica Larga"
  )


  list(
    equipos = larga_eq,
    alumnos = larga_al
  )
}


#************************************************************************
#NORMALIZE ADMIN LIST

normalizar_lista <- function(x){

  if(
    is.null(x) ||
    nrow(x)==0
  ){

    return(
      empty_lista
    )
  }

  if(
    !("Equipo" %in% names(x)) ||
    !("Alumno" %in% names(x))
  ){

    return(
      empty_lista
    )
  }

  if(!("Activo" %in% names(x))){

    x$Activo <- rep(
      "SI",
      nrow(x)
    )
  }

  y <- data.frame(

    Equipo = as.character(
      x$Equipo
    ),

    Alumno = as.character(
      x$Alumno
    ),

    Activo = as.character(
      x$Activo
    ),

    stringsAsFactors = FALSE
  )

  y$Equipo <- trimws(
    y$Equipo
  )

  y$Alumno <- trimws(
    y$Alumno
  )

  y$Activo <- toupper(
    trimws(
      y$Activo
    )
  )

  y$Activo[
    is.na(y$Activo) |
    y$Activo==""
  ] <- "SI"

  y$Activo[
    !(y$Activo %in% c(
      "SI",
      "NO"
    ))
  ] <- "SI"

  y <- y[

    !is.na(y$Equipo) &
    !is.na(y$Alumno) &
    y$Equipo != "" &
    y$Alumno != "",

  ]

  y <- unique(
    y
  )

  rownames(y) <- NULL

  y
}


lista_activa <- function(x){

  x <- normalizar_lista(
    x
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

  x <- x[
    x$Activo == "SI",
    c(
      "Equipo",
      "Alumno"
    ),
    drop = FALSE
  ]

  rownames(x) <- NULL

  x
}


#************************************************************************
#GOOGLE ADMIN BOOK

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

if(!("Semestres" %in% sheet_names(google_config))){

  sheet_write(
    empty_semestres,
    ss = google_config,
    sheet = "Semestres"
  )
}


if(!(semestre %in% sheet_names(google_config))){

  sheet_write(
    empty_lista,
    ss = google_config,
    sheet = semestre
  )
}


#************************************************************************
#SEMESTER BOOKS

get_libros_semestre <- function(){

  registro <- read_old(
    google_config,
    "Semestres"
  )

  columnas <- c(
    "Semestre",
    "Libro_Equipos_ID",
    "Libro_Alumnos_ID",
    "Fecha_Creacion"
  )

  if(
    nrow(registro)==0 ||
    !all(columnas %in% names(registro))
  ){

    registro <- empty_semestres
  }

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

        if(
          is.na(equipo_id) ||
          equipo_id==""
        ){

          FALSE

        }else{

          gs4_get(
            as_sheets_id(
              equipo_id
            )
          )

          TRUE
        }

      },error=function(e){

        FALSE
      })


      alumno_ok <- tryCatch({

        if(
          is.na(alumno_id) ||
          alumno_id==""
        ){

          FALSE

        }else{

          gs4_get(
            as_sheets_id(
              alumno_id
            )
          )

          TRUE
        }

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
#CHECK TABS

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


#new tab with the converted long rubric
if(!("Rubrica Larga" %in% sheet_names(google_equipos))){

  sheet_write(
    empty_larga_equipos,
    ss = google_equipos,
    sheet = "Rubrica Larga"
  )
}


if(!("Rubrica Larga" %in% sheet_names(google_alumnos))){

  sheet_write(
    empty_larga_alumnos,
    ss = google_alumnos,
    sheet = "Rubrica Larga"
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
#READ STUDENT EXCEL

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

    hojas <- excel_sheets(
      path
    )

    if("Lista_Alumnos" %in% hojas){

      x <- as.data.frame(
        read_excel(
          path,
          sheet = "Lista_Alumnos"
        )
      )

    }else{

      x <- as.data.frame(
        read_excel(
          path
        )
      )
    }
  }


  nombres <- names(
    x
  )


  nombres_limpios <- iconv(
    nombres,
    from = "",
    to = "ASCII//TRANSLIT"
  )

  nombres_limpios <- tolower(
    nombres_limpios
  )

  nombres_limpios <- gsub(
    "[^a-z0-9]",
    "",
    nombres_limpios
  )


  col_equipo <- which(
    nombres_limpios %in% c(
      "equipo",
      "equipos",
      "team",
      "grupo"
    )
  )


  col_alumno <- which(
    nombres_limpios %in% c(
      "alumno",
      "alumnos",
      "estudiante",
      "estudiantes",
      "nombre",
      "nombrealumno",
      "nombredelalumno",
      "nombreestudiante",
      "nombredelestudiante"
    )
  )


  col_activo <- which(
    nombres_limpios %in% c(
      "activo",
      "activa",
      "active",
      "estado"
    )
  )


  if(length(col_equipo)==0){

    stop(
      paste0(
        "No se encontro la columna Equipo. Columnas encontradas: ",
        paste(
          nombres,
          collapse = ", "
        )
      )
    )
  }


  if(length(col_alumno)==0){

    stop(
      paste0(
        "No se encontro la columna Alumno. Columnas encontradas: ",
        paste(
          nombres,
          collapse = ", "
        )
      )
    )
  }


  lista <- data.frame(

    Equipo = as.character(
      x[[
        col_equipo[1]
      ]]
    ),

    Alumno = as.character(
      x[[
        col_alumno[1]
      ]]
    ),

    stringsAsFactors = FALSE
  )


  if(length(col_activo)>0){

    lista$Activo <- as.character(
      x[[
        col_activo[1]
      ]]
    )

  }else{

    lista$Activo <- rep(
      "SI",
      nrow(lista)
    )
  }


  normalizar_lista(
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

    lista_completa <- read_old(
      google_config,
      semestre
    )

    lista <- lista_activa(
      lista_completa
    )
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
  #NO STUDENTS

  if(
    is.null(lista) ||
    nrow(lista)==0
  ){

    prom_eq <- empty_prom_equipos
    prom_al <- empty_prom_alumnos


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

    Comprension = rep(
      NA_real_,
      length(equipos)
    ),

    Prototipo = rep(
      NA_real_,
      length(equipos)
    ),

    Modelo = rep(
      NA_real_,
      length(equipos)
    ),

    Tecnologia = rep(
      NA_real_,
      length(equipos)
    ),

    Comunicacion = rep(
      NA_real_,
      length(equipos)
    ),

    Formacion = rep(
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

    columnas <- c(
      "Comprension",
      "Prototipo",
      "Modelo",
      "Tecnologia",
      "Comunicacion",
      "Formacion",
      "Nota_Final"
    )

    for(c in columnas){

      if(c %in% names(datos_eq)){

        datos_eq[[c]] <- suppressWarnings(
          as.numeric(
            datos_eq[[c]]
          )
        )
      }
    }


    for(i in seq_along(equipos)){

      eq <- equipos[i]


      datos <- datos_eq[
        trimws(
          datos_eq$Equipo
        ) == trimws(
          eq
        ),
      ]


      if(nrow(datos)>0){

        prom_eq$Comprension[i] <- promedio(
          datos$Comprension
        )

        prom_eq$Prototipo[i] <- promedio(
          datos$Prototipo
        )

        prom_eq$Modelo[i] <- promedio(
          datos$Modelo
        )

        prom_eq$Tecnologia[i] <- promedio(
          datos$Tecnologia
        )

        prom_eq$Comunicacion[i] <- promedio(
          datos$Comunicacion
        )

        prom_eq$Formacion[i] <- promedio(
          datos$Formacion
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

        Comprension = promedio(
          prom_eq$Comprension
        ),

        Prototipo = promedio(
          prom_eq$Prototipo
        ),

        Modelo = promedio(
          prom_eq$Modelo
        ),

        Tecnologia = promedio(
          prom_eq$Tecnologia
        ),

        Comunicacion = promedio(
          prom_eq$Comunicacion
        ),

        Formacion = promedio(
          prom_eq$Formacion
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


  prom_al$Comprension <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Prototipo <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Modelo <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Tecnologia <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Comunicacion <- rep(
    NA_real_,
    nrow(prom_al)
  )

  prom_al$Formacion <- rep(
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

    columnas <- c(
      "Comprension",
      "Prototipo",
      "Modelo",
      "Tecnologia",
      "Comunicacion",
      "Formacion",
      "Nota_Final"
    )

    for(c in columnas){

      if(c %in% names(datos_al)){

        datos_al[[c]] <- suppressWarnings(
          as.numeric(
            datos_al[[c]]
          )
        )
      }
    }


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

        prom_al$Comprension[i] <- promedio(
          datos$Comprension
        )

        prom_al$Prototipo[i] <- promedio(
          datos$Prototipo
        )

        prom_al$Modelo[i] <- promedio(
          datos$Modelo
        )

        prom_al$Tecnologia[i] <- promedio(
          datos$Tecnologia
        )

        prom_al$Comunicacion[i] <- promedio(
          datos$Comunicacion
        )

        prom_al$Formacion[i] <- promedio(
          datos$Formacion
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

        Comprension = promedio(
          prom_al$Comprension
        ),

        Prototipo = promedio(
          prom_al$Prototipo
        ),

        Modelo = promedio(
          prom_al$Modelo
        ),

        Tecnologia = promedio(
          prom_al$Tecnologia
        ),

        Comunicacion = promedio(
          prom_al$Comunicacion
        ),

        Formacion = promedio(
          prom_al$Formacion
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
      #EVALUACION

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

            title = "Rubrica corta - 6 aspectos",

            status = "warning",
            solidHeader = TRUE,
            width = 8,


            tags$p(
              "Escala: 0 a 10"
            ),


            tags$p(
              tags$strong("Referencia: "),
              "Excelente 9-10 | Bueno 7-8 | Satisfactorio 5-6 | Insuficiente menor a 5"
            ),


            hr(),


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
      #RESULTADOS

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
                "Comprension conceptual" = "Comprension",
                "Calidad y funcionamiento del prototipo" = "Prototipo",
                "Modelo matematico y analisis" = "Modelo",
                "Uso de tecnologia y recursos" = "Tecnologia",
                "Comunicacion y trabajo en equipo" = "Comunicacion",
                "Formacion continua y aprendizaje autonomo" = "Formacion"
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
  #START DATA

  lista_admin_inicio <- normalizar_lista(

    read_old(
      google_config,
      semestre
    )
  )


  lista_inicio <- lista_activa(
    lista_admin_inicio
  )


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


  largas_inicio <- actualiza_rubrica_larga(
    equipos_inicio,
    alumnos_inicio
  )


  lista_admin <- reactiveVal(
    lista_admin_inicio
  )


  alumnos_actuales <- reactiveVal(
    lista_inicio
  )


  guardados <- reactiveValues(
    equipos = equipos_inicio,
    alumnos = alumnos_inicio,
    prom_equipos = promedios_inicio$equipos,
    prom_alumnos = promedios_inicio$alumnos,
    larga_equipos = largas_inicio$equipos,
    larga_alumnos = largas_inicio$alumnos
  )


  admin_ok <- reactiveVal(
    FALSE
  )


  #************************************************************************
  #LOCAL UPDATE LIST

  actualizar_lista_local <- function(lista_completa){

    lista_completa <- normalizar_lista(
      lista_completa
    )

    lista_admin(
      lista_completa
    )


    activos <- lista_activa(
      lista_completa
    )

    alumnos_actuales(
      activos
    )


    nuevos_promedios <- actualiza_promedios(
      guardados$equipos,
      guardados$alumnos,
      activos
    )

    guardados$prom_equipos <- nuevos_promedios$equipos
    guardados$prom_alumnos <- nuevos_promedios$alumnos


    activos
  }


  #************************************************************************
  #TEAM SELECTOR

  output$equipo_ui <- renderUI({

    lista <- alumnos_actuales()


    if(nrow(lista)==0){

      return(
        tags$p(
          strong(
            "No hay alumnos activos cargados para este semestre."
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
  #TEAM STUDENTS

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
  #RUBRIC INPUTS

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


        tags$p(
          rubrica$Descripcion[i]
        ),


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
  #CURRENT GRADES

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
  #SAVE EVALUATION

  observeEvent(input$guardar,{

    if(nrow(alumnos_actuales())==0){

      showNotification(
        "Primero se debe cargar la lista de alumnos.",
        type = "error"
      )

      return()
    }


    if(
      is.null(input$equipo) ||
      input$equipo==""
    ){

      showNotification(
        "Selecciona un equipo.",
        type = "error"
      )

      return()
    }


    if(
      is.null(input$profesor) ||
      trimws(input$profesor)==""
    ){

      showNotification(
        "Falta el profesor evaluador.",
        type = "error"
      )

      return()
    }


    if(
      is.null(input$materia) ||
      trimws(input$materia)==""
    ){

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
        "Las calificaciones deben estar entre 0 y 10.",
        type = "error"
      )

      return()
    }


    #************************************************************************
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
    #TEAM ROW

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

      Comprension = grado[1],
      Prototipo = grado[2],
      Modelo = grado[3],
      Tecnologia = grado[4],
      Comunicacion = grado[5],
      Formacion = grado[6],

      Nota_Final = nota_final,

      stringsAsFactors = FALSE
    )


    #************************************************************************
    #STUDENT ROWS

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

      Formacion = rep(
        grado[6],
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


      #local
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


      nuevos_promedios <- actualiza_promedios(
        guardados$equipos,
        guardados$alumnos,
        alumnos_actuales()
      )


      guardados$prom_equipos <- nuevos_promedios$equipos
      guardados$prom_alumnos <- nuevos_promedios$alumnos


      #long rubric tabs
      nuevas_largas <- actualiza_rubrica_larga(
        guardados$equipos,
        guardados$alumnos
      )


      guardados$larga_equipos <- nuevas_largas$equipos
      guardados$larga_alumnos <- nuevas_largas$alumnos


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
        nota_final,
        " / 10"
      ),
      type = "message",
      duration = 6
    )
  })


  #************************************************************************
  #RESULT TABLE

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
      " / 10"
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
          10
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
          10
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


          #****************************************************************
          #ALUMNOS

          tabPanel(

            "Alumnos",

            br(),


            tags$p(
              "Los alumnos pueden editarse aqui o directamente en Google Sheets."
            ),


            hr(),


            h4(
              "Cargar lista completa"
            ),


            tags$p(
              "El archivo debe tener Equipo y Alumno. La columna Activo es opcional."
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


            HTML("&nbsp;"),


            actionButton(
              "actualizar_larga",
              "Actualizar Rubrica Larga",
              icon = icon("refresh")
            ),


            br(),
            br(),


            tags$p(
              tags$strong("Rubrica larga: "),
              "en los libros de Equipos y Alumnos hay una pestaña nueva llamada 'Rubrica Larga'."
            ),


            tags$a(
              href = google_equipos_url,
              target = "_blank",
              class = "btn btn-info",
              icon("table"),
              " Equipos - Rubrica Larga"
            ),


            HTML("&nbsp;"),


            tags$a(
              href = google_alumnos_url,
              target = "_blank",
              class = "btn btn-info",
              icon("table"),
              " Alumnos - Rubrica Larga"
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


          #****************************************************************
          #RUBRICA CORTA

          tabPanel(

            "Rubrica corta",

            br(),


            h3(
              "Rubrica corta - 6 aspectos"
            ),


            tags$p(
              "Esta es la rubrica que actualmente utiliza la aplicacion para calificar."
            ),


            tableOutput(
              "tabla_rubrica_corta"
            ),


            hr(),


            tags$strong(
              "Total: 100%"
            )
          ),


          #****************************************************************
          #RUBRICA LARGA

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
              "Calidad y funcionamiento del prototipo y Comunicacion y trabajo en equipo permanecen como criterios propios de la rubrica corta."
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

      No = 1:nrow(rubrica),

      Aspecto = rubrica$Aspecto,

      `Peso (%)` = rubrica$Peso,

      Descripcion = rubrica$Descripcion,

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

      `Pertenece a rubrica corta` =
        rubrica_larga$Relacion_Corta,

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
      30
    )
  })


  output$lista_actual <- renderTable({

    req(
      admin_ok()
    )


    alumnos_actuales()

  })


  #************************************************************************
  #UPDATE LONG RUBRIC MANUALLY

  observeEvent(input$actualizar_larga,{

    if(!admin_ok()){

      return()
    }


    #read current grades directly from google
    datos_eq <- read_old(
      google_equipos,
      "Calificaciones"
    )


    datos_al <- read_old(
      google_alumnos,
      "Calificaciones"
    )


    resultado <- tryCatch({

      nuevas_largas <- actualiza_rubrica_larga(
        datos_eq,
        datos_al
      )


      guardados$larga_equipos <- nuevas_largas$equipos

      guardados$larga_alumnos <- nuevas_largas$alumnos


      TRUE


    },error=function(e){

      showNotification(
        paste(
          "Error al actualizar la rubrica larga:",
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
      "Rubrica larga actualizada en los libros de Equipos y Alumnos.",
      type = "message",
      duration = 6
    )
  })


  #************************************************************************
  #REFRESH FROM GOOGLE

  observeEvent(input$reload_alumnos,{

    if(!admin_ok()){

      return()
    }


    nueva_completa <- normalizar_lista(

      read_old(
        google_config,
        semestre
      )
    )


    activos <- actualizar_lista_local(
      nueva_completa
    )


    showNotification(
      paste(
        nrow(activos),
        "alumnos activos cargados desde Google Sheets."
      ),
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


    nueva <- normalizar_lista(
      nueva
    )


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


    #use the uploaded data immediately
    activos <- actualizar_lista_local(
      nueva
    )


    showNotification(
      paste(
        nrow(activos),
        "alumnos activos cargados para",
        semestre
      ),
      type = "message",
      duration = 6
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


    lista_completa <- normalizar_lista(
      lista_admin()
    )


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


    lista_completa <- normalizar_lista(
      lista_completa
    )


    resultado <- tryCatch({

      sheet_write(
        lista_completa,
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


    actualizar_lista_local(
      lista_completa
    )


    updateTextInput(
      session,
      "admin_alumno",
      value = ""
    )


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


    activos <- alumnos_actuales()


    if(
      nrow(activos)==0 ||
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
      idx < 1 ||
      idx > nrow(activos)
    ){

      return()
    }


    target_equipo <- activos$Equipo[idx]
    target_alumno <- activos$Alumno[idx]


    lista_completa <- normalizar_lista(
      lista_admin()
    )


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


    if(length(fila)==0){

      return()
    }


    lista_completa$Activo[fila] <- "NO"


    resultado <- tryCatch({

      sheet_write(
        lista_completa,
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


    actualizar_lista_local(
      lista_completa
    )


    showNotification(
      "Alumno desactivado.",
      type = "message"
    )
  })

}


#************************************************************************
#RUN

shinyApp(
  ui,
  server
)
