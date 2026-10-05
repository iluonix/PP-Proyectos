library(shiny)
library(shinydashboard)
library(googlesheets4)
library(readxl)
library(digest)

#************************************************************************

private_dir <- file.path(
  getwd(),
  "private"
)

config_file <- file.path(
  private_dir,
  "config.rds"
)

google_cache <- file.path(
  private_dir,
  "google-token"
)


if(!file.exists(config_file)){

  stop(
    paste0(
      "Falta private/config.rds. ",
      "Ejecuta source('configurar.R') desde RStudio antes de correr o publicar."
    )
  )
}


if(
  !dir.exists(google_cache) ||
  length(
    list.files(
      google_cache,
      all.files = FALSE
    )
  )==0
){

  stop(
    paste0(
      "Falta la autorizacion de Google. ",
      "Ejecuta source('configurar.R') desde RStudio y autoriza ",
      "marco.montufar14@gmail.com."
    )
  )
}


config <- readRDS(
  config_file
)


required_config <- c(
  "google_email",
  "google_admin_sheet_id",
  "google_equipos_sheet_id",
  "google_alumnos_sheet_id",
  "admin_hash"
)


faltantes <- required_config[
  !(required_config %in% names(config))
]


if(length(faltantes)>0){

  stop(
    paste(
      "Faltan datos en private/config.rds:",
      paste(
        faltantes,
        collapse = ", "
      )
    )
  )
}


google_email <- trimws(
  as.character(
    config$google_email
  )
)


if(
  tolower(google_email) !=
  "marco.montufar14@gmail.com"
){

  stop(
    paste0(
      "La configuracion de Google no corresponde a Montufar. ",
      "Vuelve a ejecutar source('configurar.R')."
    )
  )
}


#************************************************************************
#GOOGLE AUTH
#
#The cached OAuth token belongs to Montufar.
#The app reuses it when running locally or on shinyapps.io.
#************************************************************************

options(
  gargle_oauth_cache = google_cache,
  gargle_oauth_email = google_email
)


gs4_auth(
  email = google_email,
  cache = google_cache
)


google_admin <- as_sheets_id(
  config$google_admin_sheet_id
)

google_equipos <- as_sheets_id(
  config$google_equipos_sheet_id
)

google_alumnos <- as_sheets_id(
  config$google_alumnos_sheet_id
)


#basic connection check

tryCatch({

  gs4_get(
    google_admin
  )

  gs4_get(
    google_equipos
  )

  gs4_get(
    google_alumnos
  )

},error=function(e){

  stop(
    paste0(
      "No se pudieron abrir los Google Sheets de Montufar. ",
      "Si la autorizacion fue revocada, ejecuta otra vez source('configurar.R') ",
      "y vuelve a publicar. Detalle: ",
      e$message
    )
  )
})


#************************************************************************
#SEMESTER

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


sheet_lista <- semestre

sheet_cal_equipos <- paste(
  "Calificaciones",
  semestre,
  sep = " - "
)

sheet_prom_equipos <- paste(
  "Promedios",
  semestre,
  sep = " - "
)

sheet_larga_equipos <- paste(
  "Rubrica Larga",
  semestre,
  sep = " - "
)


sheet_cal_alumnos <- paste(
  "Calificaciones",
  semestre,
  sep = " - "
)

sheet_prom_alumnos <- paste(
  "Promedios",
  semestre,
  sep = " - "
)

sheet_larga_alumnos <- paste(
  "Rubrica Larga",
  semestre,
  sep = " - "
)


#************************************************************************
#SHORT RUBRIC
#
#THIS IS THE RUBRIC USED TO EVALUATE
#scale 0-10
#************************************************************************

rubrica <- data.frame(

  Numero = c(
    1,2,3,4,5,6
  ),

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
    "La suma de la rubrica corta debe ser igual a 100."
  )
}


#************************************************************************
#LONG RUBRIC
#
#PC + UT
#
#IMPORTANT:
#Nota_Larga is ALWAYS equal to Nota_Corta.
#PC and UT are kept as diagnostic/detail averages only.
#************************************************************************

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

empty_lista <- data.frame(

  Equipo = character(),
  Alumno = character(),
  Activo = character(),

  stringsAsFactors = FALSE
)


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

  Nota_Corta = numeric(),

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

  Nota_Corta = numeric(),

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

  Nota_Corta = numeric(),
  Nota_Larga = numeric(),

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

  Nota_Corta = numeric(),
  Nota_Larga = numeric(),

  stringsAsFactors = FALSE
)


empty_larga_equipos <- data.frame(

  ID = character(),
  Fecha = character(),
  Semestre = character(),

  Profesor = character(),
  Materia_Grupo = character(),

  Equipo = character(),
  Integrantes = character(),

  PC_1 = numeric(),
  PC_2 = numeric(),
  PC_3 = numeric(),
  PC_4 = numeric(),
  PC_5 = numeric(),

  UT_1 = numeric(),
  UT_2 = numeric(),
  UT_3 = numeric(),

  Promedio_PC = numeric(),
  Promedio_UT = numeric(),

  Nota_Corta = numeric(),
  Nota_Larga = numeric(),

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

  PC_1 = numeric(),
  PC_2 = numeric(),
  PC_3 = numeric(),
  PC_4 = numeric(),
  PC_5 = numeric(),

  UT_1 = numeric(),
  UT_2 = numeric(),
  UT_3 = numeric(),

  Promedio_PC = numeric(),
  Promedio_UT = numeric(),

  Nota_Corta = numeric(),
  Nota_Larga = numeric(),

  stringsAsFactors = FALSE
)


#************************************************************************
#BASIC FUNCTIONS

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


get_final <- function(grado){

  round(

    sum(
      grado *
      rubrica$Peso
    ) / 100,

    2
  )
}


hash_password <- function(password){

  digest::digest(
    password,
    algo = "sha256",
    serialize = FALSE
  )
}


admin_password_ok <- function(password){

  if(
    is.null(password) ||
    trimws(password)==""
  ){

    return(
      FALSE
    )
  }


  hash_guardado <- trimws(
    as.character(
      config$admin_hash
    )
  )


  if(
    is.na(hash_guardado) ||
    hash_guardado==""
  ){

    return(
      FALSE
    )
  }


  identical(
    hash_password(
      password
    ),
    hash_guardado
  )
}


google_url <- function(id){

  paste0(
    "https://docs.google.com/spreadsheets/d/",
    id,
    "/edit"
  )
}


#************************************************************************
#GOOGLE HELPERS

ensure_sheet <- function(
  ss,
  sheet,
  empty_table,
  existing_sheets = NULL
){

  if(is.null(existing_sheets)){

    existing_sheets <- sheet_names(
      ss
    )
  }


  if(!(sheet %in% existing_sheets)){

    sheet_write(
      empty_table,
      ss = ss,
      sheet = sheet
    )
  }
}


read_table <- function(
  ss,
  sheet,
  empty_table
){

  x <- tryCatch({

    read_sheet(
      ss,
      sheet = sheet,
      show_col_types = FALSE
    )

  },error=function(e){

    stop(
      paste0(
        "No se pudo leer la hoja '",
        sheet,
        "': ",
        e$message
      )
    )
  })


  x <- as.data.frame(
    x
  )


  if(nrow(x)==0){

    return(
      empty_table
    )
  }


  x
}


#************************************************************************
#NORMALIZE ROSTER

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

    ,

    drop = FALSE
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
    x$Activo=="SI",
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
#READ STUDENT FILE

leer_lista_archivo <- function(
  path,
  name
){

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

  }else if(ext %in% c(
    "xlsx",
    "xls"
  )){

    hojas <- excel_sheets(
      path
    )


    hoja <- if(
      "Lista_Alumnos" %in% hojas
    ){
      "Lista_Alumnos"
    }else{
      hojas[1]
    }


    x <- as.data.frame(

      read_excel(
        path,
        sheet = hoja
      )
    )

  }else{

    stop(
      "El archivo debe ser .xlsx, .xls o .csv"
    )
  }


  nombres <- names(
    x
  )


  #first transliterate, then lowercase, then remove symbols
  nombres_limpios <- iconv(
    nombres,
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
      "team"
    )
  )


  col_alumno <- which(

    nombres_limpios %in% c(
      "alumno",
      "estudiante",
      "nombre",
      "nombrealumno",
      "student"
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
      "El archivo debe tener las columnas Equipo y Alumno."
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
#LONG RUBRIC CONVERSION
#
#Short -> Long:
#1 -> PC1
#3 -> PC2, PC3
#6 -> PC4, PC5
#4 -> UT1, UT2, UT3
#
#Nota_Larga = Nota_Corta ALWAYS
#************************************************************************

convertir_larga_equipos <- function(x){

  if(
    is.null(x) ||
    nrow(x)==0
  ){

    return(
      empty_larga_equipos
    )
  }


  y <- data.frame(

    ID = as.character(
      x$ID
    ),

    Fecha = as.character(
      x$Fecha
    ),

    Semestre = as.character(
      x$Semestre
    ),

    Profesor = as.character(
      x$Profesor
    ),

    Materia_Grupo = as.character(
      x$Materia_Grupo
    ),

    Equipo = as.character(
      x$Equipo
    ),

    Integrantes = as.character(
      x$Integrantes
    ),

    PC_1 = as.numeric(
      x$Comprension
    ),

    PC_2 = as.numeric(
      x$Modelo
    ),

    PC_3 = as.numeric(
      x$Modelo
    ),

    PC_4 = as.numeric(
      x$Formacion
    ),

    PC_5 = as.numeric(
      x$Formacion
    ),

    UT_1 = as.numeric(
      x$Tecnologia
    ),

    UT_2 = as.numeric(
      x$Tecnologia
    ),

    UT_3 = as.numeric(
      x$Tecnologia
    ),

    stringsAsFactors = FALSE
  )


  y$Promedio_PC <- round(

    rowMeans(
      y[
        ,
        c(
          "PC_1",
          "PC_2",
          "PC_3",
          "PC_4",
          "PC_5"
        ),
        drop = FALSE
      ],
      na.rm = TRUE
    ),

    2
  )


  y$Promedio_UT <- round(

    rowMeans(
      y[
        ,
        c(
          "UT_1",
          "UT_2",
          "UT_3"
        ),
        drop = FALSE
      ],
      na.rm = TRUE
    ),

    2
  )


  y$Nota_Corta <- as.numeric(
    x$Nota_Corta
  )

  y$Nota_Larga <- y$Nota_Corta


  y
}


convertir_larga_alumnos <- function(x){

  if(
    is.null(x) ||
    nrow(x)==0
  ){

    return(
      empty_larga_alumnos
    )
  }


  y <- data.frame(

    ID = as.character(
      x$ID
    ),

    Fecha = as.character(
      x$Fecha
    ),

    Semestre = as.character(
      x$Semestre
    ),

    Profesor = as.character(
      x$Profesor
    ),

    Materia_Grupo = as.character(
      x$Materia_Grupo
    ),

    Equipo = as.character(
      x$Equipo
    ),

    Alumno = as.character(
      x$Alumno
    ),

    PC_1 = as.numeric(
      x$Comprension
    ),

    PC_2 = as.numeric(
      x$Modelo
    ),

    PC_3 = as.numeric(
      x$Modelo
    ),

    PC_4 = as.numeric(
      x$Formacion
    ),

    PC_5 = as.numeric(
      x$Formacion
    ),

    UT_1 = as.numeric(
      x$Tecnologia
    ),

    UT_2 = as.numeric(
      x$Tecnologia
    ),

    UT_3 = as.numeric(
      x$Tecnologia
    ),

    stringsAsFactors = FALSE
  )


  y$Promedio_PC <- round(

    rowMeans(
      y[
        ,
        c(
          "PC_1",
          "PC_2",
          "PC_3",
          "PC_4",
          "PC_5"
        ),
        drop = FALSE
      ],
      na.rm = TRUE
    ),

    2
  )


  y$Promedio_UT <- round(

    rowMeans(
      y[
        ,
        c(
          "UT_1",
          "UT_2",
          "UT_3"
        ),
        drop = FALSE
      ],
      na.rm = TRUE
    ),

    2
  )


  y$Nota_Corta <- as.numeric(
    x$Nota_Corta
  )

  y$Nota_Larga <- y$Nota_Corta


  y
}


#************************************************************************
#AVERAGES

calcular_promedios <- function(
  datos_eq,
  datos_al,
  lista
){

  lista <- lista_activa(
    lista
  )


  #************************************************************************
  #TEAMS

  equipos_lista <- if(
    nrow(lista)>0
  ){
    as.character(
      lista$Equipo
    )
  }else{
    character()
  }


  equipos_datos <- if(
    !is.null(datos_eq) &&
    nrow(datos_eq)>0 &&
    "Equipo" %in% names(datos_eq)
  ){
    as.character(
      datos_eq$Equipo
    )
  }else{
    character()
  }


  equipos <- unique(
    c(
      equipos_lista,
      equipos_datos
    )
  )


  equipos <- equipos[
    !is.na(equipos) &
    trimws(equipos)!=""
  ]


  if(length(equipos)==0){

    prom_eq <- empty_prom_equipos

  }else{

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

      Nota_Corta = rep(
        NA_real_,
        length(equipos)
      ),

      Nota_Larga = rep(
        NA_real_,
        length(equipos)
      ),

      stringsAsFactors = FALSE
    )


    if(
      !is.null(datos_eq) &&
      nrow(datos_eq)>0
    ){

      datos_eq$Equipo <- as.character(
        datos_eq$Equipo
      )


      for(i in seq_along(equipos)){

        eq <- equipos[i]


        d <- datos_eq[

          trimws(
            datos_eq$Equipo
          ) == trimws(
            eq
          ),

          ,

          drop = FALSE
        ]


        if(nrow(d)>0){

          prom_eq$Comprension[i] <- promedio(
            d$Comprension
          )

          prom_eq$Prototipo[i] <- promedio(
            d$Prototipo
          )

          prom_eq$Modelo[i] <- promedio(
            d$Modelo
          )

          prom_eq$Tecnologia[i] <- promedio(
            d$Tecnologia
          )

          prom_eq$Comunicacion[i] <- promedio(
            d$Comunicacion
          )

          prom_eq$Formacion[i] <- promedio(
            d$Formacion
          )

          prom_eq$Nota_Corta[i] <- promedio(
            d$Nota_Corta
          )

          #same final average
          prom_eq$Nota_Larga[i] <- prom_eq$Nota_Corta[i]
        }
      }
    }


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

        Nota_Corta = promedio(
          prom_eq$Nota_Corta
        ),

        Nota_Larga = promedio(
          prom_eq$Nota_Corta
        ),

        stringsAsFactors = FALSE
      )
    )
  }


  #************************************************************************
  #STUDENTS

  pares_lista <- lista


  pares_datos <- if(
    !is.null(datos_al) &&
    nrow(datos_al)>0 &&
    all(
      c(
        "Equipo",
        "Alumno"
      ) %in% names(datos_al)
    )
  ){

    unique(

      data.frame(

        Equipo = as.character(
          datos_al$Equipo
        ),

        Alumno = as.character(
          datos_al$Alumno
        ),

        stringsAsFactors = FALSE
      )
    )

  }else{

    data.frame(
      Equipo = character(),
      Alumno = character(),
      stringsAsFactors = FALSE
    )
  }


  pares <- unique(

    rbind(
      pares_lista,
      pares_datos
    )
  )


  if(nrow(pares)>0){

    pares$Equipo <- trimws(
      as.character(
        pares$Equipo
      )
    )

    pares$Alumno <- trimws(
      as.character(
        pares$Alumno
      )
    )


    pares <- pares[

      !is.na(pares$Equipo) &
      !is.na(pares$Alumno) &
      pares$Equipo!="" &
      pares$Alumno!="",

      ,

      drop = FALSE
    ]
  }


  if(nrow(pares)==0){

    prom_al <- empty_prom_alumnos

  }else{

    prom_al <- data.frame(

      Equipo = pares$Equipo,
      Alumno = pares$Alumno,

      Comprension = rep(
        NA_real_,
        nrow(pares)
      ),

      Prototipo = rep(
        NA_real_,
        nrow(pares)
      ),

      Modelo = rep(
        NA_real_,
        nrow(pares)
      ),

      Tecnologia = rep(
        NA_real_,
        nrow(pares)
      ),

      Comunicacion = rep(
        NA_real_,
        nrow(pares)
      ),

      Formacion = rep(
        NA_real_,
        nrow(pares)
      ),

      Nota_Corta = rep(
        NA_real_,
        nrow(pares)
      ),

      Nota_Larga = rep(
        NA_real_,
        nrow(pares)
      ),

      stringsAsFactors = FALSE
    )


    if(
      !is.null(datos_al) &&
      nrow(datos_al)>0
    ){

      datos_al$Equipo <- as.character(
        datos_al$Equipo
      )

      datos_al$Alumno <- as.character(
        datos_al$Alumno
      )


      for(i in 1:nrow(prom_al)){

        eq <- prom_al$Equipo[i]
        al <- prom_al$Alumno[i]


        d <- datos_al[

          trimws(
            datos_al$Equipo
          ) == trimws(
            eq
          ) &

          trimws(
            datos_al$Alumno
          ) == trimws(
            al
          ),

          ,

          drop = FALSE
        ]


        if(nrow(d)>0){

          prom_al$Comprension[i] <- promedio(
            d$Comprension
          )

          prom_al$Prototipo[i] <- promedio(
            d$Prototipo
          )

          prom_al$Modelo[i] <- promedio(
            d$Modelo
          )

          prom_al$Tecnologia[i] <- promedio(
            d$Tecnologia
          )

          prom_al$Comunicacion[i] <- promedio(
            d$Comunicacion
          )

          prom_al$Formacion[i] <- promedio(
            d$Formacion
          )

          prom_al$Nota_Corta[i] <- promedio(
            d$Nota_Corta
          )

          prom_al$Nota_Larga[i] <- prom_al$Nota_Corta[i]
        }
      }
    }


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

        Nota_Corta = promedio(
          prom_al$Nota_Corta
        ),

        Nota_Larga = promedio(
          prom_al$Nota_Corta
        ),

        stringsAsFactors = FALSE
      )
    )
  }


  list(
    equipos = prom_eq,
    alumnos = prom_al
  )
}


#************************************************************************
#CREATE MISSING TABS
#
#No spreadsheet is created here.
#Only sheets/tabs are added inside the 3 files created in Montufar's Drive.
#************************************************************************

admin_sheets <- sheet_names(
  google_admin
)

equipos_sheets <- sheet_names(
  google_equipos
)

alumnos_sheets <- sheet_names(
  google_alumnos
)


ensure_sheet(
  google_admin,
  sheet_lista,
  empty_lista,
  admin_sheets
)


ensure_sheet(
  google_admin,
  "Rubrica Corta",
  rubrica,
  admin_sheets
)


ensure_sheet(
  google_admin,
  "Rubrica Larga",
  rubrica_larga,
  admin_sheets
)


ensure_sheet(
  google_equipos,
  sheet_cal_equipos,
  empty_equipos,
  equipos_sheets
)


ensure_sheet(
  google_equipos,
  sheet_prom_equipos,
  empty_prom_equipos,
  equipos_sheets
)


ensure_sheet(
  google_equipos,
  sheet_larga_equipos,
  empty_larga_equipos,
  equipos_sheets
)


ensure_sheet(
  google_alumnos,
  sheet_cal_alumnos,
  empty_alumnos,
  alumnos_sheets
)


ensure_sheet(
  google_alumnos,
  sheet_prom_alumnos,
  empty_prom_alumnos,
  alumnos_sheets
)


ensure_sheet(
  google_alumnos,
  sheet_larga_alumnos,
  empty_larga_alumnos,
  alumnos_sheets
)


#************************************************************************
#URLS

google_admin_url <- google_url(
  config$google_admin_sheet_id
)

google_equipos_url <- google_url(
  config$google_equipos_sheet_id
)

google_alumnos_url <- google_url(
  config$google_alumnos_sheet_id
)


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

      #************************************************************************
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

            title = "Rubrica corta",

            status = "warning",
            solidHeader = TRUE,
            width = 8,


            tags$p(
              "Escala de 0 a 10. La nota global de la rubrica larga siempre sera igual a la nota de esta rubrica."
            ),


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


      #************************************************************************
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


            actionButton(
              "refrescar_resultados",
              "Actualizar desde Google",
              icon = icon("refresh")
            ),


            hr(),


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
                "Nota global" = "Nota_Corta",
                "Comprension conceptual" = "Comprension",
                "Calidad del prototipo" = "Prototipo",
                "Modelo matematico" = "Modelo",
                "Tecnologia y recursos" = "Tecnologia",
                "Comunicacion y trabajo en equipo" = "Comunicacion",
                "Formacion continua" = "Formacion"
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
            ),


            hr(),


            h4(
              "Comprobacion"
            ),


            tableOutput(
              "comparacion_rubricas"
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


      #************************************************************************
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

server <- function(
  input,
  output,
  session
){


  #************************************************************************
  #LOAD CURRENT DATA

  lista_inicio <- normalizar_lista(

    read_table(
      google_admin,
      sheet_lista,
      empty_lista
    )
  )


  equipos_inicio <- read_table(
    google_equipos,
    sheet_cal_equipos,
    empty_equipos
  )


  alumnos_inicio <- read_table(
    google_alumnos,
    sheet_cal_alumnos,
    empty_alumnos
  )


  lista_admin <- reactiveVal(
    lista_inicio
  )


  alumnos_actuales <- reactiveVal(
    lista_activa(
      lista_inicio
    )
  )


  datos_equipos <- reactiveVal(
    equipos_inicio
  )


  datos_alumnos <- reactiveVal(
    alumnos_inicio
  )


  promedios_local <- reactiveVal(

    calcular_promedios(
      equipos_inicio,
      alumnos_inicio,
      lista_inicio
    )
  )


  admin_ok <- reactiveVal(
    FALSE
  )


  #************************************************************************
  #REFRESH

  refrescar_datos <- function(){

    lista_nueva <- normalizar_lista(

      read_table(
        google_admin,
        sheet_lista,
        empty_lista
      )
    )


    eq_nuevo <- read_table(
      google_equipos,
      sheet_cal_equipos,
      empty_equipos
    )


    al_nuevo <- read_table(
      google_alumnos,
      sheet_cal_alumnos,
      empty_alumnos
    )


    lista_admin(
      lista_nueva
    )


    alumnos_actuales(
      lista_activa(
        lista_nueva
      )
    )


    datos_equipos(
      eq_nuevo
    )


    datos_alumnos(
      al_nuevo
    )


    promedios_local(

      calcular_promedios(
        eq_nuevo,
        al_nuevo,
        lista_nueva
      )
    )
  }


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


    nota <- get_final(
      grado_actual()
    )


    paste0(
      "Rubrica corta: ",
      formatC(
        nota,
        format = "f",
        digits = 2
      ),
      " / 10    |    Rubrica larga: ",
      formatC(
        nota,
        format = "f",
        digits = 2
      ),
      " / 10"
    )
  })


  #************************************************************************
  #SAVE

  observeEvent(
    input$guardar,
    {


      if(nrow(alumnos_actuales())==0){

        showNotification(
          "Primero se debe cargar la lista de alumnos.",
          type = "error"
        )

        return()
      }


      if(
        is.null(input$profesor) ||
        trimws(input$profesor)==""
      ){

        showNotification(
          "Escribe el nombre del profesor evaluador.",
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
        is.null(input$equipo) ||
        trimws(input$equipo)==""
      ){

        showNotification(
          "Selecciona un equipo.",
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
          is.na(grado) |
          grado < 0 |
          grado > 10
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


      nota_corta <- get_final(
        grado
      )


      #the final long rubric grade MUST be identical
      nota_larga <- nota_corta


      #************************************************************************
      #SHORT TEAM ROW

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

        Nota_Corta = nota_corta,

        stringsAsFactors = FALSE
      )


      #************************************************************************
      #SHORT STUDENT ROWS

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

        Nota_Corta = rep(
          nota_corta,
          length(miembros)
        ),

        stringsAsFactors = FALSE
      )


      #************************************************************************
      #LONG RUBRIC ROWS

      largo_equipo <- convertir_larga_equipos(
        nuevo_equipo
      )


      largos_alumnos <- convertir_larga_alumnos(
        nuevos_alumnos
      )


      #extra check
      largo_equipo$Nota_Larga <- nota_larga

      largos_alumnos$Nota_Larga <- rep(
        nota_larga,
        nrow(largos_alumnos)
      )


      #************************************************************************
      #GOOGLE SAVE

      resultado <- tryCatch({


        withProgress(

          message = "Guardando evaluacion...",

          value = 0.2,

          {


            sheet_append(
              google_equipos,
              nuevo_equipo,
              sheet = sheet_cal_equipos
            )


            incProgress(
              0.2
            )


            sheet_append(
              google_alumnos,
              nuevos_alumnos,
              sheet = sheet_cal_alumnos
            )


            incProgress(
              0.2
            )


            sheet_append(
              google_equipos,
              largo_equipo,
              sheet = sheet_larga_equipos
            )


            incProgress(
              0.2
            )


            sheet_append(
              google_alumnos,
              largos_alumnos,
              sheet = sheet_larga_alumnos
            )


            incProgress(
              0.2
            )
          }
        )


        TRUE

      },error=function(e){


        showNotification(

          paste(
            "Google Sheets error:",
            e$message
          ),

          type = "error",

          duration = 12
        )


        FALSE
      })


      if(!resultado){

        return()
      }


      #************************************************************************
      #LOCAL UPDATE

      eq_actual <- datos_equipos()


      if(nrow(eq_actual)==0){

        eq_actual <- nuevo_equipo

      }else{

        eq_actual <- rbind(
          eq_actual,
          nuevo_equipo
        )
      }


      al_actual <- datos_alumnos()


      if(nrow(al_actual)==0){

        al_actual <- nuevos_alumnos

      }else{

        al_actual <- rbind(
          al_actual,
          nuevos_alumnos
        )
      }


      datos_equipos(
        eq_actual
      )


      datos_alumnos(
        al_actual
      )


      promedios_local(

        calcular_promedios(
          eq_actual,
          al_actual,
          lista_admin()
        )
      )


      showNotification(

        paste0(
          "Calificacion enviada. Equipo ",
          equipo_actual,
          ": ",
          formatC(
            nota_corta,
            format = "f",
            digits = 2
          ),
          " / 10"
        ),

        type = "message",

        duration = 7
      )
    }
  )


  #************************************************************************
  #REFRESH RESULTS

  observeEvent(
    input$refrescar_resultados,
    {

      resultado <- tryCatch({

        refrescar_datos()

        TRUE

      },error=function(e){

        showNotification(
          paste(
            "No se pudieron actualizar los datos:",
            e$message
          ),
          type = "error",
          duration = 10
        )

        FALSE
      })


      if(resultado){

        showNotification(
          "Resultados actualizados desde Google.",
          type = "message"
        )
      }
    }
  )


  #************************************************************************
  #RESULTS TABLE

  output$tabla_promedios <- renderTable({


    p <- promedios_local()


    if(input$tipo_grafica=="equipos"){

      p$equipos

    }else{

      p$alumnos
    }

  },digits = 2)


  #************************************************************************
  #GENERAL AVERAGE

  obtener_promedio_general <- reactive({


    p <- promedios_local()


    if(input$tipo_grafica=="equipos"){

      datos <- p$equipos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        return(
          NA_real_
        )
      }


      x <- datos[
        datos$Equipo=="PROMEDIO GENERAL",
        ,
        drop = FALSE
      ]

    }else{

      datos <- p$alumnos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        return(
          NA_real_
        )
      }


      x <- datos[
        datos$Alumno=="PROMEDIO GENERAL",
        ,
        drop = FALSE
      ]
    }


    if(
      nrow(x)==0 ||
      is.na(x$Nota_Corta[1])
    ){

      return(
        NA_real_
      )
    }


    as.numeric(
      x$Nota_Corta[1]
    )
  })


  output$promedio_general <- renderText({


    x <- obtener_promedio_general()


    if(is.na(x)){

      return(
        "Sin calificaciones"
      )
    }


    paste0(
      formatC(
        x,
        format = "f",
        digits = 2
      ),
      " / 10"
    )
  })


  #************************************************************************
  #SHORT VS LONG CHECK

  output$comparacion_rubricas <- renderTable({


    x <- obtener_promedio_general()


    if(is.na(x)){

      return(

        data.frame(
          Rubrica = c(
            "Corta",
            "Larga"
          ),
          Promedio = c(
            NA_real_,
            NA_real_
          )
        )
      )
    }


    data.frame(

      Rubrica = c(
        "Corta",
        "Larga"
      ),

      Promedio = c(
        x,
        x
      ),

      check.names = FALSE
    )

  },digits = 2)


  #************************************************************************
  #GRAPH

  output$grafica <- renderPlot({


    p <- promedios_local()


    if(input$tipo_grafica=="equipos"){

      datos <- p$equipos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        plot.new()

        text(
          0.5,
          0.5,
          "Sin calificaciones"
        )

        return()
      }


      datos <- datos[
        datos$Equipo!="PROMEDIO GENERAL",
        ,
        drop = FALSE
      ]


      etiquetas <- datos$Equipo

    }else{

      datos <- p$alumnos


      if(
        is.null(datos) ||
        nrow(datos)==0
      ){

        plot.new()

        text(
          0.5,
          0.5,
          "Sin calificaciones"
        )

        return()
      }


      datos <- datos[
        datos$Alumno!="PROMEDIO GENERAL",
        ,
        drop = FALSE
      ]


      etiquetas <- datos$Alumno
    }


    if(nrow(datos)==0){

      plot.new()

      text(
        0.5,
        0.5,
        "Sin calificaciones"
      )

      return()
    }


    criterio <- input$criterio_grafica


    valores <- suppressWarnings(
      as.numeric(
        datos[[criterio]]
      )
    )


    valores_plot <- valores

    valores_plot[
      is.na(valores_plot)
    ] <- 0


    barplot(

      valores_plot,

      names.arg = etiquetas,

      las = 2,

      ylim = c(
        0,
        10
      ),

      ylab = "Calificacion",

      main = "Resultados"
    )


    abline(
      h = seq(
        0,
        10,
        1
      ),
      lty = 3
    )
  })


  #************************************************************************
  #ADMIN UI

  output$admin_ui <- renderUI({


    if(!admin_ok()){


      return(

        tagList(

          h4(
            "Acceso de administracion"
          ),


          tags$p(
            "Solo el profesor Montufar tiene acceso a esta seccion mediante su contraseña."
          ),


          passwordInput(
            "admin_password",
            "Contraseña:"
          ),


          actionButton(
            "login_admin",
            "Entrar",
            icon = icon("sign-in")
          )
        )
      )
    }


    lista <- lista_admin()


    opciones_alumnos <- if(
      nrow(lista)>0
    ){

      setNames(

        seq_len(
          nrow(lista)
        ),

        paste0(
          lista$Equipo,
          " - ",
          lista$Alumno,
          " [",
          lista$Activo,
          "]"
        )
      )

    }else{

      character()
    }


    tagList(

      fluidRow(

        column(

          width = 9,

          h3(
            semestre
          ),

          tags$p(
            "Administracion central de alumnos, rubricas y Google Sheets."
          )
        ),


        column(

          width = 3,

          actionButton(
            "cerrar_admin",
            "Cerrar administracion",
            icon = icon("sign-out")
          )
        )
      ),


      tabsetPanel(

        tabPanel(

          "Alumnos",


          br(),


          fluidRow(

            column(

              width = 6,


              h4(
                "Cargar lista"
              ),


              fileInput(

                "archivo_lista",

                "Archivo .xlsx, .xls o .csv",

                accept = c(
                  ".xlsx",
                  ".xls",
                  ".csv"
                )
              ),


              actionButton(
                "cargar_lista",
                "Reemplazar lista del semestre",
                icon = icon("upload")
              ),


              tags$small(
                "El archivo debe contener Equipo y Alumno. Activo es opcional."
              )
            ),


            column(

              width = 6,


              h4(
                "Agregar alumno"
              ),


              textInput(
                "nuevo_equipo",
                "Equipo:"
              ),


              textInput(
                "nuevo_alumno",
                "Alumno:"
              ),


              actionButton(
                "agregar_alumno",
                "Agregar",
                icon = icon("plus")
              )
            )
          ),


          hr(),


          h4(
            "Activar / desactivar alumno"
          ),


          selectInput(

            "alumno_admin",

            "Alumno:",

            choices = opciones_alumnos
          ),


          actionButton(
            "desactivar_alumno",
            "Desactivar",
            icon = icon("ban")
          ),


          actionButton(
            "activar_alumno",
            "Activar",
            icon = icon("check")
          ),


          hr(),


          tableOutput(
            "tabla_lista_admin"
          )
        ),


        tabPanel(

          "Rubricas",


          br(),


          h4(
            "Rubrica corta"
          ),


          tags$p(
            "Esta es la rubrica que los profesores evaluadores califican."
          ),


          tableOutput(
            "tabla_rubrica_corta"
          ),


          hr(),


          h4(
            "Rubrica larga P.C. + U.T."
          ),


          tags$p(
            "Los criterios largos se derivan de la rubrica corta. La Nota_Larga siempre es igual a Nota_Corta."
          ),


          tableOutput(
            "tabla_rubrica_larga"
          )
        ),


        tabPanel(

          "Google / Mantenimiento",


          br(),


          tags$p(
            strong(
              "Los tres Google Sheets fueron creados con la cuenta del profesor Montufar."
            )
          ),


          tags$p(
            "La aplicacion usa la autorizacion guardada de marco.montufar14@gmail.com."
          ),


          tags$p(
            "Estos enlaces son para que Montufar pueda abrir directamente sus archivos en Google Drive."
          ),


          tags$ul(

            tags$li(

              tags$a(
                href = google_admin_url,
                target = "_blank",
                "Administracion Rubricas UAEH"
              )
            ),


            tags$li(

              tags$a(
                href = google_equipos_url,
                target = "_blank",
                "Evaluaciones Rubricas UAEH - Equipos"
              )
            ),


            tags$li(

              tags$a(
                href = google_alumnos_url,
                target = "_blank",
                "Evaluaciones Rubricas UAEH - Alumnos"
              )
            )
          ),


          hr(),


          actionButton(
            "refrescar_admin",
            "Actualizar datos desde Google",
            icon = icon("refresh")
          ),


          br(),
          br(),


          actionButton(
            "actualizar_derivados",
            "Actualizar Promedios y Rubrica Larga",
            icon = icon("calculator"),
            class = "btn-success"
          ),


          tags$p(
            "Este boton relee las calificaciones originales y vuelve a generar las hojas derivadas. No modifica las evaluaciones originales."
          )
        )
      )
    )
  })


  #************************************************************************
  #ADMIN LOGIN

  observeEvent(
    input$login_admin,
    {


      if(
        !is.null(input$admin_password) &&
        admin_password_ok(
          input$admin_password
        )
      ){

        admin_ok(
          TRUE
        )


        updateTextInput(
          session,
          "admin_password",
          value = ""
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
    }
  )


  observeEvent(
    input$cerrar_admin,
    {

      admin_ok(
        FALSE
      )


      showNotification(
        "Administracion cerrada.",
        type = "message"
      )
    }
  )


  #************************************************************************
  #ADMIN TABLES

  output$tabla_lista_admin <- renderTable({


    req(
      admin_ok()
    )


    lista_admin()

  })


  output$tabla_rubrica_corta <- renderTable({


    req(
      admin_ok()
    )


    rubrica[
      ,
      c(
        "Numero",
        "Aspecto",
        "Descripcion",
        "Peso"
      )
    ]

  },digits = 2)


  output$tabla_rubrica_larga <- renderTable({


    req(
      admin_ok()
    )


    rubrica_larga

  })


  #************************************************************************
  #UPLOAD ROSTER

  observeEvent(
    input$cargar_lista,
    {


      req(
        admin_ok()
      )


      if(is.null(input$archivo_lista)){

        showNotification(
          "Selecciona un archivo.",
          type = "error"
        )

        return()
      }


      resultado <- tryCatch({


        nueva_lista <- leer_lista_archivo(

          input$archivo_lista$datapath,

          input$archivo_lista$name
        )


        if(nrow(nueva_lista)==0){

          stop(
            "La lista no contiene alumnos validos."
          )
        }


        sheet_write(

          nueva_lista,

          ss = google_admin,

          sheet = sheet_lista
        )


        lista_admin(
          nueva_lista
        )


        alumnos_actuales(
          lista_activa(
            nueva_lista
          )
        )


        promedios_local(

          calcular_promedios(
            datos_equipos(),
            datos_alumnos(),
            nueva_lista
          )
        )


        TRUE

      },error=function(e){


        showNotification(

          paste(
            "No se pudo cargar la lista:",
            e$message
          ),

          type = "error",

          duration = 10
        )


        FALSE
      })


      if(resultado){

        showNotification(
          "Lista de alumnos actualizada.",
          type = "message"
        )
      }
    }
  )


  #************************************************************************
  #ADD STUDENT

  observeEvent(
    input$agregar_alumno,
    {


      req(
        admin_ok()
      )


      equipo <- trimws(
        input$nuevo_equipo
      )

      alumno <- trimws(
        input$nuevo_alumno
      )


      if(
        equipo=="" ||
        alumno==""
      ){

        showNotification(
          "Escribe equipo y nombre del alumno.",
          type = "error"
        )

        return()
      }


      lista <- lista_admin()


      nueva_fila <- data.frame(

        Equipo = equipo,
        Alumno = alumno,
        Activo = "SI",

        stringsAsFactors = FALSE
      )


      lista <- normalizar_lista(

        rbind(
          lista,
          nueva_fila
        )
      )


      resultado <- tryCatch({


        sheet_write(

          lista,

          ss = google_admin,

          sheet = sheet_lista
        )


        TRUE

      },error=function(e){


        showNotification(

          paste(
            "No se pudo guardar:",
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


      lista_admin(
        lista
      )


      alumnos_actuales(
        lista_activa(
          lista
        )
      )


      promedios_local(

        calcular_promedios(
          datos_equipos(),
          datos_alumnos(),
          lista
        )
      )


      updateTextInput(
        session,
        "nuevo_equipo",
        value = ""
      )


      updateTextInput(
        session,
        "nuevo_alumno",
        value = ""
      )


      showNotification(
        "Alumno agregado.",
        type = "message"
      )
    }
  )


  #************************************************************************
  #ACTIVATE / DEACTIVATE

  cambiar_estado_alumno <- function(
    indice,
    estado
  ){

    lista <- lista_admin()


    if(
      length(indice)==0 ||
      is.na(indice) ||
      indice < 1 ||
      indice > nrow(lista)
    ){

      showNotification(
        "Selecciona un alumno.",
        type = "error"
      )

      return(
        FALSE
      )
    }


    lista$Activo[indice] <- estado


    resultado <- tryCatch({


      sheet_write(

        lista,

        ss = google_admin,

        sheet = sheet_lista
      )


      TRUE

    },error=function(e){


      showNotification(

        paste(
          "No se pudo guardar:",
          e$message
        ),

        type = "error",

        duration = 10
      )


      FALSE
    })


    if(!resultado){

      return(
        FALSE
      )
    }


    lista_admin(
      lista
    )


    alumnos_actuales(
      lista_activa(
        lista
      )
    )


    promedios_local(

      calcular_promedios(
        datos_equipos(),
        datos_alumnos(),
        lista
      )
    )


    TRUE
  }


  observeEvent(
    input$desactivar_alumno,
    {


      req(
        admin_ok()
      )


      indice <- suppressWarnings(
        as.integer(
          input$alumno_admin
        )
      )


      if(
        cambiar_estado_alumno(
          indice,
          "NO"
        )
      ){

        showNotification(
          "Alumno desactivado.",
          type = "message"
        )
      }
    }
  )


  observeEvent(
    input$activar_alumno,
    {


      req(
        admin_ok()
      )


      indice <- suppressWarnings(
        as.integer(
          input$alumno_admin
        )
      )


      if(
        cambiar_estado_alumno(
          indice,
          "SI"
        )
      ){

        showNotification(
          "Alumno activado.",
          type = "message"
        )
      }
    }
  )


  #************************************************************************
  #ADMIN REFRESH

  observeEvent(
    input$refrescar_admin,
    {


      req(
        admin_ok()
      )


      resultado <- tryCatch({

        refrescar_datos()

        TRUE

      },error=function(e){

        showNotification(
          paste(
            "No se pudieron actualizar los datos:",
            e$message
          ),
          type = "error",
          duration = 10
        )

        FALSE
      })


      if(resultado){

        showNotification(
          "Datos actualizados desde Google.",
          type = "message"
        )
      }
    }
  )


  #************************************************************************
  #REBUILD DERIVED SHEETS
  #
  #The original short evaluations are never rewritten here.
  #Promedios and Rubrica Larga are rebuilt from the original rows.
  #************************************************************************

  observeEvent(
    input$actualizar_derivados,
    {


      req(
        admin_ok()
      )


      resultado <- tryCatch({


        withProgress(

          message = "Actualizando hojas derivadas...",

          value = 0.1,

          {


            #always reread original evaluations first
            eq <- read_table(
              google_equipos,
              sheet_cal_equipos,
              empty_equipos
            )


            al <- read_table(
              google_alumnos,
              sheet_cal_alumnos,
              empty_alumnos
            )


            lista <- normalizar_lista(

              read_table(
                google_admin,
                sheet_lista,
                empty_lista
              )
            )


            incProgress(
              0.2
            )


            p <- calcular_promedios(
              eq,
              al,
              lista
            )


            sheet_write(

              p$equipos,

              ss = google_equipos,

              sheet = sheet_prom_equipos
            )


            incProgress(
              0.15
            )


            sheet_write(

              p$alumnos,

              ss = google_alumnos,

              sheet = sheet_prom_alumnos
            )


            incProgress(
              0.15
            )


            largo_eq <- convertir_larga_equipos(
              eq
            )


            largo_al <- convertir_larga_alumnos(
              al
            )


            #this enforces equality again
            if(nrow(largo_eq)>0){

              largo_eq$Nota_Larga <- largo_eq$Nota_Corta
            }


            if(nrow(largo_al)>0){

              largo_al$Nota_Larga <- largo_al$Nota_Corta
            }


            sheet_write(

              largo_eq,

              ss = google_equipos,

              sheet = sheet_larga_equipos
            )


            incProgress(
              0.15
            )


            sheet_write(

              largo_al,

              ss = google_alumnos,

              sheet = sheet_larga_alumnos
            )


            incProgress(
              0.15
            )


            #also update rubric reference tabs
            sheet_write(

              rubrica,

              ss = google_admin,

              sheet = "Rubrica Corta"
            )


            sheet_write(

              rubrica_larga,

              ss = google_admin,

              sheet = "Rubrica Larga"
            )


            #local state
            lista_admin(
              lista
            )


            alumnos_actuales(
              lista_activa(
                lista
              )
            )


            datos_equipos(
              eq
            )


            datos_alumnos(
              al
            )


            promedios_local(
              p
            )
          }
        )


        TRUE

      },error=function(e){


        showNotification(

          paste(
            "No se pudieron actualizar las hojas:",
            e$message
          ),

          type = "error",

          duration = 12
        )


        FALSE
      })


      if(resultado){

        showNotification(

          paste0(
            "Promedios y rubrica larga actualizados. ",
            "Nota larga = nota corta."
          ),

          type = "message",

          duration = 8
        )
      }
    }
  )
}


#************************************************************************
#RUN

shinyApp(
  ui = ui,
  server = server
)
