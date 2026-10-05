
GOOGLE_EMAIL <- "marco.montufar14@gmail.com"


#************************************************************************
#PACKAGES

paquetes <- c(
  "googlesheets4",
  "digest"
)


faltantes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if(length(faltantes)>0){

  install.packages(
    faltantes
  )
}


#************************************************************************
#FOLDERS

private_dir <- file.path(
  getwd(),
  "private"
)

google_cache <- file.path(
  private_dir,
  "google-token"
)

config_file <- file.path(
  private_dir,
  "config.rds"
)


if(!dir.exists(private_dir)){

  dir.create(
    private_dir,
    recursive = TRUE
  )
}


if(!dir.exists(google_cache)){

  dir.create(
    google_cache,
    recursive = TRUE
  )
}


#************************************************************************
#GOOGLE LOGIN

options(
  gargle_oauth_cache = google_cache,
  gargle_oauth_email = GOOGLE_EMAIL
)


cat(
  "\nSe abrira Google para autorizar esta cuenta:\n",
  GOOGLE_EMAIL,
  "\n\n",
  sep = ""
)


googlesheets4::gs4_auth(

  email = GOOGLE_EMAIL,

  cache = google_cache
)


#************************************************************************
#PASSWORD

pedir_password <- function(){

  if(
    requireNamespace(
      "rstudioapi",
      quietly = TRUE
    ) &&
    rstudioapi::isAvailable()
  ){

    return(
      rstudioapi::askForPassword(
        "Contraseña de Administracion para el profesor Montufar"
      )
    )
  }


  readline(
    "Contraseña de Administracion para el profesor Montufar: "
  )
}


password_admin <- pedir_password()


if(nchar(password_admin)<8){

  stop(
    "Usa una contraseña de al menos 8 caracteres."
  )
}


admin_hash <- digest::digest(

  password_admin,

  algo = "sha256",

  serialize = FALSE
)


rm(
  password_admin
)


#************************************************************************
#REUSE OLD SHEETS IF THEY ARE ALREADY CONFIGURED

config_anterior <- NULL


if(file.exists(config_file)){

  config_anterior <- tryCatch(

    readRDS(
      config_file
    ),

    error=function(e){
      NULL
    }
  )
}


ids_ok <- FALSE


if(
  !is.null(config_anterior) &&
  all(
    c(
      "google_admin_sheet_id",
      "google_equipos_sheet_id",
      "google_alumnos_sheet_id"
    ) %in% names(config_anterior)
  )
){

  ids <- c(
    config_anterior$google_admin_sheet_id,
    config_anterior$google_equipos_sheet_id,
    config_anterior$google_alumnos_sheet_id
  )


  pruebas <- vapply(

    ids,

    function(id){

      tryCatch({

        googlesheets4::gs4_get(

          googlesheets4::as_sheets_id(
            id
          )
        )

        TRUE

      },error=function(e){

        FALSE
      })
    },

    logical(1)
  )


  ids_ok <- all(
    pruebas
  )
}


#************************************************************************
#CREATE THE 3 GOOGLE SHEETS
#
#Because the authenticated user is Montufar,
#these files are created in Montufar's Google Drive.
#************************************************************************

if(ids_ok){

  cat(
    "\nYa existen los 3 Google Sheets configurados. Se reutilizaran.\n"
  )


  google_admin_id <- as.character(
    config_anterior$google_admin_sheet_id
  )

  google_equipos_id <- as.character(
    config_anterior$google_equipos_sheet_id
  )

  google_alumnos_id <- as.character(
    config_anterior$google_alumnos_sheet_id
  )


}else{

  cat(
    "\nCreando los 3 Google Sheets en el Drive de Montufar...\n"
  )


  inicio_admin <- data.frame(

    Mensaje = "Archivo administrativo del Sistema de Rubricas UAEH",

    stringsAsFactors = FALSE
  )


  inicio_equipos <- data.frame(

    Mensaje = "Evaluaciones por equipo del Sistema de Rubricas UAEH",

    stringsAsFactors = FALSE
  )


  inicio_alumnos <- data.frame(

    Mensaje = "Evaluaciones por alumno del Sistema de Rubricas UAEH",

    stringsAsFactors = FALSE
  )


  google_admin <- googlesheets4::gs4_create(

    "Administracion Rubricas UAEH",

    sheets = list(
      Inicio = inicio_admin
    )
  )


  google_equipos <- googlesheets4::gs4_create(

    "Evaluaciones Rubricas UAEH - Equipos",

    sheets = list(
      Inicio = inicio_equipos
    )
  )


  google_alumnos <- googlesheets4::gs4_create(

    "Evaluaciones Rubricas UAEH - Alumnos",

    sheets = list(
      Inicio = inicio_alumnos
    )
  )


  google_admin_id <- as.character(
    google_admin
  )

  google_equipos_id <- as.character(
    google_equipos
  )

  google_alumnos_id <- as.character(
    google_alumnos
  )
}


#************************************************************************
#SAVE CONFIG

config <- list(

  google_email = GOOGLE_EMAIL,

  google_admin_sheet_id = google_admin_id,

  google_equipos_sheet_id = google_equipos_id,

  google_alumnos_sheet_id = google_alumnos_id,

  admin_hash = admin_hash
)


saveRDS(
  config,
  config_file
)


#************************************************************************
#DONE

cat(
  "\n\nLISTO\n",
  "---------------------------------------------\n",
  "Google usado: ",
  GOOGLE_EMAIL,
  "\n",
  "Se guardo la autorizacion en: private/google-token/\n",
  "Se guardo la configuracion en: private/config.rds\n",
  "Los Google Sheets pertenecen a Montufar.\n\n",
  "Ahora ejecuta:\n",
  "shiny::runApp()\n\n",
  sep = ""
)
