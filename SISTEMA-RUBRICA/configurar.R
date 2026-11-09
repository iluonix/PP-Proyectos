#************************************************************************
#CONFIGURAR SISTEMA DE RÚBRICAS - PROFESOR MONTÚFAR
#Ejecute source("configurar.R") en RStudio después de clonar el proyecto.
#
#No guarde contraseñas ni credenciales en GitHub.
#Los tres Google Sheets se crean en el Drive de la cuenta autenticada.
#Este script intenta reutilizar los archivos previamente configurados.
#************************************************************************

GOOGLE_EMAIL <- "marco.montufar14@gmail.com"

paquetes <- c("googlesheets4", "sodium")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly=TRUE)]
if(length(faltantes)>0) {
  stop(paste("Instale los paquetes necesarios con source('instalar_paquetes.R'):",
             paste(faltantes,collapse=", ")))
}

private_dir <- file.path(getwd(), "private")
cache_path <- file.path(private_dir, "google-token")
config_path <- file.path(private_dir, "config.rds")

dir.create(cache_path, recursive=TRUE, showWarnings=FALSE)
options(gargle_oauth_cache=cache_path, gargle_oauth_email=GOOGLE_EMAIL)

cat("\nSe solicitará autorización de Google para:\n", GOOGLE_EMAIL, "\n\n")
googlesheets4::gs4_auth(email=GOOGLE_EMAIL, cache=cache_path)

#************************************************************************
#REUTILIZAR IDS SI YA EXISTEN

config <- if(file.exists(config_path)) readRDS(config_path) else list()

if(!is.null(config$google_email) &&
   tolower(trimws(as.character(config$google_email))) != tolower(GOOGLE_EMAIL)){
  stop(paste0("La carpeta private/ pertenece a otro correo (",
              config$google_email,"). No debe reutilizarse para Montúfar."))
}

config$google_email <- GOOGLE_EMAIL

#Si el profesor ya tiene los archivos, puede reutilizarlos desde otra PC.
ids_necesarios <- c("google_admin_sheet_id", "google_equipos_sheet_id", "google_alumnos_sheet_id")
ids_faltantes <- vapply(ids_necesarios, function(campo){
  id <- config[[campo]]
  is.null(id) || length(id)!=1 || is.na(id) || !nzchar(as.character(id))
}, logical(1))

extraer_id <- function(texto){
  texto <- trimws(texto)
  if(grepl("/spreadsheets/d/",texto,fixed=TRUE)){
    texto <- sub("^.*?/spreadsheets/d/", "", texto)
    texto <- sub("[/?].*$", "", texto)
  }
  texto
}

if(any(ids_faltantes)){
  cat("\nSi estos tres Google Sheets ya existen en su Drive, puede reutilizarlos.\n")
  respuesta <- tolower(trimws(readline("¿Ya tiene los tres archivos? (s/N): ")))
  if(respuesta %in% c("s", "si", "sí")){
    etiquetas <- c("Administración", "Equipos", "Alumnos")
    for(i in seq_along(ids_necesarios)){
      campo <- ids_necesarios[i]
      if(ids_faltantes[i]){
        entrada <- readline(paste0("Pegue el enlace de Google Sheets de ", etiquetas[i], ": "))
        id <- extraer_id(entrada)
        if(!nzchar(id)) stop("Los tres enlaces son obligatorios para reutilizarlos.")
        googlesheets4::gs4_get(googlesheets4::as_sheets_id(id))
        config[[campo]] <- id
        saveRDS(config, config_path)
      }
    }
  }
}

archivos <- list(
  list(campo="google_admin_sheet_id", nombre="Administracion Rubricas UAEH"),
  list(campo="google_equipos_sheet_id", nombre="Evaluaciones Rubricas UAEH - Equipos"),
  list(campo="google_alumnos_sheet_id", nombre="Evaluaciones Rubricas UAEH - Alumnos")
)

for(archivo in archivos){
  campo <- archivo$campo
  actual <- config[[campo]]

  if(!is.null(actual) && length(actual)==1 && !is.na(actual) && nzchar(actual)){
    #No crear duplicados si un archivo existente temporalmente no es accesible.
    tryCatch(
      googlesheets4::gs4_get(googlesheets4::as_sheets_id(actual)),
      error=function(e){
        stop(paste0("No se pudo abrir ",archivo$nombre,
          ". Se conserva el ID previo para evitar duplicados. Detalle: ",e$message))
      }
    )
    cat("Reutilizando: ", archivo$nombre, "\n", sep="")
    next
  }

  cat("Creando: ", archivo$nombre, "\n", sep="")
  hoja <- googlesheets4::gs4_create(
    archivo$nombre,
    sheets=list(Inicio=data.frame(
      Descripcion=paste("Archivo creado para el sistema de rúbricas:",archivo$nombre),
      stringsAsFactors=FALSE
    ))
  )

  config[[campo]] <- as.character(hoja)
  #Guardar el ID inmediatamente para evitar duplicados si se interrumpe el script.
  saveRDS(config,config_path)
}

#************************************************************************
#CONTRASEÑA DE ADMINISTRACIÓN
#La V4 verifica el hash de la pestaña 'Acceso' (no el hash antiguo local).

ss_admin <- googlesheets4::as_sheets_id(config$google_admin_sheet_id)
hojas_admin <- googlesheets4::sheet_names(ss_admin)

crear_password <- TRUE
if("Acceso" %in% hojas_admin){
  existente <- tryCatch(
    as.data.frame(googlesheets4::read_sheet(ss_admin,sheet="Acceso")),
    error=function(e) stop(paste("No se pudo comprobar la contraseña existente:",e$message))
  )
  if(nrow(existente)>0 && "Hash" %in% names(existente) &&
     !is.na(existente$Hash[1]) && nzchar(as.character(existente$Hash[1]))){
    crear_password <- FALSE
    cat("\nYa existe una contraseña administrativa en Google y no se modificará.\n")
    cat("Si la ha olvidado, ejecute source('recuperar_password.R').\n")
  }
}

if(crear_password){
  if(requireNamespace("rstudioapi",quietly=TRUE) && rstudioapi::isAvailable()){
    nueva <- rstudioapi::askForPassword(
      "Contraseña inicial de Administración (12 caracteres mínimo)"
    )
    repetir <- rstudioapi::askForPassword("Confirme la contraseña")
  }else{
    nueva <- readline("Contraseña de Administración (12 caracteres mínimo): ")
    repetir <- readline("Confirme la contraseña: ")
  }

  if(is.null(nueva) || nchar(nueva)<12 || !identical(nueva,repetir)){
    stop("La contraseña debe tener al menos 12 caracteres y coincidir con la confirmación.")
  }

  hash <- sodium::password_store(nueva)
  googlesheets4::sheet_write(
    data.frame(Hash=hash,stringsAsFactors=FALSE),
    ss=ss_admin,
    sheet="Acceso"
  )
  rm(nueva,repetir,hash)
  cat("La contraseña de Administración ha sido guardada.\n")
}

#La configuración local contiene IDs y correo, no contraseñas.
saveRDS(config,config_path)

cat("\nCONFIGURACIÓN FINALIZADA\n")
cat("Cuenta de Google: ",GOOGLE_EMAIL,"\n",sep="")
cat("Configuración privada: private/config.rds\n")
cat("Autorización Google: private/google-token/\n")
cat("Ahora ejecute: shiny::runApp()\n\n")
