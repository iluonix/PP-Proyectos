instalar_paquetes <- source(".Rprofile", local = TRUE, encoding = "UTF-8")$value
instalar_paquetes()

library(shiny)
library(shinydashboard)
library(googlesheets4)
library(readxl)
library(sodium)

#************************************************************************
#GOOGLE / PRIVATE
`%||%` <- function(x,y){if(is.null(x) || length(x)==0) y else x}

#************************************************************************
#SEMESTRE

get_semestre <- function(fecha=Sys.Date()){
  m <- as.integer(format(fecha, "%m"))
  y <- format(fecha, "%Y")
  if(m<=6) paste("Enero - Junio", y) else paste("Julio - Diciembre", y)
}

semestre <- get_semestre()
nombres_hojas <- function(s){
  list(
    lista = s,
    equipos = paste("Calificaciones", s, sep=" - "),
    alumnos = paste("Calificaciones", s, sep=" - "),
    promedios = paste("Promedios", s, sep=" - "),
    largas = paste("Rubrica Larga", s, sep=" - ")
  )
}
hojas <- nombres_hojas(semestre)

#************************************************************************
#RUBRICA CORTA - 6 ASPECTOS / ESCALA 0-10

rubrica <- data.frame(
  RC = 1:6,
  Aspecto = c(
    "Comprensión conceptual",
    "Calidad y funcionamiento del prototipo",
    "Modelo matemático y análisis",
    "Uso de tecnología y recursos",
    "Comunicación y trabajo en equipo",
    "Formación continua y aprendizaje autónomo"
  ),
  Peso = c(20,20,20,20,10,10),
  Descripcion = c(
    "Explica los conceptos fundamentales que sustentan el proyecto.",
    "Demuestra que el prototipo funciona y cumple sus objetivos.",
    "Relaciona ecuaciones, resultados y limitaciones del modelo.",
    "Utiliza adecuadamente las herramientas TIC y los recursos disponibles.",
    "Presenta las ideas con claridad y demuestra una participación coordinada.",
    "Identifica necesidades de aprendizaje y plantea cómo continuar formándose."
  ),
  RL_relacionada = c("RL1, RL2", "RL3, RL7, RL8", "RL2, RL3",
                    "RL6, RL7, RL8", "RL1 (indirecta)", "RL4, RL5"),
  stringsAsFactors = FALSE
)

rubrica_larga <- data.frame(
  RL = 1:8,
  Rubrica = c(rep("P.C.",5),rep("U.T.",3)),
  Aspecto = c(
    "Extraer información y adaptar los problemas",
    "Aplica principios matemáticos y físicos",
    "Evaluación de las limitaciones",
    "Identifican las necesidades de formación continua",
    "Desarrollan un plan de formación continua",
    "Selección de la herramienta TIC",
    "Uso de herramientas TIC",
    "Evaluación de las limitaciones de las herramientas TIC"
  ),
  Fuente_RC = c("RC1 + RC5", "RC1 + RC3", "RC2 + RC3", "RC6", "RC6",
                "RC4", "RC2 + RC4", "RC2 + RC4"),
  stringsAsFactors = FALSE
)

#IMPORTANTE: La RL no contempla directamente la comunicacion / trabajo en equipo.
#El vinculo RC5 -> RL1 es indirecto (comunicar y adaptar los problemas).
#No equivale a una medicion independiente de ese criterio.

calificacion_corta <- function(g){
  round(sum(as.numeric(g) * rubrica$Peso)/100,2)
}

calificaciones_largas <- function(g){
  #1 Comprension; 2 Prototipo; 3 Modelo; 4 Tecnologia; 5 Comunicacion; 6 Formacion
  c(mean(c(g[1],g[5])), mean(c(g[1],g[3])), mean(c(g[2],g[3])),
    g[6], g[6], g[4], mean(c(g[2],g[4])), mean(c(g[2],g[4])))
}

#************************************************************************
#CAMPOS DEL DRIVE - LOS TRES CAMPOS SE LLAMAN COMO SE SOLICITO

criterios <- c("Comprension","Prototipo","Modelo","Tecnologia","Comunicacion","Formacion")
calificaciones <- c("Calificación RC", "Calificación RL", "Promedio")
campos_comunes <- c("ID","Fecha local","Semestre",
                    "Profesor","Materia / grupo","Equipo")

campos_corta <- function(tipo){
  c(campos_comunes, if(tipo=="equipo") "Integrantes" else "Alumno",
    criterios,calificaciones)
}

campos_larga <- function(tipo){
  c(campos_comunes, if(tipo=="equipo") "Integrantes" else "Alumno",
    paste0("PC_",1:5),paste0("UT_",1:3),"Promedio PC","Promedio UT",
    calificaciones)
}

vacia <- function(columnas){
  numericas <- c(criterios,calificaciones,paste0("PC_",1:5),paste0("UT_",1:3),
                "Promedio PC","Promedio UT")
  valores <- lapply(columnas, function(n){
    if(n %in% numericas) numeric() else character()
  })
  as.data.frame(setNames(valores,columnas),check.names=FALSE)
}

empty_lista <- data.frame(Equipo=character(), Alumno=character(),
                          Activo=character(),stringsAsFactors=FALSE)
empty_equipos <- vacia(campos_corta("equipo"))
empty_alumnos <- vacia(campos_corta("alumno"))
empty_larga_eq <- vacia(campos_larga("equipo"))
empty_larga_al <- vacia(campos_larga("alumno"))

#************************************************************************
#HELPERS GOOGLE

existe_hoja <- function(ss, hoja){ hoja %in% sheet_names(ss) }

asegurar_hoja <- function(ss,hoja,plantilla){
  if(!existe_hoja(ss,hoja)) sheet_write(plantilla,ss=ss,sheet=hoja)
}

leer_hoja <- function(ss,hoja,plantilla){
  if(!existe_hoja(ss,hoja)) return(plantilla)
  #ATENCION: read_sheet NO acepta show_col_types.
  x <- as.data.frame(read_sheet(ss,sheet=hoja),check.names=FALSE)
  if(nrow(x)==0) return(plantilla)
  x
}

#************************************************************************
#ESQUEMA DE LAS HOJAS DE EVALUACIÓN

normalizar_encabezados <- function(encabezados){
  sin_acentos <- iconv(encabezados, to="ASCII//TRANSLIT", sub="")
  gsub("[^a-z0-9]", "", tolower(sin_acentos))
}

orden_columnas <- function(actuales, esperados){
  claves <- normalizar_encabezados(actuales)
  requeridas <- normalizar_encabezados(esperados)
  apariciones <- vapply(requeridas, function(clave) sum(claves == clave), integer(1))
  if(any(apariciones != 1L)) return(NULL)
  match(requeridas, claves)
}

migrar_campos <- function(ss, hoja, plantilla){
  if(!existe_hoja(ss, hoja)){
    sheet_write(plantilla, ss=ss, sheet=hoja)
    return(invisible(TRUE))
  }

  esperados <- names(plantilla)
  primera_fila <- range_read_cells(ss, sheet=hoja, range="1:1")
  actuales <- vapply(primera_fila$cell, function(celda){
    celda$effectiveValue$stringValue %||% ""
  }, character(1))

  if(identical(actuales, esperados) &&
     identical(primera_fila$col, seq_along(esperados))){
    return(invisible(FALSE))
  }

  celdas <- range_read_cells(ss, sheet=hoja, cell_data="full", discard_empty=FALSE)
  tiene_valor <- vapply(celdas$cell, function(celda){
    !is.null(celda$userEnteredValue) || !is.null(celda$effectiveValue)
  }, logical(1))
  resultado <- plantilla
  trasladadas <- FALSE
  con_datos <- any(celdas$row[tiene_valor] > 1L)
  if(con_datos && !is.null(orden_columnas(actuales, esperados))){
    datos <- as.data.frame(read_sheet(ss, sheet=hoja, trim_ws=FALSE,
                                    .name_repair="minimal"), check.names=FALSE)
    orden <- orden_columnas(names(datos), esperados)
    if(!is.null(orden)){
      resultado <- datos[, orden, drop=FALSE]
      names(resultado) <- esperados
      trasladadas <- TRUE
    }
  }

  if(any(tiene_valor)){
    base <- paste("Respaldo", hoja, format(Sys.time(), "%Y%m%d-%H%M%S"))
    existentes <- tolower(sheet_names(ss))
    respaldo <- base
    sufijo <- 1L
    while(tolower(respaldo) %in% existentes){
      sufijo <- sufijo + 1L
      respaldo <- paste(base, sufijo)
    }
    sheet_copy(ss, from_sheet=hoja, to_sheet=respaldo)
    message("Pestaña original guardada en «", respaldo, "».")
  }

  sheet_write(resultado, ss=ss, sheet=hoja)
  if(con_datos && !trasladadas){
    warning(paste0("Se creó una tabla vacía en «", hoja,
                   "» porque sus encabezados no permiten asignar las calificaciones con certeza. ",
                   "Los datos anteriores están en «", respaldo, "»."), call.=FALSE)
  }
  invisible(TRUE)
}


#************************************************************************
#PROMEDIOS AUTOMATICOS GOOGLE SHEETS
#El esquema ahora es A:P. F=Equipo, G=Alumno, N/O/P=calificaciones.
#Se usa el separador correcto de argumentos segun la region del archivo.

api_get <- function(ss,campos){
  req <- request_generate("sheets.spreadsheets.get",params=list(
    spreadsheetId=as.character(ss),fields=campos))
  res <- request_make(req)
  httr::stop_for_status(res)
  httr::content(res,as="parsed",type="application/json",simplifyVector=FALSE)
}

api_lote <- function(ss,solicitudes){
  if(length(solicitudes)==0) return(invisible(NULL))
  req <- request_generate("sheets.spreadsheets.batchUpdate",params=list(
    spreadsheetId=as.character(ss),requests=solicitudes))
  res <- request_make(req)
  gargle::response_process(res)
  invisible(TRUE)
}

locale_separador <- function(ss){
  x <- api_get(ss,"properties(locale)")
  region <- tolower(x$properties$locale %||% "en_US")
  # Paises que normalmente usan coma decimal: ; separa argumentos.
  con_punto_y_coma <- c("es_es","es_ar","es_co","es_cl","es_pe","es_uy",
    "es_ve","pt_br","pt_pt","fr_fr","de_de","it_it","nl_nl","ru_ru",
    "pl_pl","tr_tr","da_dk","fi_fi","sv_se","cs_cz")
  if(region %in% con_punto_y_coma) ";" else ","
}

formula_promedios <- function(hoja_cal,tipo,sep=","){
  src <- paste0("'",gsub("'","''",hoja_cal,fixed=TRUE),"'!A1:P")
  if(tipo=="equipo"){
    q <- paste0('select F, avg(N), avg(O), avg(P), count(N) ',
      'where F is not null group by F ',
      "label F 'Equipo', avg(N) 'Calificación RC', avg(O) 'Calificación RL', ",
      "avg(P) 'Promedio', count(N) 'Evaluaciones'")
  }else{
    q <- paste0('select F, G, avg(N), avg(O), avg(P), count(N) ',
      'where F is not null and G is not null group by F, G ',
      "label F 'Equipo', G 'Alumno', avg(N) 'Calificación RC', ",
      "avg(O) 'Calificación RL', avg(P) 'Promedio', count(N) 'Evaluaciones'")
  }
  paste0('=IFERROR(QUERY(',src,sep,'"',q,'"',sep,'1)',sep,'"Sin evaluaciones")')
}

celda_promedios <- function(ss, hoja){
  celda <- range_read_cells(ss, sheet=hoja, range="A1", cell_data="full")$cell
  if(length(celda) == 0L) return(list())
  celda[[1L]]
}

es_error_formula <- function(celda){
  is.null(celda$effectiveValue) || !is.null(celda$effectiveValue$errorValue)
}

instalar_promedios <- function(ss, hoja_prom, hoja_cal, tipo){
  if(!existe_hoja(ss, hoja_prom)) sheet_add(ss, hoja_prom)
  sep <- locale_separador(ss)
  separadores <- c(sep, if(sep == ",") ";" else ",")
  formulas <- vapply(separadores, function(separador){
    formula_promedios(hoja_cal, tipo, separador)
  }, character(1), USE.NAMES=FALSE)
  existente <- celda_promedios(ss, hoja_prom)
  if(!es_error_formula(existente) &&
     (existente$userEnteredValue$formulaValue %||% "") %in% formulas){
    return(invisible(TRUE))
  }

  # Solo se reemplaza el resumen derivado cuando su fórmula necesita reparación.
  range_clear(ss, range=paste0("'", hoja_prom, "'!A:Z"), reformat=FALSE)
  for(formula in formulas){
    datos <- data.frame(x=gs4_formula(formula))
    range_write(ss, data=datos, sheet=hoja_prom, range="A1",
                col_names=FALSE, reformat=FALSE)
    if(!es_error_formula(celda_promedios(ss, hoja_prom))) return(invisible(TRUE))
  }
  stop(paste("No se pudo corregir la fórmula de", hoja_prom,
             "Revise el idioma o la región del archivo de Google Sheets."))
}

#************************************************************************
#FORMATO GOOGLE: bandas alternadas, encabezado, promedio destacado
#Se aplica una sola vez si la hoja todavia no tiene colores alternos.

color_rgb <- function(r,g,b){list(red=r/255,green=g/255,blue=b/255)}

formatear_archivo <- function(ss,hojas_formato){
  info <- api_get(ss,"sheets(properties(title,sheetId),bandedRanges(bandedRangeId))")
  solicitudes <- list()
  for(item in hojas_formato){
    candidato <- Filter(function(z){
      identical(z$properties$title,item$hoja)
    },info$sheets %||% list())
    if(length(candidato)==0) next
    sh <- candidato[[1]]
    if(length(sh$bandedRanges %||% list())>0) next
    id <- as.integer(sh$properties$sheetId)
    n <- as.integer(item$columnas)
    rango <- list(sheetId=id,startRowIndex=0,startColumnIndex=0,endColumnIndex=n)

    solicitudes[[length(solicitudes)+1]] <- list(addBanding=list(bandedRange=list(
      range=rango,rowProperties=list(
        headerColor=color_rgb(38,67,95),
        firstBandColor=color_rgb(255,255,255),
        secondBandColor=color_rgb(236,244,250)))))

    solicitudes[[length(solicitudes)+1]] <- list(repeatCell=list(
      range=list(sheetId=id,startRowIndex=0,endRowIndex=1,
                 startColumnIndex=0,endColumnIndex=n),
      cell=list(userEnteredFormat=list(textFormat=list(
        foregroundColor=color_rgb(255,255,255),bold=TRUE))),
      fields="userEnteredFormat.textFormat"))

    solicitudes[[length(solicitudes)+1]] <- list(updateSheetProperties=list(
      properties=list(sheetId=id,gridProperties=list(frozenRowCount=1)),
      fields="gridProperties.frozenRowCount"))

    if(!is.null(item$promedio)){
      col <- as.integer(item$promedio)-1L
      solicitudes[[length(solicitudes)+1]] <- list(repeatCell=list(
        range=list(sheetId=id,startRowIndex=1,
                   startColumnIndex=col,endColumnIndex=col+1L),
        cell=list(userEnteredFormat=list(
          backgroundColor=color_rgb(211,242,221),
          textFormat=list(bold=TRUE,foregroundColor=color_rgb(30,80,51)))),
        fields="userEnteredFormat.backgroundColor,userEnteredFormat.textFormat"))
    }
  }
  api_lote(ss,solicitudes)
}

#************************************************************************
#ALUMNOS

normalizar_lista <- function(x){
  if(is.null(x) || nrow(x)==0) return(empty_lista)
  if(!all(c("Equipo","Alumno") %in% names(x))) return(empty_lista)
  if(!"Activo" %in% names(x)) x$Activo <- rep("SI",nrow(x))
  y <- data.frame(
    Equipo=trimws(as.character(x$Equipo)),
    Alumno=trimws(as.character(x$Alumno)),
    Activo=toupper(trimws(as.character(x$Activo))),
    stringsAsFactors=FALSE
  )
  y$Activo[is.na(y$Activo)|y$Activo==""] <- "SI"
  y <- y[!is.na(y$Equipo)&!is.na(y$Alumno)&y$Equipo!=""&y$Alumno!="",,
         drop=FALSE]
  unique(y)
}

leer_lista_archivo <- function(path,nombre){
  ext <- tolower(tools::file_ext(nombre))
  if(ext=="csv"){
    x <- read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,
                  fileEncoding="UTF-8-BOM")
  }else if(ext %in% c("xlsx","xls")){
    tabs <- readxl::excel_sheets(path)
    tab <- if("Lista_Alumnos" %in% tabs) "Lista_Alumnos" else tabs[1]
    x <- as.data.frame(readxl::read_excel(path,sheet=tab),check.names=FALSE)
  }else stop("Seleccione un archivo CSV, XLS o XLSX.")
  claves <- iconv(names(x),to="ASCII//TRANSLIT")
  claves <- gsub("[^a-z0-9]","",tolower(claves))
  e <- which(claves %in% c("equipo","team"))
  a <- which(claves %in% c("alumno","nombre","nombrealumno","estudiante"))
  ac <- which(claves %in% c("activo","active"))
  if(length(e)==0 || length(a)==0) stop("Faltan columnas Equipo y Alumno")
  out <- data.frame(Equipo=as.character(x[[e[1]]]),
                    Alumno=as.character(x[[a[1]]]),
                    stringsAsFactors=FALSE)
  out$Activo <- if(length(ac)>0) as.character(x[[ac[1]]]) else "SI"
  normalizar_lista(out)
}

#************************************************************************
#DATOS PARA GOOGLE

fila_corta <- function(tipo,id,fecha,profesor,materia,equipo,integrante,g){
  row <- data.frame(ID=id,`Fecha local`=fecha,Semestre=semestre,
                    Profesor=profesor,`Materia / grupo`=materia,Equipo=equipo,
                    check.names=FALSE,stringsAsFactors=FALSE)
  row[[if(tipo=="equipo") "Integrantes" else "Alumno"]] <- integrante
  for(i in seq_along(criterios)) row[[criterios[i]]] <- as.numeric(g[i])
  rc <- calificacion_corta(g)
  row[["Calificación RC"]] <- rc
  row[["Calificación RL"]] <- rc
  row[["Promedio"]] <- rc
  row[,campos_corta(tipo),drop=FALSE]
}

fila_larga <- function(corta,tipo){
  g <- as.numeric(corta[1,criterios,drop=TRUE])
  rl <- calificaciones_largas(g)
  row <- corta[,c(campos_comunes,
                   if(tipo=="equipo") "Integrantes" else "Alumno"),drop=FALSE]
  for(i in 1:5) row[[paste0("PC_",i)]] <- rl[i]
  for(i in 1:3) row[[paste0("UT_",i)]] <- rl[i+5]
  row[["Promedio PC"]] <- round(mean(rl[1:5]),2)
  row[["Promedio UT"]] <- round(mean(rl[6:8]),2)
  for(n in calificaciones) row[[n]] <- corta[[n]]
  row[,campos_larga(tipo),drop=FALSE]
}

promedios_R <- function(datos,tipo){
  if(nrow(datos)==0) return(data.frame())
  claves <- if(tipo=="equipo") "Equipo" else c("Equipo","Alumno")
  if(!all(c(claves,calificaciones) %in% names(datos))) return(data.frame())
  d <- datos[,c(claves,calificaciones),drop=FALSE]
  for(n in calificaciones) d[[n]] <- as.numeric(d[[n]])
  base <- aggregate(d[,calificaciones,drop=FALSE],by=d[,claves,drop=FALSE],
                    FUN=function(z) round(mean(z,na.rm=TRUE),2))
  conteo <- aggregate(rep(1,nrow(d)),by=d[,claves,drop=FALSE],FUN=length)
  names(conteo)[ncol(conteo)] <- "Evaluaciones"
  merge(base,conteo,by=claves,sort=FALSE)
}

#************************************************************************
#CREAR / MIGRAR HOJAS ACTUALES

preparar_hojas <- function(config){
  campos <- c("google_admin_sheet_id", "google_equipos_sheet_id", "google_alumnos_sheet_id")
  ids <- lapply(campos, function(campo) as.character(as_sheets_id(config[[campo]])))
  ids <- unlist(ids, use.names=FALSE)
  if(length(ids) != 3L || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)){
    stop("La configuración debe identificar tres libros diferentes: Administración, Equipos y Alumnos.")
  }
  google_admin <- as_sheets_id(ids[[1L]])
  google_equipos <- as_sheets_id(ids[[2L]])
  google_alumnos <- as_sheets_id(ids[[3L]])

  asegurar_hoja(google_admin, hojas$lista, empty_lista)
  asegurar_hoja(google_admin, "Rubrica Corta", rubrica)
  asegurar_hoja(google_admin, "Rubrica Larga", rubrica_larga)

  migrar_campos(google_equipos, hojas$equipos, empty_equipos)
  migrar_campos(google_equipos, hojas$largas, empty_larga_eq)
  migrar_campos(google_alumnos, hojas$alumnos, empty_alumnos)
  migrar_campos(google_alumnos, hojas$largas, empty_larga_al)
  #Repara formulas existentes: ya no usan columnas H/R de la version vieja.
  instalar_promedios(google_equipos,hojas$promedios,hojas$equipos,"equipo")
  instalar_promedios(google_alumnos,hojas$promedios,hojas$alumnos,"alumno")

  #Bandas de color. Se extienden automaticamente al agregar mas filas.
  tryCatch({
    formatear_archivo(google_equipos,list(
      list(hoja=hojas$equipos,columnas=ncol(empty_equipos),promedio=ncol(empty_equipos)),
      list(hoja=hojas$largas,columnas=ncol(empty_larga_eq),promedio=ncol(empty_larga_eq)),
      list(hoja=hojas$promedios,columnas=5,promedio=4)
    ))
    formatear_archivo(google_alumnos,list(
      list(hoja=hojas$alumnos,columnas=ncol(empty_alumnos),promedio=ncol(empty_alumnos)),
      list(hoja=hojas$largas,columnas=ncol(empty_larga_al),promedio=ncol(empty_larga_al)),
      list(hoja=hojas$promedios,columnas=6,promedio=5)
    ))

    #Administración: listas del semestre, rúbricas y acceso.
    #Se aplican bandas de color sin modificar las calificaciones ni las listas.
    hojas_admin <- sheet_names(google_admin)
    nombres_semestre <- grep(
      "^(Enero - Junio|Julio - Diciembre) [0-9]{4}$",
      hojas_admin,
      value=TRUE
    )
    formato_admin <- lapply(nombres_semestre,function(nombre){
      list(hoja=nombre,columnas=ncol(empty_lista))
    })
    formato_admin <- c(formato_admin,list(
      list(hoja="Rubrica Corta",columnas=ncol(rubrica)),
      list(hoja="Rubrica Larga",columnas=ncol(rubrica_larga))
    ))
    if("Acceso" %in% hojas_admin){
      formato_admin <- c(formato_admin,list(list(hoja="Acceso",columnas=1)))
    }
    formatear_archivo(google_admin,formato_admin)
  },error=function(e){
    warning(paste("No se pudo aplicar el formato visual en Google Sheets:",e$message))
  })
  invisible(TRUE)
}

url_google <- function(id){
  paste0("https://docs.google.com/spreadsheets/d/",id,"/edit")
}

# Configuración local de los libros del sistema de rúbricas.
correo_montufar <- "marco.montufar14@gmail.com"
libros_rubricas <- c(
  google_admin_sheet_id = "Administracion Rubricas UAEH",
  google_equipos_sheet_id = "Evaluaciones Rubricas UAEH - Equipos",
  google_alumnos_sheet_id = "Evaluaciones Rubricas UAEH - Alumnos"
)

texto_presente <- function(valor) {
  is.character(valor) && length(valor) == 1L && !is.na(valor) &&
    nzchar(trimws(valor))
}

ids_configuracion <- function(config) {
  vapply(names(libros_rubricas), function(campo) {
    valor <- config[[campo]]
    if (texto_presente(valor)) trimws(valor) else ""
  }, character(1))
}

leer_configuracion <- function(ruta) {
  config <- if (file.exists(ruta)) readRDS(ruta) else list()
  if (!is.list(config) || (!is.null(config$google_email) &&
      (!texto_presente(config$google_email) ||
       tolower(trimws(config$google_email)) != correo_montufar))) {
    stop("La configuración privada no corresponde a la cuenta de Montúfar.", call. = FALSE)
  }
  config$google_email <- correo_montufar
  config
}

es_rstudio_escritorio <- function() {
  interactive() && rstudioapi::isAvailable() &&
    identical(rstudioapi::versionInfo()$mode, "desktop")
}

guardar_configuracion <- function(valor, ruta) {
  temporal <- tempfile("configuracion-", tmpdir = dirname(ruta))
  on.exit(unlink(temporal), add = TRUE)
  saveRDS(valor, temporal)
  if (!file.rename(temporal, ruta)) {
    stop(paste("No se pudo guardar la configuración en", ruta), call. = FALSE)
  }
  invisible(valor)
}

autorizar_configuracion <- function(cache, drive = FALSE) {
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  use_oob <- !es_rstudio_escritorio() && gargle::gargle_oob_default()
  if (drive) {
    googledrive::drive_auth(email = correo_montufar, cache = cache, use_oob = use_oob)
    correo <- googledrive::drive_user()$emailAddress
  } else {
    googlesheets4::gs4_auth(email = correo_montufar, cache = cache, use_oob = use_oob)
    correo <- googlesheets4::gs4_user()
  }

  verificar_cuenta_google(correo)
}

verificar_cuenta_google <- function(correo) {
  if (!texto_presente(correo) || tolower(trimws(correo)) != correo_montufar) {
    stop(paste("Autorice la cuenta", correo_montufar, "para continuar."), call. = FALSE)
  }
}

tomar_bloqueo_configuracion <- function(privado) {
  bloqueo <- filelock::lock(file.path(privado, "configuracion.lock"), timeout = 0)
  if (is.null(bloqueo)) {
    stop("Otra sesión está configurando el sistema. Inténtelo cuando termine.", call. = FALSE)
  }
  bloqueo
}

obtener_hash <- function(ss) {
  if (!"Acceso" %in% googlesheets4::sheet_names(ss)) return("")
  acceso <- googlesheets4::read_sheet(ss, sheet = "Acceso", col_types = "c")
  if (!"Hash" %in% names(acceso) || !nrow(acceso) || !texto_presente(acceso$Hash[1])) {
    return("")
  }
  acceso$Hash[1]
}

pedir_password_admin <- function() {
  nueva <- rstudioapi::askForPassword("Contraseña de Administración (mínimo 12 caracteres)")
  if (is.null(nueva)) return(NULL)
  if (!texto_presente(nueva) || nchar(nueva) < 12L) {
    stop("La contraseña debe tener al menos 12 caracteres.", call. = FALSE)
  }
  repetir <- rstudioapi::askForPassword("Confirme la contraseña de Administración")
  if (is.null(repetir)) return(NULL)
  if (!identical(nueva, repetir)) {
    stop("Las contraseñas no coinciden.", call. = FALSE)
  }
  nueva
}

guardar_password_admin <- function(ss, nueva) {
  hash <- sodium::password_store(nueva)
  googlesheets4::sheet_write(data.frame(Hash = hash), ss = ss, sheet = "Acceso")
  if (!identical(obtener_hash(ss), hash)) {
    stop("Google Sheets no confirmó el cambio de contraseña.", call. = FALSE)
  }
  invisible(TRUE)
}

crear_acceso <- function(ss) {
  if (texto_presente(obtener_hash(ss))) return(invisible(FALSE))
  nueva <- pedir_password_admin()
  if (is.null(nueva)) stop("Se canceló la configuración de la contraseña.", call. = FALSE)
  guardar_password_admin(ss, nueva)
}

restablecer_password_admin <- function(privado, ss) {
  token_anterior <- googlesheets4::gs4_token()
  on.exit(googlesheets4::gs4_auth(token = token_anterior), add = TRUE)
  googlesheets4::gs4_auth(email = FALSE, cache = FALSE, use_oob = FALSE)
  verificar_cuenta_google(googlesheets4::gs4_user())
  nueva <- pedir_password_admin()
  if (is.null(nueva)) return(FALSE)

  bloqueo <- tomar_bloqueo_configuracion(privado)
  on.exit(filelock::unlock(bloqueo), add = TRUE)
  actual <- leer_configuracion(file.path(privado, "config.rds"))
  if (!identical(ids_configuracion(actual)[["google_admin_sheet_id"]], as.character(ss))) {
    stop("El libro de Administración cambió. Cierre la aplicación y pulse Run App.", call. = FALSE)
  }
  guardar_password_admin(ss, nueva)
}

crear_libro <- function(titulo) {
  as.character(googlesheets4::gs4_create(
    titulo, locale = "es_MX", timeZone = "America/Mexico_City",
    sheets = "Inicio"
  ))
}

id_de_enlace <- function(entrada) {
  entrada <- trimws(entrada)
  if (grepl("/spreadsheets/d/", entrada, fixed = TRUE)) {
    entrada <- sub("^.*?/spreadsheets/d/", "", entrada, perl = TRUE)
    entrada <- sub("[/?#].*$", "", entrada)
  }
  if (!grepl("^[a-zA-Z0-9_-]+$", entrada)) {
    stop("Introduzca un ID o un enlace válido de Google Sheets.", call. = FALSE)
  }
  entrada
}

mostrar_libros_activos <- function(config) {
  cat("\nLibros activos del sistema de rúbricas\n")
  for (campo in names(libros_rubricas)) {
    cat(libros_rubricas[[campo]], "\n  https://docs.google.com/spreadsheets/d/",
        config[[campo]], "/edit\n", sep = "")
  }
}

configurar_libros <- function(config, ruta, pendiente) {
  ids_activos <- ids_configuracion(config)
  if (file.exists(pendiente)) {
    config <- leer_configuracion(pendiente)
    vinculados <- nzchar(ids_activos)
    if (any(ids_configuracion(config)[vinculados] != ids_activos[vinculados])) {
      stop("Los IDs activos cambiaron desde que se inició la configuración.", call. = FALSE)
    }
  }
  ids <- ids_configuracion(config)
  faltan <- !nzchar(ids)
  if (any(faltan)) {
    vincular <- rstudioapi::showQuestion(
      "Configurar Google Sheets", "¿Desea vincular libros existentes a la aplicación?",
      ok = "Vincular libros", cancel = "Crear libros nuevos", timeout = 600
    )
    if (isTRUE(vincular)) {
      for (campo in names(libros_rubricas)[faltan]) {
        instruccion <- paste0("Pegue el enlace de ", libros_rubricas[[campo]], ".")
        entrada <- rstudioapi::showPrompt("Vincular libro de Google Sheets", instruccion,
                                         default = "", timeout = 600)
        if (is.null(entrada)) stop("Se canceló la vinculación.", call. = FALSE)
        config[[campo]] <- id_de_enlace(entrada)
      }
    }
  }

  ids <- ids_configuracion(config)
  usados <- ids[nzchar(ids)]
  if (anyDuplicated(usados)) {
    stop("Cada función del sistema requiere un libro distinto de Google Sheets.", call. = FALSE)
  }
  for (campo in names(usados)) googlesheets4::gs4_get(usados[[campo]])

  for (campo in names(ids)[!nzchar(ids)]) {
    titulo <- libros_rubricas[[campo]]
    message("Creando ", titulo)
    config[[campo]] <- crear_libro(titulo)
    guardar_configuracion(config, pendiente)
  }
  if (!identical(ids_configuracion(config), ids_activos)) {
    guardar_configuracion(config, pendiente)
  }
  crear_acceso(config$google_admin_sheet_id)
  preparar_hojas(config)
  guardar_configuracion(config, ruta)
  unlink(pendiente)
  mostrar_libros_activos(config)
  invisible(config)
}

buscar_libros_drive <- function(titulos) {
  if (!length(titulos)) stop("Indique los títulos de los libros de rúbricas.", call. = FALSE)
  condicion <- paste0("(name = '", paste(titulos, collapse = "' or name = '"), "')")
  encontrados <- googledrive::drive_find(type = "spreadsheet", q = c("'me' in owners", condicion))
  encontrados[encontrados$name %in% titulos, , drop = FALSE]
}

consultar_libro_drive <- function(id) {
  tryCatch(
    googledrive::drive_get(id = id),
    http_error_404 = function(e) NULL
  )
}

libro_eliminable <- function(libro) {
  recurso <- libro$drive_resource[[1]]
  identical(recurso$mimeType, "application/vnd.google-apps.spreadsheet") &&
    isTRUE(recurso$ownedByMe) && isTRUE(recurso$capabilities$canTrash)
}

cancelar_reinicio <- function() {
  stop(errorCondition("Se canceló el reinicio.", class = "reinicio_cancelado"))
}

elegir_libros_anteriores <- function(ids_config) {
  encontrados <- buscar_libros_drive(unname(libros_rubricas))
  ids <- as.character(encontrados$id)
  candidatos <- setNames(as.character(encontrados$name), ids)
  ids_config <- unique(unname(ids_config[nzchar(ids_config)]))

  for (id in setdiff(ids_config, ids)) {
    libro <- consultar_libro_drive(id)
    if (is.null(libro)) {
      warning(paste("El ID configurado", id, "ya no es visible en Google Drive."),
              call. = FALSE, immediate. = TRUE)
      next
    }
    if (isTRUE(libro$drive_resource[[1]]$trashed)) next
    if (!libro_eliminable(libro)) {
      stop(paste("El archivo configurado", libro$name[[1]], "(", id,
                 ") debe ser un libro propio con permiso para enviarlo a la papelera."),
           call. = FALSE)
    }
    candidatos[id] <- libro$name[[1]]
  }

  if (!length(candidatos)) return(character())
  mostrar <- function(libros) {
    ids <- names(libros)
    activos <- ifelse(ids %in% ids_config, " (activo en la configuración)", "")
    paste0(unname(libros), " [", ids, "]", activos, collapse = "\n")
  }
  opcionales <- setdiff(names(candidatos), ids_config)
  if (length(opcionales)) {
    instruccion <- paste(
      "Si un libro no activo es ajeno al sistema, introduzca su ID para excluirlo.",
      "Separe varios IDs con espacios."
    )
    excluir <- rstudioapi::showPrompt("Revisar libros detectados",
      paste(paste(instruccion, "Deje el guion para incluir todos."),
            mostrar(candidatos), sep = "\n\n"),
      default = "-", timeout = 600)
    if (is.null(excluir)) cancelar_reinicio()
    excluir <- trimws(excluir)
    if (nzchar(excluir) && excluir != "-") {
      excluir <- strsplit(excluir, "[[:space:],]+")[[1]]
      if (!all(excluir %in% opcionales)) {
        stop("Algún ID indicado no corresponde a los libros opcionales de la lista.", call. = FALSE)
      }
      candidatos <- candidatos[!names(candidatos) %in% excluir]
    }
  }
  seleccionados <- names(candidatos)
  for (id in intersect(seleccionados, ids)) {
    if (!libro_eliminable(encontrados[ids == id, , drop = FALSE])) {
      stop(paste("Google Drive no permite enviar a la papelera el libro", id), call. = FALSE)
    }
  }
  if (length(seleccionados)) {
    aviso <- "Los nuevos libros iniciarán sin alumnos ni calificaciones. Los siguientes libros pasarán a la papelera de Google Drive."
    confirmado <- rstudioapi::showQuestion(
      "Confirmar el reinicio de Google Sheets", paste(aviso, mostrar(candidatos), sep = "\n\n"),
      ok = "Rehacer libros", cancel = "Cancelar", timeout = 600
    )
    if (!isTRUE(confirmado)) cancelar_reinicio()
  }
  seleccionados
}

libros_nuevos <- function(estado, pendiente) {
  config <- estado$nuevo
  for (campo in names(libros_rubricas)) {
    if (texto_presente(config[[campo]])) {
      googlesheets4::gs4_get(config[[campo]])
      next
    }

    temporal <- paste(libros_rubricas[[campo]], "reinicio", estado$marca, sep = " - ")
    existentes <- buscar_libros_drive(temporal)
    if (nrow(existentes) > 1L) {
      stop(paste("Se encontró más de un libro temporal con el título", temporal), call. = FALSE)
    }
    id <- if (nrow(existentes)) as.character(existentes$id[[1]]) else crear_libro(temporal)
    config[[campo]] <- id
    estado$nuevo <- config
    guardar_configuracion(estado, pendiente)
  }
  estado
}

renombrar_libros_nuevos <- function(config) {
  for (campo in names(libros_rubricas)) {
    libro <- consultar_libro_drive(config[[campo]])
    if (is.null(libro) || !libro_eliminable(libro) ||
        isTRUE(libro$drive_resource[[1]]$trashed)) {
      stop(paste("No se pudo validar el libro nuevo de", libros_rubricas[[campo]]), call. = FALSE)
    }
    titulo <- libros_rubricas[[campo]]
    if (!identical(libro$name[[1]], titulo)) {
      googledrive::drive_rename(libro, titulo, overwrite = NA)
    }
  }
}

enviar_a_papelera <- function(id) {
  libro <- consultar_libro_drive(id)
  if (is.null(libro)) {
    stop(paste("El libro anterior ya no es visible en Google Drive:", id), call. = FALSE)
  }
  if (isTRUE(libro$drive_resource[[1]]$trashed)) return(invisible(FALSE))
  if (!libro_eliminable(libro)) {
    stop(paste("Google Drive ya no permite enviar a la papelera el libro", id), call. = FALSE)
  }
  actualizado <- googledrive::drive_trash(libro)
  if (nrow(actualizado) != 1L || !isTRUE(actualizado$drive_resource[[1]]$trashed)) {
    stop(paste("Google Drive no confirmó el traslado a la papelera del libro", id), call. = FALSE)
  }
  invisible(TRUE)
}

reiniciar_libros <- function(config, ruta, pendiente, configuracion_pendiente) {
  if (file.exists(pendiente)) {
    message("Reanudando el reinicio pendiente.")
    estado <- readRDS(pendiente)
    ids_activos <- ids_configuracion(config)
    if (!identical(ids_activos, estado$activos) &&
        !identical(ids_activos, ids_configuracion(estado$nuevo))) {
      stop("Los IDs activos cambiaron desde que se inició el reinicio.", call. = FALSE)
    }
  } else {
    # Una configuración inicial interrumpida también puede tener libros ya vinculados.
    ids_activos <- ids_configuracion(config)
    ids_previos <- ids_activos
    if (file.exists(configuracion_pendiente)) {
      ids_previos <- c(ids_previos, ids_configuracion(leer_configuracion(configuracion_pendiente)))
    }
    anteriores <- elegir_libros_anteriores(ids_previos)
    marca <- paste(format(Sys.time(), "%Y%m%d%H%M%S"), Sys.getpid(), sep = "-")
    estado <- list(
      activos = ids_activos, anteriores = anteriores, marca = marca,
      nuevo = list(google_email = correo_montufar), listo = FALSE
    )
    guardar_configuracion(estado, pendiente)
  }

  if (!isTRUE(estado$listo)) {
    estado <- libros_nuevos(estado, pendiente)
    crear_acceso(estado$nuevo$google_admin_sheet_id)
    preparar_hojas(estado$nuevo)
  }
  renombrar_libros_nuevos(estado$nuevo)
  if (!isTRUE(estado$listo)) {
    estado$listo <- TRUE
    guardar_configuracion(estado, pendiente)
  }

  ids_nuevos <- ids_configuracion(estado$nuevo)
  if (any(estado$anteriores %in% ids_nuevos)) {
    stop("Un libro nuevo aparece entre los libros anteriores.", call. = FALSE)
  }
  if (!identical(ids_configuracion(config), ids_nuevos)) {
    guardar_configuracion(estado$nuevo, ruta)
  }
  for (id in estado$anteriores) {
    enviar_a_papelera(id)
    estado$anteriores <- setdiff(estado$anteriores, id)
    guardar_configuracion(estado, pendiente)
  }

  if (unlink(configuracion_pendiente) != 0L || unlink(pendiente) != 0L) {
    stop("No se pudo retirar el registro local del reinicio. Vuelva a ejecutar el reinicio.",
         call. = FALSE)
  }
  mostrar_libros_activos(estado$nuevo)
  invisible(estado$nuevo)
}

configurar_sistema_google <- function(reiniciar = FALSE) {
  if (!es_rstudio_escritorio()) stop("Abra app.R en RStudio Desktop y pulse Run App.", call. = FALSE)
  privado <- file.path(getwd(), "private")
  dir.create(privado, recursive = TRUE, showWarnings = FALSE)
  ruta <- file.path(privado, "config.rds")
  configuracion_pendiente <- file.path(privado, "configuracion-pendiente.rds")
  reinicio_pendiente <- file.path(privado, "reinicio-pendiente.rds")
  bloqueo <- tomar_bloqueo_configuracion(privado)
  on.exit(filelock::unlock(bloqueo), add = TRUE)

  if (!reiniciar && file.exists(reinicio_pendiente)) {
    stop("Abra app.R y pulse Run App para reanudar el reinicio pendiente.", call. = FALSE)
  }
  config <- leer_configuracion(ruta)
  if (reiniciar) {
    autorizar_configuracion(file.path(privado, "google-drive-token"), drive = TRUE)
    autorizar_configuracion(file.path(privado, "google-token"))
    reiniciar_libros(config, ruta, reinicio_pendiente, configuracion_pendiente)
  } else {
    autorizar_configuracion(file.path(privado, "google-token"))
    configurar_libros(config, ruta, configuracion_pendiente)
  }
}

preparar_arranque_rstudio <- function(privado, config) {
  marca <- file.path(privado, "arranque-rstudio.rds")
  hay_reinicio <- file.exists(file.path(privado, "reinicio-pendiente.rds"))
  ids <- ids_configuracion(config)
  completo <- all(nzchar(ids)) && !anyDuplicated(ids)

  if (hay_reinicio) {
    rehacer <- rstudioapi::showQuestion(
      "Reinicio pendiente", "Hay un reinicio de Google Sheets pendiente. ¿Desea reanudarlo?",
      ok = "Reanudar reinicio", cancel = "Ahora no", timeout = 600
    )
    if (!isTRUE(rehacer)) cancelar_reinicio()
  } else if (!file.exists(marca)) {
    alternativa <- if (completo) "Abra los libros configurados." else "Configure los libros para esta aplicación."
    rehacer <- rstudioapi::showQuestion(
      "Google Sheets del profesor Montúfar",
      paste0("Cuenta de Google\n", correo_montufar, "\n\n",
             "Si desea reemplazar los libros anteriores por tres libros vacíos, elija Rehacer libros. ",
             alternativa),
      ok = "Rehacer libros", cancel = if (completo) "Abrir libros actuales" else "Configurar", timeout = 600
    )
  } else {
    rehacer <- FALSE
  }

  if (isTRUE(rehacer) || !completo) {
    instalar_paquetes(configuracion = TRUE)
    config <- configurar_sistema_google(reiniciar = isTRUE(rehacer))
  }
  if (!file.exists(marca)) guardar_configuracion(TRUE, marca)
  config
}

private_dir <- file.path(getwd(), "private")
config_path <- file.path(private_dir, "config.rds")
cache_path <- file.path(private_dir, "google-token")

en_rstudio <- es_rstudio_escritorio()
iniciar_google <- function() {
  config <- leer_configuracion(config_path)
  if (en_rstudio) config <- preparar_arranque_rstudio(private_dir, config)
  ids <- ids_configuracion(config)
  if (any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Abra SISTEMA-RUBRICA.Rproj en RStudio Desktop y pulse Run App en app.R para configurar Google.",
         call. = FALSE)
  }
  autorizar_configuracion(cache_path)
  preparar_hojas(config)
  config
}

config <- tryCatch(iniciar_google(), error = function(e) {
  if (en_rstudio) rstudioapi::showDialog("Sistema de Rúbricas", conditionMessage(e))
  stop(e)
})
google_admin <- as_sheets_id(config$google_admin_sheet_id)
google_equipos <- as_sheets_id(config$google_equipos_sheet_id)
google_alumnos <- as_sheets_id(config$google_alumnos_sheet_id)

#************************************************************************
#HORA LOCAL DEL DISPOSITIVO
#La fecha se genera en el navegador justo al presionar Enviar.

script_hora <- HTML(paste0(
  'function enviarCalificacion(){',
  '  var d = new Date();',
  '  var p = function(x){return String(x).padStart(2,"0");};',
  '  var fecha = d.getFullYear()+"-"+p(d.getMonth()+1)+"-"+p(d.getDate())',
  '    +" "+p(d.getHours())+":"+p(d.getMinutes())+":"+p(d.getSeconds());',
  '  Shiny.setInputValue("guardar", {fecha:fecha,evento:d.getTime()},',
  '    {priority:"event"});',
  '}'
))

#************************************************************************
#UI

#************************************************************************
#PASSWORD - MOSTRAR / OCULTAR
#
#El ojo funciona en el login y al cambiar la contrasena.
#La contrasena sigue siendo de tipo password por defecto.
#************************************************************************

password_con_ojo <- function(id,texto){
  tags$div(class="password-eye-container",
    tags$label(`for`=id,texto),
    tags$div(class="password-eye-row",
      passwordInput(id,label=NULL,width="100%"),
      tags$button(type="button",class="password-eye-toggle",
                  title="Mostrar contraseña",`aria-label`="Mostrar contraseña",
                  `aria-pressed`="false",icon("eye"))
    )
  )
}

estilo_password <- tags$head(
  tags$style(HTML("
    .password-eye-container { max-width: 440px; margin-bottom: 15px; }
    .password-eye-container > label { display:block; margin-bottom: 6px; }
    .password-eye-row { position:relative; width:100%; }
    .password-eye-row .shiny-input-container { width:100% !important; margin-bottom:0; }
    .password-eye-row .form-group { margin-bottom:0; }
    .password-eye-row input { padding-right:45px; height:36px; }
    .password-eye-toggle { position:absolute; right:4px; top:50%;
      transform:translateY(-50%); height:30px; width:39px; z-index:3;
      border:0; border-radius:4px; background:transparent; color:#566573;
      display:flex; align-items:center; justify-content:center; cursor:pointer; }
    .password-eye-toggle:hover, .password-eye-toggle:focus-visible {
      background:#eef4f7; color:#367fa9; outline:1px solid #367fa9; }
  ")),
  tags$script(HTML("
    $(document).on('click', '.password-eye-toggle', function(e){
      e.preventDefault();
      var b=$(this); var f=b.closest('.password-eye-row').find('input').first();
      if(!f.length) return;
      var ver=f.attr('type')==='password';
      f.attr('type',ver?'text':'password');
      b.find('i').removeClass('fa-eye fa-eye-slash')
        .addClass(ver?'fa-eye-slash':'fa-eye');
      b.attr('title',ver?'Ocultar contraseña':'Mostrar contraseña');
      b.attr('aria-label',ver?'Ocultar contraseña':'Mostrar contraseña');
      b.attr('aria-pressed',String(ver));
    });
  "))
)

ui <- dashboardPage(
  skin="blue",
  dashboardHeader(title="Evaluación de Rúbricas"),
  dashboardSidebar(
    sidebarMenu(id="menu",
      menuItem("Evaluar equipo",tabName="evaluacion",icon=icon("users")),
      menuItem("Resultados",tabName="resultados",icon=icon("bar-chart")),
      menuItem("Administración",tabName="admin",icon=icon("lock"))
    )
  ),
  dashboardBody(
    estilo_password,
    tags$head(tags$script(script_hora)),
    tabItems(
      tabItem(tabName="evaluacion",
        fluidRow(
          box(title=paste("Evaluación -",semestre),width=4,status="primary",
              solidHeader=TRUE,
              textInput("profesor","Profesor evaluador:"),
              textInput("materia","Materia / grupo:"),
              uiOutput("equipo_ui"),
              uiOutput("integrantes_ui"),
              tags$p("Los integrantes seleccionados recibirán la misma calificación."),
              tags$button(type="button",class="btn btn-success",
                          onclick="enviarCalificacion();",
                          icon("paper-plane")," Enviar calificaciones")
          ),
          box(title="Rúbrica",width=8,status="warning",solidHeader=TRUE,
              tags$p("Escala de calificación: 0 a 10."),
              uiOutput("inputs_grado"))
        ),
        fluidRow(box(title="Calificación",width=12,status="info",solidHeader=TRUE,
                     tableOutput("preview"),h3(textOutput("preview_total"))))
      ),
      tabItem(tabName="resultados",
        fluidRow(
          box(title="Resultados",width=4,status="primary",solidHeader=TRUE,
              tags$p("Los resultados se sincronizan con Google cada 45 segundos mientras esta pestaña permanezca abierta."),
              actionButton("refrescar","Actualizar ahora",icon=icon("refresh")),
              selectInput("tipo_resultado","Mostrar:",
                          choices=c("Equipos"="equipo","Alumnos"="alumno")),
              h4("Promedio general"),textOutput("promedio_general")),
          box(title="Gráfica de promedios",width=8,status="info",solidHeader=TRUE,
              plotOutput("grafica",height="390px"))
        ),
        fluidRow(box(title="Promedios de los profesores evaluadores",width=12,status="success",
                     solidHeader=TRUE,tableOutput("tabla_promedios")))
      ),
      tabItem(tabName="admin",
        fluidRow(box(title="Administración",width=12,status="danger",solidHeader=TRUE,
                     uiOutput("admin_ui")))
      )
    )
  )
)

#************************************************************************
#SERVER

server <- function(input,output,session){
  loopback <- c("127.0.0.1", "::1", "::ffff:127.0.0.1")
  control_local <- en_rstudio && isTRUE(session$request$SERVER_NAME %in% loopback) &&
    isTRUE(session$request$REMOTE_ADDR %in% loopback)

  lista_inicio <- normalizar_lista(leer_hoja(google_admin,hojas$lista,empty_lista))
  eq_inicio <- leer_hoja(google_equipos,hojas$equipos,empty_equipos)
  al_inicio <- leer_hoja(google_alumnos,hojas$alumnos,empty_alumnos)

  lista_admin <- reactiveVal(lista_inicio)
  datos_equipos <- reactiveVal(eq_inicio)
  datos_alumnos <- reactiveVal(al_inicio)
  admin_ok <- reactiveVal(FALSE)
  semestres_version <- reactiveVal(0L)
  respaldo_generado <- reactiveVal("")

  semestres_disponibles <- reactive({
    semestres_version()
    regs <- grep("^(Enero - Junio|Julio - Diciembre) [0-9]{4}$",
                 sheet_names(google_admin),value=TRUE)
    unique(c(semestre,regs))
  })

  observeEvent(input$semestre_borrar,{
    respaldo_generado("")
    updateCheckboxInput(session,"respaldo_descargado",value=FALSE)
  })

  alumnos_activos <- reactive({
    d <- lista_admin()
    d[d$Activo=="SI",,drop=FALSE]
  })

  refrescar <- function(){
    lista_admin(normalizar_lista(leer_hoja(google_admin,hojas$lista,empty_lista)))
    datos_equipos(leer_hoja(google_equipos,hojas$equipos,empty_equipos))
    datos_alumnos(leer_hoja(google_alumnos,hojas$alumnos,empty_alumnos))
  }

  #Actualizacion periodica entre profesores/sesiones.
  observe({
    if(identical(input$menu,"resultados")){
      invalidateLater(45000,session)
      tryCatch(refrescar(),error=function(e){
        showNotification(paste("No se pudo sincronizar:",e$message),
                         type="warning",duration=7)
      })
    }
  })

  observeEvent(input$refrescar,{
    tryCatch({
      refrescar()
      showNotification("Sincronizado con Google.",type="message")
    },error=function(e){showNotification(e$message,type="error")})
  })

  output$equipo_ui <- renderUI({
    x <- alumnos_activos()
    if(nrow(x)==0) return(tags$p("No hay alumnos activos. Solicite al administrador que cargue la lista."))
    equipos <- unique(x$Equipo)
    selectInput("equipo","Equipo:",choices=equipos)
  })

  output$integrantes_ui <- renderUI({
    req(input$equipo)
    x <- alumnos_activos()
    integrantes <- x$Alumno[x$Equipo==input$equipo]
    checkboxGroupInput("integrantes","Integrantes:",choices=integrantes,
                       selected=integrantes)
  })

  output$inputs_grado <- renderUI({
    tagList(lapply(seq_len(nrow(rubrica)),function(i){
      tagList(
        h4(paste0(rubrica$Aspecto[i]," (",rubrica$Peso[i],"%)")),
        tags$p(rubrica$Descripcion[i]),
        numericInput(paste0("aspecto",i),"Calificación:",value=10,
                     min=0,max=10,step=0.5),hr()
      )
    }))
  })

  grado_actual <- reactive({
    sapply(seq_len(nrow(rubrica)),function(i){
      x <- input[[paste0("aspecto",i)]]
      if(is.null(x)) 10 else as.numeric(x)
    })
  })

  output$preview <- renderTable({
    g <- grado_actual()
    data.frame(Aspecto=rubrica$Aspecto,
               Porcentaje=paste0(rubrica$Peso,"%"),
               Calificación=g,Aporte=round(g*rubrica$Peso/100,2),
               check.names=FALSE)
  },digits=2)

  output$preview_total <- renderText({
    g <- grado_actual()
    if(any(is.na(g)) || any(g<0|g>10)) return("Calificaciones inválidas")
    paste0("Calificación final: ",sprintf("%.2f",calificacion_corta(g))," / 10")
  })

  #************************************************************************
  #GUARDAR EVALUACION

  observeEvent(input$guardar,{
    evento <- input$guardar
    profesor <- trimws(input$profesor %||% "")
    materia <- trimws(input$materia %||% "")
    equipo <- trimws(input$equipo %||% "")
    miembros <- input$integrantes
    g <- grado_actual()

    if(!nzchar(profesor) || !nzchar(materia) || !nzchar(equipo) ||
       length(miembros)==0 || nrow(alumnos_activos())==0){
      showNotification("Complete los campos de profesor, materia, equipo e integrantes.",type="error")
      return()
    }
    if(any(is.na(g)) || any(g<0|g>10)){
      showNotification("Todas las calificaciones deben estar entre 0 y 10.",type="error")
      return()
    }

    #Fecha y zona reportadas por el navegador al presionar Enviar.
    fecha <- as.character(evento$fecha %||% "")
    if(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$",fecha)){
      showNotification("No se recibió la hora local del dispositivo.",type="error")
      return()
    }
    id <- paste0(format(Sys.time(),"%Y%m%d%H%M%OS6",tz="UTC"),"_",
                 paste(sample(0:9,5,replace=TRUE),collapse=""))

    eq <- fila_corta("equipo",id,fecha,profesor,materia,
                      equipo,paste(miembros,collapse=", "),g)
    alumnos <- do.call(rbind,lapply(miembros,function(al){
      fila_corta("alumno",id,fecha,profesor,materia,equipo,al,g)
    }))
    larga_eq <- fila_larga(eq,"equipo")
    larga_al <- do.call(rbind,lapply(seq_len(nrow(alumnos)),function(i){
      fila_larga(alumnos[i,,drop=FALSE],"alumno")
    }))

    #Las cuatro escrituras no constituyen una transaccion.
    #El ID permite diagnosticar/recuperar un envio parcial en Google.
    ok <- tryCatch({
      sheet_append(google_equipos,eq,sheet=hojas$equipos)
      sheet_append(google_alumnos,alumnos,sheet=hojas$alumnos)
      sheet_append(google_equipos,larga_eq,sheet=hojas$largas)
      sheet_append(google_alumnos,larga_al,sheet=hojas$largas)
      TRUE
    },error=function(e){
      showNotification(paste("Error de Google (ID",id,"):",e$message,
                             "Verifique los registros antes de volver a enviar la evaluación."),
                       type="error",duration=15)
      FALSE
    })
    if(!ok) return()

    #No sobrescribimos resúmenes: QUERY se actualiza solo en Google.
    tryCatch(refrescar(),error=function(e){
      showNotification("Guardado en Google, pero falló la lectura posterior.",
                       type="warning")
    })
    showNotification(paste("Guardado. Calificación:",sprintf("%.2f",calificacion_corta(g))),
                     type="message",duration=7)
  })

  #************************************************************************
  #RESULTADOS

  resultado_actual <- reactive({
    if(identical(input$tipo_resultado,"alumno")){
      promedios_R(datos_alumnos(),"alumno")
    }else{
      promedios_R(datos_equipos(),"equipo")
    }
  })

  output$tabla_promedios <- renderTable({resultado_actual()},digits=2)

  output$promedio_general <- renderText({
    d <- resultado_actual()
    if(nrow(d)==0) return("Sin calificaciones")
    sprintf("%.2f / 10",mean(d[["Promedio"]],na.rm=TRUE))
  })

  output$grafica <- renderPlot({
    d <- resultado_actual()
    if(nrow(d)==0){
      plot.new(); text(.5,.5,"Sin calificaciones")
      return()
    }
    etiquetas <- if("Alumno" %in% names(d)) d$Alumno else d$Equipo
    barplot(d[["Promedio"]],names.arg=etiquetas,las=2,
            ylim=c(0,10),ylab="Promedio",main="Promedios por evaluación")
  })

  #************************************************************************
  #ADMIN CON UNA CONTRASEÑA

  comprobar_password <- function(pass){
    if(is.null(pass) || !nzchar(pass)) return(FALSE)
    acceso <- leer_hoja(google_admin,"Acceso",
                        data.frame(Hash=character(),stringsAsFactors=FALSE))
    if(nrow(acceso)==0 || !"Hash" %in% names(acceso)) return(FALSE)
    tryCatch(sodium::password_verify(as.character(acceso$Hash[1]),pass),
             error=function(e) FALSE)
  }

  output$admin_ui <- renderUI({
    if(!admin_ok()){
      return(tagList(
        h4("Acceso administrativo"),
        password_con_ojo("admin_password","Contraseña:"),
        actionButton("entrar_admin","Entrar",icon=icon("sign-in")),
        if (control_local) actionButton("restablecer_password", "Restablecer contraseña")
      ))
    }
    lista <- lista_admin()
    opciones <- if(nrow(lista)>0){
      setNames(as.character(seq_len(nrow(lista))),
               paste(lista$Equipo,lista$Alumno,paste0("[",lista$Activo,"]")))
    }else character()

    disponibles <- semestres_disponibles()
    if(!semestre %in% disponibles) disponibles <- c(semestre,disponibles)

    tagList(
      actionButton("cerrar_admin","Cerrar Administración",icon=icon("sign-out")),
      tabsetPanel(
        tabPanel("Alumnos",br(),
          fileInput("archivo_lista","Seleccione un archivo Excel o CSV",accept=c(".csv",".xls",".xlsx")),
          actionButton("cargar_lista","Reemplazar lista actual",class="btn-success"),hr(),
          textInput("nuevo_equipo","Equipo:"),
          textInput("nuevo_alumno","Alumno:"),
          actionButton("agregar_alumno","Agregar alumno"),hr(),
          selectInput("alumno_admin","Alumno:",choices=opciones),
          actionButton("activar","Activar"),
          actionButton("desactivar","Desactivar"),hr(),
          tableOutput("tabla_lista")),
        tabPanel("Semestre",br(),
          tags$p("Puede respaldar y eliminar registros de cualquier semestre, incluido el actual."),
          tags$p(strong("Advertencia:"),
                 " Eliminar el semestre actual borrará su lista y evaluaciones, y lo reiniciará vacío."),
          selectInput("semestre_borrar","Seleccione el semestre:",choices=disponibles),
          downloadButton("descargar_respaldo","Descargar respaldo ZIP"),
          checkboxInput("respaldo_descargado","Confirmo que he descargado y guardado el respaldo."),
          actionButton("pedir_borrado","Eliminar semestre seleccionado",class="btn-danger")),
        tabPanel("Google",br(),
          tags$div(style="margin-bottom:20px;",
            h4("Administración"),
            tags$p("Listas de alumnos, referencias de rúbricas y contraseña administrativa."),
            tags$a("Abrir Google Sheets de Administración",href=url_google(config$google_admin_sheet_id),
                   target="_blank",class="btn btn-primary")),
          tags$div(style="margin-bottom:20px;",
            h4("Equipos"),
            tags$p("Calificaciones y promedios de cada equipo, incluyendo la rúbrica larga."),
            tags$a("Abrir Google Sheets de Equipos",href=url_google(config$google_equipos_sheet_id),
                   target="_blank",class="btn btn-primary")),
          tags$div(style="margin-bottom:20px;",
            h4("Alumnos"),
            tags$p("Calificaciones individuales y promedios acumulados de todos los profesores."),
            tags$a("Abrir Google Sheets de Alumnos",href=url_google(config$google_alumnos_sheet_id),
                   target="_blank",class="btn btn-primary")),
          if (control_local) actionButton("rehacer_libros", "Reiniciar libros", class="btn-danger")),
        tabPanel("Rúbricas",br(),
          tableOutput("tabla_comparativa")),
        tabPanel("Contraseña",br(),
          tags$p("Puede cambiar su contraseña mientras tenga acceso a Administración."),
          password_con_ojo("password_nueva","Nueva contraseña (12 caracteres mínimo):"),
          password_con_ojo("password_confirmar","Confirmar nueva contraseña:"),
          actionButton("cambiar_password","Guardar nueva contraseña"))
      )
    )
  })


  observeEvent(input$entrar_admin,{
    if(comprobar_password(input$admin_password)){
      admin_ok(TRUE)
      showNotification("Acceso concedido.",type="message")
    }else{
      showNotification("Contraseña incorrecta.",type="error")
    }
  })
  observeEvent(input$cerrar_admin,{admin_ok(FALSE)})

  observeEvent(input$restablecer_password, {
    req(control_local, !admin_ok())
    if (!isTRUE(rstudioapi::showQuestion(
      "Restablecer contraseña",
      paste("Autorice en el navegador la cuenta", correo_montufar,
            "para cambiar la contraseña de Administración."),
      ok = "Autorizar Google", cancel = "Cancelar", timeout = 600
    ))) return()

    tryCatch({
      instalar_paquetes(configuracion = TRUE)
      if (isTRUE(restablecer_password_admin(private_dir, google_admin))) {
        updateTextInput(session, "admin_password", value = "")
        showNotification("Contraseña restablecida. Entre con la nueva contraseña.", type = "message")
      }
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = 15))
  })

  observeEvent(input$rehacer_libros, {
    req(admin_ok(), control_local)
    if (!isTRUE(rstudioapi::showQuestion(
      "Reiniciar Google Sheets",
      "Se crearán tres libros vacíos. Puede revisar qué libros se enviarán a la papelera antes de confirmar.",
      ok = "Continuar", cancel = "Cancelar", timeout = 600
    ))) return()

    nuevo <- tryCatch({
      instalar_paquetes(configuracion = TRUE)
      configurar_sistema_google(reiniciar = TRUE)
    }, reinicio_cancelado = function(e) NULL, error = function(e) {
      showNotification(conditionMessage(e), type="error", duration=15)
      # Si ya se activó la nueva configuración, la sesión debe liberar los IDs anteriores.
      actual <- leer_configuracion(config_path)
      if (!identical(ids_configuracion(actual), ids_configuracion(config))) {
        rstudioapi::showDialog("Reinicio pendiente",
          "Los nuevos libros están vinculados. Pulse Run App para completar el reinicio.")
        stopApp()
      }
      NULL
    })
    if (is.null(nuevo)) return()
    rstudioapi::showDialog("Libros reemplazados",
      "Los libros nuevos están listos. Pulse Run App para abrir la aplicación con estos libros.")
    stopApp()
  })

  output$tabla_lista <- renderTable({req(admin_ok());lista_admin()})
  output$tabla_comparativa <- renderTable({
    req(admin_ok())

    relacion_corta <- vapply(seq_len(nrow(rubrica_larga)),function(i){
      fuente <- rubrica_larga$Fuente_RC[i]
      coincidencias <- regmatches(fuente,gregexpr("RC[1-6]",fuente))[[1]]
      if(length(coincidencias)==0) return("Sin correspondencia")

      relaciones <- vapply(coincidencias,function(codigo){
        numero <- as.integer(sub("RC","",codigo,fixed=TRUE))
        etiqueta <- paste0(codigo, ". ", rubrica$Aspecto[numero])
        if(i==1 && numero==5){
          etiqueta <- paste0(etiqueta," (relación indirecta)")
        }
        etiqueta
      },character(1))

      paste(relaciones,collapse="; ")
    },character(1))

    data.frame(
      `PC/UT`=rubrica_larga$Rubrica,
      `Rúbrica larga`=paste0("RL",rubrica_larga$RL,". ",rubrica_larga$Aspecto),
      `Relación con rúbrica corta`=relacion_corta,
      check.names=FALSE,
      stringsAsFactors=FALSE
    )
  },striped=TRUE,hover=TRUE,spacing="s")

  guardar_lista <- function(lista){
    sheet_write(lista,ss=google_admin,sheet=hojas$lista)
    lista_admin(lista)
  }

  observeEvent(input$cargar_lista,{
    req(admin_ok(),input$archivo_lista)
    tryCatch({
      d <- leer_lista_archivo(input$archivo_lista$datapath,input$archivo_lista$name)
      if(nrow(d)==0) stop("La lista no contiene alumnos")
      guardar_lista(d)
      showNotification("Lista guardada.",type="message")
    },error=function(e){showNotification(e$message,type="error")})
  })

  observeEvent(input$agregar_alumno,{
    req(admin_ok())
    eq <- trimws(input$nuevo_equipo %||% "")
    al <- trimws(input$nuevo_alumno %||% "")
    if(!nzchar(eq) || !nzchar(al)){
      showNotification("Complete los campos Equipo y Alumno.",type="error");return()
    }
    tryCatch({
      d <- rbind(lista_admin(),data.frame(Equipo=eq,Alumno=al,Activo="SI"))
      guardar_lista(normalizar_lista(d))
      updateTextInput(session,"nuevo_equipo",value="")
      updateTextInput(session,"nuevo_alumno",value="")
      showNotification("Alumno guardado.",type="message")
    },error=function(e){showNotification(e$message,type="error")})
  })

  cambiar_alumno <- function(estado){
    req(admin_ok())
    i <- suppressWarnings(as.integer(input$alumno_admin))
    d <- lista_admin()
    if(length(i)!=1 || is.na(i) || i<1 || i>nrow(d)) return()
    d$Activo[i] <- estado
    tryCatch(guardar_lista(d),error=function(e){showNotification(e$message,type="error")})
  }
  observeEvent(input$activar,{cambiar_alumno("SI")})
  observeEvent(input$desactivar,{cambiar_alumno("NO")})

  observeEvent(input$cambiar_password,{
    req(admin_ok())
    nueva <- input$password_nueva %||% ""
    conf <- input$password_confirmar %||% ""
    if(nchar(nueva)<12 || !identical(nueva,conf)){
      showNotification("Utilice al menos 12 caracteres y confirme la contraseña correctamente.",type="error")
      return()
    }
    tryCatch({
      guardar_password_admin(google_admin, nueva)
      tryCatch(formatear_archivo(google_admin,list(
        list(hoja="Acceso",columnas=1))),
        error=function(e) warning(paste("No se pudo dar formato a Acceso:",e$message)))
      updateTextInput(session,"password_nueva",value="")
      updateTextInput(session,"password_confirmar",value="")
      showNotification("Contraseña cambiada y guardada en Google.",type="message")
    },error=function(e){showNotification(e$message,type="error")})
  })

  #************************************************************************
  #ELIMINAR SEMESTRE ANTERIOR CON RESPALDO PREVIO

  recursos_semestre <- function(s){
    n <- nombres_hojas(s)
    list(
      list(ss=google_admin,hoja=n$lista,tipo="lista"),
      list(ss=google_equipos,hoja=n$equipos,tipo="equipos"),
      list(ss=google_equipos,hoja=n$promedios,tipo="promedios_equipos"),
      list(ss=google_equipos,hoja=n$largas,tipo="larga_equipos"),
      list(ss=google_alumnos,hoja=n$alumnos,tipo="alumnos"),
      list(ss=google_alumnos,hoja=n$promedios,tipo="promedios_alumnos"),
      list(ss=google_alumnos,hoja=n$largas,tipo="larga_alumnos")
    )
  }

  output$descargar_respaldo <- downloadHandler(
    filename=function(){
      req(admin_ok(),input$semestre_borrar)
      paste0("respaldo_rubricas_",gsub("[^A-Za-z0-9]","_",input$semestre_borrar),".zip")
    },
    content=function(file){
      req(admin_ok(),input$semestre_borrar)
      dir <- tempfile("respaldo_")
      dir.create(dir)
      on.exit(unlink(dir,recursive=TRUE),add=TRUE)
      archivos <- character()
      for(r in recursos_semestre(input$semestre_borrar)){
        if(existe_hoja(r$ss,r$hoja)){
          d <- as.data.frame(read_sheet(r$ss,sheet=r$hoja),check.names=FALSE)
          destino <- file.path(dir,paste0(r$tipo,".csv"))
          write.csv(d,destino,row.names=FALSE,fileEncoding="UTF-8")
          archivos <- c(archivos,basename(destino))
        }
      }
      if(length(archivos)==0) stop("No hay hojas del semestre")
      zip::zipr(zipfile=file,files=archivos,root=dir)
      respaldo_generado(input$semestre_borrar)
    }
  )

  observeEvent(input$pedir_borrado,{
    req(admin_ok(),input$semestre_borrar)
    if(!isTRUE(input$respaldo_descargado) ||
       !identical(respaldo_generado(),input$semestre_borrar)){
      showNotification("Primero descargue el respaldo ZIP de este semestre y confirme que lo ha guardado.",type="error")
      return()
    }
    actual <- identical(input$semestre_borrar,semestre)
    showModal(modalDialog(
      title=if(actual) "ADVERTENCIA: Eliminar semestre actual" else
                         "Confirmar eliminación irreversible",
      if(actual) tags$p(style="color:#a00;font-weight:bold;",
        "Se borrará también la lista de alumnos y todas las calificaciones actuales. " ,
        "El semestre quedará reiniciado y vacío."),
      tags$p(paste("Escriba exactamente:",input$semestre_borrar)),
      textInput("texto_borrar","Semestre:"),
      footer=tagList(modalButton("Cancelar"),
                     actionButton("confirmar_borrar","Eliminar definitivamente",
                                  class="btn-danger")),easyClose=FALSE
    ))
  })

  observeEvent(input$confirmar_borrar,{
    req(admin_ok())
    s <- input$semestre_borrar %||% ""
    if(!nzchar(s) ||
       !isTRUE(input$respaldo_descargado) ||
       !identical(respaldo_generado(),s) ||
       !identical(trimws(input$texto_borrar %||% ""),s)){
      showNotification("La confirmación no coincide.",type="error")
      return()
    }
    tryCatch({
      #Google no permite eliminar la ultima pestaña de un archivo.
      #Se crea Inicio si todas las pestañas del archivo pertenecen al semestre.
      grupos <- list(google_admin,google_equipos,google_alumnos)
      for(archivo in grupos){
        targets <- vapply(recursos_semestre(s),function(r){
          if(identical(as.character(r$ss),as.character(archivo))) r$hoja else NA_character_
        },character(1))
        targets <- targets[!is.na(targets)]
        presentes <- sheet_names(archivo)
        if(length(presentes)>0 && all(presentes %in% targets)){
          if(!("Inicio" %in% presentes)) sheet_add(archivo,"Inicio")
        }
      }

      #Se borran las siete pestañas del semestre, nunca los tres archivos.
      for(r in recursos_semestre(s)){
        if(existe_hoja(r$ss,r$hoja)) sheet_delete(r$ss,r$hoja)
      }
      if(identical(s,semestre)){
        #La app sigue viva: vuelve a crear el semestre, pero vacío.
        asegurar_hoja(google_admin,hojas$lista,empty_lista)
        asegurar_hoja(google_equipos,hojas$equipos,empty_equipos)
        asegurar_hoja(google_equipos,hojas$largas,empty_larga_eq)
        asegurar_hoja(google_alumnos,hojas$alumnos,empty_alumnos)
        asegurar_hoja(google_alumnos,hojas$largas,empty_larga_al)
        instalar_promedios(google_equipos,hojas$promedios,hojas$equipos,"equipo")
        instalar_promedios(google_alumnos,hojas$promedios,hojas$alumnos,"alumno")
        formatear_archivo(google_equipos,list(
          list(hoja=hojas$equipos,columnas=ncol(empty_equipos),promedio=ncol(empty_equipos)),
          list(hoja=hojas$largas,columnas=ncol(empty_larga_eq),promedio=ncol(empty_larga_eq)),
          list(hoja=hojas$promedios,columnas=5,promedio=4)))
        formatear_archivo(google_alumnos,list(
          list(hoja=hojas$alumnos,columnas=ncol(empty_alumnos),promedio=ncol(empty_alumnos)),
          list(hoja=hojas$largas,columnas=ncol(empty_larga_al),promedio=ncol(empty_larga_al)),
          list(hoja=hojas$promedios,columnas=6,promedio=5)))
        formatear_archivo(google_admin,list(
          list(hoja=hojas$lista,columnas=ncol(empty_lista))))
        lista_admin(empty_lista)
        datos_equipos(empty_equipos)
        datos_alumnos(empty_alumnos)
      }
      respaldo_generado("")
      updateCheckboxInput(session,"respaldo_descargado",value=FALSE)
      semestres_version(semestres_version()+1L)
      removeModal()
      showNotification(paste(if(identical(s,semestre)) "Semestre reiniciado:" else
                             "Semestre eliminado:",s),type="message",duration=8)
    },error=function(e){
      showNotification(paste("No se pudo completar la eliminación:",e$message),
                       type="error",duration=12)
    })
  })
}

shinyApp(ui=ui,server=server)
