library(shiny)
library(shinydashboard)
library(googlesheets4)
library(readxl)
library(sodium)

`%||%` <- function(x,y){if(is.null(x) || length(x)==0) y else x}

#************************************************************************
#GOOGLE / PRIVATE
#Versión V4 para el profesor Montúfar.
#La configuración privada se crea en su PC mediante configurar.R.

private_dir <- file.path(getwd(), "private")
config_path <- file.path(private_dir, "config.rds")
cache_path <- file.path(private_dir, "google-token")

if(!file.exists(config_path)){
  stop("Primero ejecute: source('configurar.R')")
}

config <- readRDS(config_path)
campos_config <- c("google_email", "google_admin_sheet_id",
                    "google_equipos_sheet_id", "google_alumnos_sheet_id")
if(!all(campos_config %in% names(config))){
  stop("Configuración incompleta. Ejecute configurar.R.")
}

#No se guarda una contraseña de Google en el codigo.
gs4_auth(email = config$google_email, cache = cache_path)
google_admin <- as_sheets_id(config$google_admin_sheet_id)
google_equipos <- as_sheets_id(config$google_equipos_sheet_id)
google_alumnos <- as_sheets_id(config$google_alumnos_sheet_id)

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
#MIGRACION SEGURA DE COLUMNAS
#Si una hoja vieja contiene Zona horaria y Fecha UTC, se guarda una COPIA
#antes de quitar esas dos columnas. No se pierde ninguna evaluacion.

migrar_campos <- function(ss,hoja,esperados){
  if(!existe_hoja(ss,hoja)) return(invisible(FALSE))
  x <- as.data.frame(read_sheet(ss,sheet=hoja),check.names=FALSE)
  actuales <- names(x)
  if(identical(actuales,esperados)) return(invisible(FALSE))

  faltan <- setdiff(esperados,actuales)
  sobran <- setdiff(actuales,esperados)
  if(length(faltan)>0 || !all(sobran %in% c("Zona horaria","Fecha UTC"))){
    stop(paste0("No se puede migrar automáticamente la hoja '",hoja,
      "'. Sus columnas difieren de las esperadas. Exporte un respaldo y revíselas."))
  }

  if(nrow(x)>0){
    tipo <- if(grepl("^Rubrica Larga",hoja)) "RL" else "Cal"
    stamp <- format(Sys.time(),"%y%m%d%H%M%S")
    respaldo <- paste0("Respaldo ",tipo," ",stamp)
    sheet_copy(ss,from_sheet=hoja,to_sheet=respaldo)
    message("Respaldo en Drive: ",respaldo)
  }

  sheet_write(x[,esperados,drop=FALSE],ss=ss,sheet=hoja)
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

es_error_formula <- function(ss,hoja){
  x <- tryCatch(as.data.frame(range_read(ss,sheet=hoja,range="A1",
                                  col_names=FALSE)),error=function(e) NULL)
  #Si Google no consigue devolver A1, vale la pena probar la otra sintaxis.
  if(is.null(x) || nrow(x)==0 || ncol(x)==0) return(TRUE)
  val <- as.character(x[1,1])
  grepl("^#(ERROR|REF|NAME|VALUE|N/A)",val)
}

instalar_promedios <- function(ss,hoja_prom,hoja_cal,tipo){
  if(!existe_hoja(ss,hoja_prom)) sheet_add(ss,hoja_prom)
  sep <- locale_separador(ss)
  escribir <- function(separador){
    #SOLO el resumen derivado es reemplazable.
    range_clear(ss,range=paste0("'",hoja_prom,"'!A:Z"),reformat=FALSE)
    f <- data.frame(x=gs4_formula(formula_promedios(hoja_cal,tipo,separador)))
    range_write(ss,data=f,sheet=hoja_prom,range="A1",
                col_names=FALSE,reformat=FALSE)
  }
  escribir(sep)
  #Reintento con el otro separador por si el Google Sheets tiene otra region.
  if(es_error_formula(ss,hoja_prom)){
    alternativo <- if(sep==",") ";" else ","
    escribir(alternativo)
    if(es_error_formula(ss,hoja_prom)){
      stop(paste("No se pudo corregir la fórmula de",hoja_prom,
                 "Revise el idioma o la región del archivo de Google Sheets."))
    }
  }
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

asegurar_hoja(google_admin,hojas$lista,empty_lista)
asegurar_hoja(google_admin,"Rubrica Corta",rubrica)
asegurar_hoja(google_admin,"Rubrica Larga",rubrica_larga)
asegurar_hoja(google_equipos,hojas$equipos,empty_equipos)
asegurar_hoja(google_equipos,hojas$largas,empty_larga_eq)
asegurar_hoja(google_alumnos,hojas$alumnos,empty_alumnos)
asegurar_hoja(google_alumnos,hojas$largas,empty_larga_al)

migrar_campos(google_equipos,hojas$equipos,campos_corta("equipo"))
migrar_campos(google_equipos,hojas$largas,campos_larga("equipo"))
migrar_campos(google_alumnos,hojas$alumnos,campos_corta("alumno"))
migrar_campos(google_alumnos,hojas$largas,campos_larga("alumno"))

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

url_google <- function(id){
  paste0("https://docs.google.com/spreadsheets/d/",id,"/edit")
}

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
        tags$p("Si ha olvidado la contraseña, el propietario puede restablecerla ",
               "desde RStudio con recuperar_password.R.")
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
                   target="_blank",class="btn btn-primary"))),
        tabPanel("Rúbricas",br(),
          tableOutput("tabla_comparativa")),
        tabPanel("Contraseña",br(),
          tags$p("Puede cambiar su contraseña mientras tenga acceso a Administración."),
          password_con_ojo("password_nueva","Nueva contraseña (12 caracteres mínimo):"),
          password_con_ojo("password_confirmar","Confirmar nueva contraseña:"),
          actionButton("cambiar_password","Guardar nueva contraseña"),
          tags$p("Si olvida la contraseña, ejecute source('recuperar_password.R') desde RStudio en el equipo del propietario."))
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
      nuevo_hash <- sodium::password_store(nueva)
      sheet_write(data.frame(Hash=nuevo_hash,stringsAsFactors=FALSE),
                  ss=google_admin,sheet="Acceso")
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
