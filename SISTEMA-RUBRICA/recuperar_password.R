#************************************************************************
#RECUPERAR CONTRASEÑA DE ADMINISTRACIÓN
#
#Ejecute este archivo desde RStudio en el equipo que tenga autorizada
#la cuenta de Google propietaria de las hojas.
#
#No es un enlace público de recuperación ni solicita la clave de Google.
#************************************************************************

paquetes <- c("shiny","sodium","googlesheets4")
faltantes <- paquetes[!vapply(paquetes,requireNamespace,logical(1),quietly=TRUE)]
if(length(faltantes)>0){
  stop(paste("Instale los paquetes necesarios:",paste(faltantes,collapse=", ")))
}

config_path <- file.path(getwd(),"private","config.rds")
if(!file.exists(config_path)){
  stop("No se encontró private/config.rds. Abra el proyecto correcto en RStudio.")
}

config <- readRDS(config_path)
if(!all(c("google_email","google_admin_sheet_id") %in% names(config))){
  stop("La configuración de Google está incompleta.")
}

cache_path <- file.path(getwd(),"private","google-token")
if(!dir.exists(cache_path)){
  stop("No se encontró la autorización de Google en private/google-token.")
}

googlesheets4::gs4_auth(email=config$google_email,cache=cache_path)

#************************************************************************
#FORMULARIO LOCAL CON OJO PARA MOSTRAR Y OCULTAR

campo_con_ojo <- function(id,texto){
  shiny::tags$div(class="password-eye-container",
    shiny::tags$label(`for`=id,texto),
    shiny::tags$div(class="password-eye-row",
      shiny::passwordInput(id,label=NULL,width="100%"),
      shiny::tags$button(type="button",class="password-eye-toggle",
        title="Mostrar contraseña",`aria-label`="Mostrar contraseña",
        `aria-pressed`="false",shiny::icon("eye"))
    )
  )
}

estilo_ojo <- shiny::tags$head(
  shiny::tags$style(shiny::HTML("
    .password-eye-container { max-width:460px; margin:0 0 18px; }
    .password-eye-container > label { display:block; margin-bottom:6px; font-weight:600; }
    .password-eye-row { position:relative; width:100%; }
    .password-eye-row .shiny-input-container { width:100% !important; margin-bottom:0; }
    .password-eye-row .form-group { margin-bottom:0; }
    .password-eye-row input { padding-right:46px; height:37px; }
    .password-eye-toggle { position:absolute; right:4px; top:50%;
      transform:translateY(-50%); height:30px; width:40px; z-index:3;
      border:0; border-radius:4px; background:transparent; color:#566573;
      display:flex; align-items:center; justify-content:center; cursor:pointer; }
    .password-eye-toggle:hover, .password-eye-toggle:focus-visible {
      background:#eef4f7; color:#226b99; outline:1px solid #226b99; }
  ")),
  shiny::tags$script(shiny::HTML("
    $(document).on('click','.password-eye-toggle',function(e){
      e.preventDefault();
      var b=$(this);
      var f=b.closest('.password-eye-row').find('input').first();
      if(!f.length) return;
      var mostrar=f.attr('type')==='password';
      f.attr('type',mostrar?'text':'password');
      b.find('i').removeClass('fa-eye fa-eye-slash')
        .addClass(mostrar?'fa-eye-slash':'fa-eye');
      b.attr('title',mostrar?'Ocultar contraseña':'Mostrar contraseña');
      b.attr('aria-label',mostrar?'Ocultar contraseña':'Mostrar contraseña');
      b.attr('aria-pressed',String(mostrar));
    });
  "))
)

ui <- shiny::fluidPage(
  estilo_ojo,
  shiny::tags$div(style="max-width:620px;margin:36px auto;padding:0 14px;",
    shiny::h2("Restablecer contraseña de Administración"),
    shiny::p("Este formulario se ejecuta únicamente en su equipo y utiliza la cuenta de Google ya autorizada en el proyecto."),
    shiny::p("Escriba una nueva contraseña de al menos 12 caracteres y confírmela."),
    campo_con_ojo("nueva_password","Nueva contraseña:"),
    campo_con_ojo("confirmacion_password","Confirmar nueva contraseña:"),
    shiny::actionButton("guardar_password","Guardar nueva contraseña",
      class="btn-primary"),
    shiny::tags$hr(),
    shiny::actionButton("salir","Cerrar formulario")
  )
)

server <- function(input,output,session){
  guardando <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(input$guardar_password,{
    if(isTRUE(guardando())) return()
    nueva <- input$nueva_password
    confirmacion <- input$confirmacion_password

    if(is.null(nueva) || nchar(nueva)<12){
      shiny::showNotification("La contraseña debe contener al menos 12 caracteres.",
        type="error",duration=7)
      return()
    }
    if(!identical(nueva,confirmacion)){
      shiny::showNotification("Las contraseñas no coinciden. Verifique ambos campos.",
        type="error",duration=7)
      return()
    }

    guardando(TRUE)
    on.exit(guardando(FALSE),add=TRUE)

    tryCatch({
      hash <- sodium::password_store(nueva)
      googlesheets4::sheet_write(
        data.frame(Hash=hash,stringsAsFactors=FALSE),
        ss=config$google_admin_sheet_id,
        sheet="Acceso"
      )

      shiny::showModal(shiny::modalDialog(
        title="Contraseña restablecida",
        shiny::p("Su contraseña de Administración se actualizó correctamente en Google Sheets."),
        shiny::p("Cierre este formulario y vuelva a abrir la aplicación para iniciar sesión."),
        footer=shiny::actionButton("cerrar_exito","Cerrar"),
        easyClose=FALSE
      ))
    },error=function(e){
      shiny::showNotification(
        paste("No se pudo restablecer la contraseña:",e$message),
        type="error",duration=12
      )
    })
  })

  shiny::observeEvent(input$cerrar_exito,{shiny::stopApp(TRUE)})
  shiny::observeEvent(input$salir,{shiny::stopApp(FALSE)})
}

cat("Se abrirá un formulario local para restablecer la contraseña de Administración.\n")
shiny::runApp(shiny::shinyApp(ui,server),host="127.0.0.1",launch.browser=TRUE)
