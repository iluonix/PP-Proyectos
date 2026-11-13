# RStudio necesita Shiny para habilitar Run App. El perfil devuelve
# el instalador que app.R reutiliza para los demás paquetes.
local({
  instalar_paquetes <- function(configuracion = FALSE, solo_shiny = FALSE) {
    paquetes <- c("shiny", "shinydashboard", "googlesheets4", "readxl",
                  "sodium", "zip", "httr", "gargle", "rstudioapi")
    if (configuracion) {
      paquetes <- c(paquetes, "googledrive", "filelock")
    }
    if (solo_shiny) paquetes <- "shiny"

    faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
    if (!length(faltantes)) return(invisible(TRUE))

    rutas_usuario <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep,
                              fixed = TRUE)[[1L]]
    rutas_usuario <- path.expand(rutas_usuario[nzchar(rutas_usuario)])
    if (length(rutas_usuario) && !dir.exists(rutas_usuario[1L])) {
      dir.create(rutas_usuario[1L], recursive = TRUE, showWarnings = FALSE)
    }
    .libPaths(c(rutas_usuario, .libPaths()))
    bibliotecas <- .libPaths()
    biblioteca <- bibliotecas[file.access(bibliotecas, mode = 2L) == 0L][1L]
    if (is.na(biblioteca)) {
      stop("R necesita una carpeta de bibliotecas con permisos de escritura.",
           call. = FALSE)
    }

    repos <- getOption("repos")
    if (is.null(repos) || is.na(repos["CRAN"]) || !nzchar(repos["CRAN"]) ||
        repos["CRAN"] == "@CRAN@") {
      repos["CRAN"] <- "https://cloud.r-project.org"
    }
    message("Instalando los paquetes de R que faltan: ", paste(faltantes, collapse = ", "))
    utils::install.packages(faltantes, lib = biblioteca, repos = repos, dependencies = NA)

    faltantes <- faltantes[!vapply(faltantes, requireNamespace, logical(1), quietly = TRUE)]
    if (length(faltantes)) {
      stop("R no pudo instalar o cargar: ", paste(faltantes, collapse = ", "),
           ". Revise los mensajes de instalación y vuelva a abrir el sistema.",
           call. = FALSE)
    }
    invisible(TRUE)
  }

  en_rstudio <- identical(.Platform$GUI, "RStudio") || identical(Sys.getenv("RSTUDIO"), "1")
  if (interactive() && en_rstudio && !length(find.package("shiny", quiet = TRUE))) {
    instalar_paquetes(solo_shiny = TRUE)
  }
  instalar_paquetes
})
