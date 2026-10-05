paquetes <- c(
  "shiny",
  "shinydashboard",
  "googlesheets4",
  "readxl",
  "digest",
  "rsconnect"
)

faltantes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if(length(faltantes)==0){

  cat(
    "Todos los paquetes necesarios ya estan instalados.\n"
  )

}else{

  cat(
    "Instalando: ",
    paste(
      faltantes,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )

  install.packages(
    faltantes,
    dependencies = TRUE
  )
}
