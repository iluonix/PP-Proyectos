#************************************************************************
#INSTALAR PAQUETES NECESARIOS
#Ejecute este archivo una sola vez en RStudio (desde el proyecto).

paquetes <- c(
  "shiny", "shinydashboard", "googlesheets4", "readxl",
  "sodium", "zip", "httr", "gargle", "rsconnect", "rstudioapi"
)

faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly=TRUE)]

if(length(faltantes)>0){
  message("Instalando: ",paste(faltantes,collapse=", "))
  install.packages(faltantes)
}else{
  message("Todos los paquetes necesarios ya están instalados.")
}
