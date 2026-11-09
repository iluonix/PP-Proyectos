# Sistema de Rúbricas UAEH — V4

Aplicación Shiny para evaluar proyectos académicos con una rúbrica de seis aspectos, generar el desglose P.C./U.T. y mantener la **calificación final de la rúbrica corta y la larga iguales**.

## Para el profesor Montúfar

**No es necesario modificar `app.R`.** El proyecto se configura una sola vez desde su computadora.

1. Descargue o clone este repositorio y abra `SISTEMA-RUBRICA.Rproj` en RStudio.
2. Ejecute `source("instalar_paquetes.R")`.
3. Ejecute `source("configurar.R")`, autorice `marco.montufar14@gmail.com` y configure su contraseña si es la primera vez. Si ya tiene sus tres Google Sheets, puede pegar sus enlaces para reutilizarlos.
4. Ejecute `shiny::runApp()` y compruebe el acceso a Administración y el guardado de evaluaciones.
5. Si desea publicar, conecte RStudio con su propia cuenta de shinyapps.io y ejecute `rsconnect::listDeploymentFiles(".")` antes de pulsar **Publish**.

> La carpeta `private/` contiene la autorización de Google y los IDs de los Sheets. **No se sube a GitHub**. Se genera únicamente en la PC del profesor. Al publicar Shiny desde RStudio se debe comprobar que `private/config.rds` y `private/google-token/` sí se incluyan en el despliegue privado.

## Uso

- **Evaluar equipo:** los evaluadores escriben su nombre y califican; no necesitan cuenta de Google ni registro previo.
- **Resultados:** consulte promedios de equipos y alumnos.
- **Administración:** requiere una contraseña y permite gestionar alumnos, semestres, enlaces a Google y rúbricas.
- **Recuperar contraseña:** cierre la app y ejecute `source("recuperar_password.R")` desde su RStudio. Abre un formulario local con mostrar/ocultar y confirmación.

Consulte [INSTRUCCIONES_GITHUB.md](INSTRUCCIONES_GITHUB.md) para el procedimiento completo de instalación, actualización y publicación.
