# Sistema de Rúbricas UAEH

Aplicación Shiny del profesor Montúfar para evaluar proyectos y consultar los resultados por equipo y por alumno. La cuenta de Google del sistema es **marco.montufar14@gmail.com**.

## Abrir en RStudio

1. Extraiga el ZIP completo. Si ya usaba el sistema, copie la carpeta `private/` de la instalación anterior a la nueva carpeta del proyecto en el equipo del profesor. Esto conserva la autorización y los IDs de los libros, incluidos los que cambiaron de nombre.
2. Abra `SISTEMA-RUBRICA.Rproj` en **RStudio Desktop**. Abra `app.R` en el editor y pulse **Run App**. R instala automáticamente los paquetes que falten. La primera descarga requiere acceso a internet.
3. En la ventana del primer arranque, elija **Rehacer libros** si desea reemplazar los anteriores. Si desea seguir con los actuales, elija **Abrir libros actuales** o **Configurar**, según aparezca. Si Google solicita acceso, autorice **marco.montufar14@gmail.com** en el navegador. RStudio solicita la contraseña de Administración en un campo oculto cuando hace falta.

Después del primer arranque, abra `app.R` y pulse **Run App** para entrar.

## Libros de Google Sheets

**Rehacer libros** crea y verifica tres libros nuevos de Administración, Equipos y Alumnos. Revise los libros anteriores detectados y confirme el reemplazo. El sistema los envía a la papelera de Google Drive después de activar los nuevos. Las tablas nuevas del semestre actual empiezan vacías. Cargue la lista de alumnos desde **Administración**.

Para reiniciar más adelante, use **Administración → Google → Reiniciar libros**. Cuando el proceso termine, pulse **Run App** otra vez. Si se interrumpe, pulse **Run App** y elija reanudar.

Cuando abre libros existentes, el sistema corrige los encabezados de evaluación que puede reconocer. Si no puede identificar las columnas, conserva la pestaña anterior en `Respaldo …` y habilita una tabla con los encabezados correctos.

Para recuperar el acceso, pulse **Restablecer contraseña** en el inicio de sesión de **Administración** desde RStudio Desktop. Autorice la cuenta del profesor y establezca una nueva contraseña en las ventanas de RStudio.

## Formato de la lista de alumnos

En **Administración → Alumnos**, puede cargar un archivo Excel o CSV con las columnas `Equipo`, `Alumno` y `Activo`. Este ejemplo muestra los encabezados y dos filas ficticias.

```csv
Equipo,Alumno,Activo
Equipo 1,Estudiante A,SI
Equipo 1,Estudiante B,SI
```

## Subir a GitHub Desktop

- **Repositorio nuevo.** En GitHub Desktop seleccione **File → New repository**. Copie los archivos extraídos, incluidos `.Rprofile` y `.gitignore`, a la carpeta creada. En **Changes**, haga el commit y pulse **Publish repository**.
- **Repositorio existente.** Abra el repositorio en GitHub Desktop y copie los archivos del ZIP en la carpeta donde está `app.R`. Puede retirar los archivos auxiliares que pertenezcan a la versión anterior y ya estén integrados en esta. Revise los cambios, haga el commit y pulse **Push origin**.

Mantenga `private/`, los tokens de Google y las listas reales de alumnos fuera de GitHub. Revise que estén ausentes de **Changes** antes de confirmar el commit. GitHub aloja el código. Para ejecutar la aplicación, abra el proyecto en RStudio y pulse **Run App**.
