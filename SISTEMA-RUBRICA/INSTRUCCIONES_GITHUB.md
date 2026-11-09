# Guía de entrega por GitHub — Sistema de Rúbricas V4

## A. Para subir la nueva versión a GitHub (quien desarrolla)

**Antes de editar:** conserve una copia de su repositorio y de las evaluaciones de Google.

1. Descargue y extraiga `SISTEMA-RUBRICA-GITHUB-MONTUFAR.zip`.
2. Abra la carpeta del repositorio local `SISTEMA-RUBRICA` en el Explorador de Windows o en GitHub Desktop (**Repository → Show in Explorer**).
3. Copie los archivos extraídos al **mismo nivel donde se encuentra el `app.R` del repositorio**, reemplazando las versiones anteriores de `app.R` y `recuperar_password.R`. Conserve las demás carpetas del repositorio si contienen material necesario.
4. Verifique que la pestaña Changes muestre los archivos modificados/añadidos y que **NO aparezca `private/`, `config.rds`, `google-token/`, `.Renviron` ni ningún token de autenticación**.
5. En GitHub Desktop escriba el resumen `Actualización V4 de rúbricas` y pulse **Commit to main**, después **Push origin** (o la rama principal que utilice).
6. Abra GitHub en el navegador y compruebe que aparezcan `app.R`, `configurar.R`, `recuperar_password.R`, `instalar_paquetes.R`, el proyecto `.Rproj` y las instrucciones.

**Importante:** `.gitignore` no elimina credenciales que Git ya rastreaba. Si `private/` apareciera en Changes o en GitHub, detenga el proceso; retire esos archivos del seguimiento y revoque cualquier token que se haya publicado.

## B. Para el profesor Montúfar: obtener la versión en su computadora

### Si ya clonó el repositorio

1. Abra **GitHub Desktop** con el repositorio `SISTEMA-RUBRICA`.
2. Pulse **Fetch origin** y después **Pull origin**, si aparece.
3. Si GitHub advierte sobre cambios locales sin guardar, **no fuerce el Pull**: primero guarde o resuelva esos cambios.
4. Abra el archivo `SISTEMA-RUBRICA.Rproj` desde la carpeta local del repositorio.
5. Sus archivos locales `private/` normalmente se conservan porque están excluidos de Git.

### Si todavía no lo descargó

1. Abra el repositorio de GitHub y pulse **Code → Open with GitHub Desktop**, o use **Code → Download ZIP**.
2. Descargue el proyecto completo, no solamente `app.R`.
3. Descomprímalo si descargó el ZIP y abra `SISTEMA-RUBRICA.Rproj` en RStudio.

## C. Primera instalación en RStudio de Montúfar

Compruebe que la consola está en la carpeta correcta:

```r
getwd()
list.files()
```

Deberían aparecer `app.R`, `configurar.R`, `instalar_paquetes.R` y `recuperar_password.R`.

Instale paquetes una sola vez:

```r
source("instalar_paquetes.R")
```

Configure Google:

```r
source("configurar.R")
```

Se solicitará autorización de la cuenta **`marco.montufar14@gmail.com`**. Si ya existe una configuración privada para esa misma cuenta, se reutilizan los archivos existentes. Si no existe configuración, el script ofrece reutilizar tres Sheets que ya estén creados pegando sus enlaces, o crear tres nuevos en el Drive autenticado.

El script comprobará la pestaña **Acceso** del archivo administrativo. Si no existe una contraseña compatible, solicitará una contraseña inicial de al menos 12 caracteres y su confirmación. Si ya existe, la conservará.

Al terminar debe aparecer una carpeta local similar a esta:

```text
SISTEMA-RUBRICA/
├── app.R
├── configurar.R
├── recuperar_password.R
├── instalar_paquetes.R
└── private/                  ← NO aparece en GitHub
    ├── config.rds
    └── google-token/
```

Pruebe la aplicación:

```r
shiny::runApp()
```

Verifique que Administración solicite su contraseña, que aparezca la lista de alumnos y que se guarden correctamente las evaluaciones en Google.

## D. Recuperar contraseña

Cierre la aplicación y, desde RStudio, ejecute:

```r
source("recuperar_password.R")
```

Se abrirá un formulario local con dos campos y opciones de ojo: **Nueva contraseña** y **Confirmar nueva contraseña**. Esta recuperación no se publica como enlace web para los evaluadores.

## E. Publicar desde RStudio del profesor

1. Inicie sesión en su cuenta de shinyapps.io y conecte la cuenta desde RStudio (por ejemplo, con el comando `rsconnect::setAccountInfo(...)` que proporciona shinyapps.io).
2. En la carpeta del proyecto, revise los archivos que se enviarán:

```r
rsconnect::listDeploymentFiles(".")
```

Deben estar `app.R`, `private/config.rds` y el contenido de `private/google-token/`.

3. **No publique** si falta `private/`: la aplicación necesita la autorización de Google. La carpeta **no** debe subirse a GitHub, pero sí debe estar dentro del bundle **privado** de despliegue.
4. Pulse **Publish** o ejecute:

```r
rsconnect::deployApp(appDir=".", appName="sistema-rubrica", appVisibility="public")
```

5. Comparta únicamente el enlace generado con los profesores evaluadores.

**Aviso:** el método sencillo de autenticación utilizado por esta aplicación guarda una autorización OAuth en el proyecto local. Si dicha autorización caduca o se revoca, el profesor deberá reautorizarla desde RStudio y volver a publicar. Trate `private/` como una credencial sensible.

## F. Actualizaciones futuras

Cuando se modifique `app.R` en GitHub, Montúfar podrá hacer **Pull** y volver a publicar. No debe volver a ejecutar `configurar.R` si el acceso a Google y la contraseña funcionan. No sustituya ni borre la carpeta privada ya configurada.

Si existe un error al actualizar, guarde el mensaje completo de RStudio y revise los archivos antes de continuar.
