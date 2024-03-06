# polio-risk
Herramienta para el análisis de riesgo de Polio

# Pasos a seguir

De manera resumida, estos son los pasos por seguir para ejecutar la herramienta.

1. Instalar dependencias.
2. Descargar herramienta.
3. Instalación de paquetes de R.
4. Preparación de shapefiles.
5. Llenar la entrada de datos.
6. Ejecutar herramienta.

# Dependencias

La herramienta de análisis de riesgo de polio fue programada utilizando el lenguaje de programación R. Las dependencias para ejecutar esta herramienta son:

1. Navegador web (Firefox, Google Chrome, Safari, etc.)
2. Excel
3. R
4. RStudio

# Descarga de herramienta

## Navegador Web

Si se desea descargar la herramienta desde el navegador web: 

1. Ingresar al repositorio de github [https://github.com/Oliversinn/polio-risk].
2. Presionar el botón verde que dice "Code" o "Código".
3. Presionar el botón de "Download Zip" o "Descargar Zip".
4. Descomprimir archivo zip.

## Utilizando git

```bash
git clone https://github.com/Oliversinn/polio-risk.git
```

# Instalación de paquetes de R

1. En la carpeta de la herramienta, hacer clic en polio-risk.Rproj. Esto abrirá una ventana de RStudio en la carpeta de la herramienta. 
2. En el panel de archivos hacer clic en el archivo install.R para abrir el archivo.
3. Ejecutar el archivo haciendo clic en el botón "Run".
4. Mientras se estén instalando las dependencias, se nos mostraran algunas preguntas en la terminal, a estas preguntas responder que sí escribiendo "y" y presionando "enter" en la terminal.

# Preparación de Shapefiles

Para que la herramienta logre imprimir los mapas de los distintos países, se necesitan estandarizar los archivos que contienen los polígonos de los mapas, estos archivos los llamaremos shapefiles. Para este procedimiento usted necesita colocar los shapefiles en formato SHP o JSON en la carpeta `./src/Shapefile_prep` de la carpeta del proyecto. Con la ayuda de mapshaper [https://mapshaper.org] llenar la configuración encontrada en el Excel `./src/Shapefile_prep/shapefile_settings.xlsx`.

## Definición de variable de configuración

* Tipo de archivo: Colocar el tipo de archivo de sus shapefiles (JSON/SHP).
* Nombre archivo: Nombre de los shapefiles sin su extensión.
* ADMIN1 ID: Nombre de la variable del shapefile que contiene el código del nivel administrativo más alto.
* ADMIN1 NOMBRE: Nombre de la variable del shapefile que contiene el nombre del nivel administrativo más alto.
* ADMIN2 ID: Nombre de la variable del shapefile que contiene el código del nivel administrativo más bajo.
* ADMIN2 NOMBRE: Nombre de la variable del shapefile que contiene el nombre del nivel administrativo más bajo.
* GEOMETRY: Nombre de la variable del shapefile que contiene los polígonos (usualmente se llama "geometry").

## Mapshaper.org

El sitio de mapshaper.org nos permite cargar los shapefies a un navegador y explorarlos. 

1. Ingresar a https://mapshaper.org.
2. Hacer clic en el botón de "select" file para subir nuestros shapefiles.
3. Seleccionar y subir shapefiles.
4. Seleccionar el botón del cursor que se encuentra en el lado derecho.
5. Hacer clic en algún polígono.
6. Con ayuda del recuadro colocado en la esquina superior izquierda, obtener el nombre de las variables (texto color gris) y colocarlo en el Excel de configuración `./src/Shapefile_prep/shapefile_settings.xlsx`.
7. Guardar cambios.

## Ejecutar script de estandarización

Una vez se tengan los shapefiles en la carpeta `./src/Shapefile_prep/` y el archivo `./src/Shapefile_prep/shapefile_settings.xlsx` con las configuraciones adecuadas, abrir y ejecutar el archivo `./src/Shapefile_prep/geodata_to_shapefiles.R`. 

1. En el panel de archivos, hacer clic en el archivo geodata_to_shapefiles.R
2. En el editor de texto, seleccionar todas las líneas del archivo y ejecutar haciendo clic en el botón "Run".

## Resultado

Como resultado:

1. Los shapefiles estandarizadas se guardarán en `./src/Data/shapefiles/`,
2. Se creará el archivo `./src/Shapefile_prep/geocodigos_nombres.xlsx` que nos servirá para nuestra entrada de datos.

# Llenar la entrada de datos

* Para llenar los datos, es necesario haber estandarizados los shapefiles previamente.
* En la carpeta `./template/` encontraran una carpeta con una platilla de llenado de datos para diferentes idiomas.
* En la primera pestaña del template, se coloca información general de los países.
* En todas las pestañas verán que las primeras 4 variables son las variables de los geo códigos e información sobre los shapefiles. Para llenar estas 4 columnas, utilizar las 4 columnas generadas en el archivo `./src/Shapefile_prep/geocodigos_nombres.xlsx`.
* Llenar el resto de las variables.

# Ejecutar herramienta.

Para ejecutar la herramienta debemos abrir y ejecutar el archivo `./src/run.R`.

# Control de calidad

Para ejecutar el control de calidad debemos abrir y ejecutar el archivo `./src/qa.R`.

# License 

NOTICE TO USER:  Please read this Software License carefully.  By using all or any portion of this Software, you accept all the terms and conditions of this License and recognize that this License is as enforceable as any written negotiated document signed by you.

The Pan American Health Organization and its affiliates (“PAHO”) hold all intellectual property rights in the software developed by PAHO.  By means of this License PAHO will grant you permission to use the Software only in accordance with the terms and conditions set forth herein. 
1.	Software License. As long as you comply with the terms and conditions of this License, PAHO grants you a non-exclusive, royalty-free, transferable license to use, reproduce, modify and distribute the Software as further set forth below in the understanding that the Software will be used solely for non-commercial purposes.

2.	Intellectual Property Ownership, Copyright Protection.  Copyright of the Software belongs to PAHO.  All rights not expressly granted herein are reserved by PAHO.

3.	Trademark, Name and Logo. The trademarks, names and logos, included in the Software, are the property of PAHO.  You are not permitted to use or reproduce them without the prior express written consent of PAHO other than as permitted by the Software. Software may not be used to promote licensees’ activities, products or services.

4.	Notices.  PAHO copyrights shall be acknowledged with a reference in the Software and any printed materials or electronic documentation accompanying the use of the software, as follows:
Comprehensive immunization special program, Pan American Health Organization, 2023

5.	Use of packages. Packages utilized in the development of this software are subject to their own licensing. PAHO distributed open software is compliant with licensing of third-party software used in this software at the time of release. 

6.	Copies. You may convey unmodified copies of the Software’s source code as you receive it, in any medium, provided that you distribute it under the terms of this License and that you conspicuously on each copy:

a.	Include the copyright notice referred above, 
b.	Include the text of this License,
c.	Keep intact all notices of the absence of any warranty, and
d.	Include the prohibitions on of commercial use of the Software and the promotion of licensee’s activities, products or services. 

7.	Each time a copy of the Software is distributed the recipient automatically receives a license from PAHO subject to these same terms and conditions.

8.	Modifications.  You may perform structural modifications of this Software, provided that you meet these conditions:

a.	The modified version of the Software must carry prominent notices stating that you modified the Software,
b.	The modified version must be released under this License, and
c.	You shall maintain all system credits and refrain from using the software for commercial purposes.

9.	Updates.  Any and all Updates, if provided, are done so on the same basis as the original Software License unless otherwise indicated in writing by PAHO.  PAHO reserves the right to provide Updates to this Software on amended terms as it sees fit. User shall be responsible for installing the latest version of the software. 

10.	Disclaimer.  PAHO disclaims any and all implied warranties or conditions, including any implied warranty of title, non-infringement, merchantability or fitness for a particular purpose.  The information contained within the Software is provided in good faith and every care has been taken in its preparation.  PAHO does not and cannot warrant the performance or results you may obtain by using the Software. PAHO provides no warranties, nor does it assume any legal liability or responsibility for the accuracy, completeness or usefulness of any of the information supplied.  Unless expressed herein, no condition, warranty or representation by PAHO is given and shall not be implied in relation to the Software available for downloading.  The disclaimer of responsibility applies to any failure of performance, error, omission, interruption, deletion, defect, delay in operation or transmission, computer virus, communication line failure, theft or destruction.

11.	Limitation of Liability.  In no event will PAHO be responsible to you for any damages, claims or costs whatsoever, or any consequential indirect or incidental damages or any lost profits or lost savings, even if a PAHO representative has been advised of the possibility of such loss, damages, claims or costs. Furthermore, PAHO will not be responsible for any claim by any third party arising out of use or inability to use the Software.

12.	Privileges and Immunities. Nothing contained in this License shall be deemed a waiver, express or implied, of any immunity from suit, judicial process, confiscation, taxation, or other immunity or privilege which PAHO may enjoy, whether pursuant to treaty, convention, law, order or decree of an international or national character or otherwise, or in accordance with international customary law.

13.	Resolution of Disputes.  You and PAHO shall use their best efforts to settle amicably any dispute, controversy or claim arising out of, or relating to this License.  Unless any such dispute, controversy or claim between the parties arising out of or relating to this Licensee or breach, termination or invalidity thereof is settled amicably within sixty (60) days after receipt by one Party of the other party’s request for such amicable settlement, such dispute, controversy or claim shall be referred by either party to arbitration in accordance with the UNCITRAL Arbitration Rules then obtaining.  The arbitral tribunal shall have no authority to award punitive damages.  Any arbitration award rendered as a result of such arbitration shall be considered to be the final adjudication of any such controversy, claim or dispute and shall bind the Parties.

14.	Severability.  Any provision of this License prohibited by the laws of any jurisdiction shall, as to such jurisdiction, be ineffective to the extent of such prohibition, without invalidating the remaining provisions of this License.

15.	Termination for cause.  This License will commence on the date of first use and unless terminated in accordance with the terms hereof, will continue in effect indefinitely.  PAHO has the right to terminate this License if, after thirty (30) days notice to Licensee, any of the following conditions has not been cured by Licensee:

 - (i) Licensee Uses or permits the Software or its updates to be used, in any manner or for any purpose not authorized hereunder; or

 - (ii)	Licensee is otherwise in breach of this License in any material respect.

