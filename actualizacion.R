library(tidyverse)
library(pdftools)
library(lubridate)

dir_archivos <- "./Archivos/"
dir_funciones <- "./Funciones/"
dir_datos <- "./Datos/"
dir_objetos <- "./Objetos/"
dir_pases <- "./Infopases/"

source(paste0(dir_funciones,"funciones.R"), encoding = "utf-8")

## Descarga de Archivos
descargar_archivo("https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/series.xlsm","series.xlsm")

## Actualización series.xlsm
source(paste0(dir_funciones,"seriese.R"), encoding = "utf-8")

## Carga datos
load(paste0(dir_datos,"series.Rda"))
rm(list = setdiff(ls(), c("dir_archivos", "dir_funciones", "dir_datos",
                          "dir_objetos","SERIESE"))) 

## Informes del día
source(paste0(dir_funciones,"graficos.R"), encoding = "utf-8")
#source(paste0(dir_objetos,"infopases.R"), encoding = "utf-8")
