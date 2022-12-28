## Script para leer el archivo series.xlsm

## Requiere los siguientes paquetes:
# readxl
# dplyr
# stringr


## Cargar paquetes
packages <- c("readxl", "dplyr", "stringr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))


## Función para reemplazar NAs en nombres
nombres_NA <- function(linea, inicio=2) {
  for(i in (inicio:length(linea))) {
    if (grepl("^\\.",linea[i])) linea[i] <- NA
    if (is.na(linea[i]) & i>2) linea[i] <- linea[i-1]
  }
  linea <- str_remove_all(linea,"\\n")
  linea <- str_remove_all(linea,"\\r")
  #linea <- str_replace_all(linea,"\\r"," ")
  linea
}

## Función para filtrar por series diarias y eliminar columnas vacías
series_diarias <- function(df) {
  df <- filter(df, is.na(`Tipo\r\nde serie`) |  `Tipo\r\nde serie`=="D" | 
                 `Tipo\r\nde serie`=="Tipo\r\nde serie")
  if(is.character(df$Fecha)) {
    df <- df[-which(is.na(df[which(df[1]=="Fecha"),]))]
    df$`Tipo\r\nde serie` <- NULL
  } else {
    df$`Tipo\r\nde serie` <- NULL
    df <- df[,!(names(df) %in% names(df)[str_detect(names(df),"^\\.")])]
    names(df) <- str_remove_all(names(df),"- ")
  }
  df
}

## Función para formatear series
formatear <- function(df) {
  if(is.character(df$Fecha)) {
    df[1] <- as.Date(as.numeric(unlist(df[1])),origin = "1899-12-30")
  } else {
    df[1] <- as.Date(df$Fecha,origin = "1899-12-30")
  }
  df <- mutate_if(df,is.character,as.numeric)
}

## Base Monetaria
df <- read_excel(paste0(dir_archivos,"series.xlsm"), sheet="BASE MONETARIA", skip=3) %>% 
  series_diarias()
nombres <- str_remove_all(paste(nombres_NA(names(df)), nombres_NA(as.character(df[1,])),
                                nombres_NA(as.character(df[2,])), nombres_NA(as.character(df[3,])),
                                nombres_NA(as.character(df[4,]))), " NA")
nombres[12:14] <- str_remove_all(nombres[12:14], " Intereses, Primas y Remun. Ctas. Ctes.")
nombres[18:19] <- str_remove_all(nombres[18:19], " Cheques Cancelatorios\\(3\\)")
nombres[20:21] <- str_remove_all(nombres[20:21], " Total\\(5\\) = \\(1\\+2\\+3\\+4\\)")
nombres[25:26] <- str_remove_all(nombres[25:26], " Cheques Cancelatorios\\(10\\)")
nombres[27:28] <- str_remove_all(nombres[27:28], " Total\\(12\\) = \\(8\\+9\\+10\\+11\\)")
names(df) <- nombres
df <- df[-c(1:5),] %>% formatear()
BASE_MONETARIA <- df

## RESERVAS
df <- read_excel(paste0(dir_archivos,"series.xlsm"), sheet="RESERVAS", skip=3) %>% 
  series_diarias()
nombres <- str_remove_all(paste(nombres_NA(names(df)), nombres_NA(as.character(df[1,])),
                                nombres_NA(as.character(df[2,])), nombres_NA(as.character(df[3,])),
                                nombres_NA(as.character(df[4,]))), " NA")
nombres[5] <- str_remove_all(nombres[5], " Divisas - Pase Pasivo en USD con el Exterior")
nombres[11:12] <- str_remove_all(nombres[11:12], " Factores de explicación de las Reservas Internacionales Otros \\(incl. pases pasivos en USD con el exterior\\)")
names(df) <- nombres
df <- df[-c(1:5),] %>% formatear()
RESERVAS <- df

## DEPOSITOS
df <- read_excel(paste0(dir_archivos,"series.xlsm"), sheet="DEPOSITOS", skip=5) %>% 
  series_diarias()
names(df)[26] <- "Millones de Pesos M2"
df <- df %>% formatear()
DEPOSITOS <- df

## PRESTAMOS
df <- read_excel(paste0(dir_archivos,"series.xlsm"), sheet="PRESTAMOS", skip=3) %>% 
  series_diarias()
nombres <- str_remove_all(paste(nombres_NA(names(df)), nombres_NA(as.character(df[1,])),
                                nombres_NA(as.character(df[2,]))), " NA")
nombres[9] <- str_remove_all(nombres[9], " Otros")
nombres[17:19] <- str_remove_all(nombres[17:19], " Otros")
names(df) <- nombres
df <- df[-c(1:5),] %>% formatear()
PRESTAMOS <- df

## TASAS DE MERCADO
df <- read_excel(paste0(dir_archivos,"series.xlsm"), sheet="TASAS DE MERCADO", skip=3) 
## Nombrar columnas
nombres <- str_remove_all(paste(nombres_NA(names(df)), nombres_NA(as.character(df[1,])),
                                nombres_NA(as.character(df[2,])),nombres_NA(as.character(df[3,])),
                                nombres_NA(as.character(df[4,]))), " NA")
names(df) <- nombres
df <- df[-c(1:5),] %>% formatear()
TASAS_DE_MERCADO <- df

## INSTRUMENTOS DEL BCRA
df <- read_excel(paste0(dir_archivos,"series.xlsm"), sheet="INSTRUMENTOS DEL BCRA", skip=3) 
df <- df[-which(is.na(df[which(str_detect(unlist(df[1]),"Fecha")),]))]
## Nombrar columnas
nombres <- str_remove_all(paste(nombres_NA(names(df)), nombres_NA(as.character(df[1,])),
                                nombres_NA(as.character(df[2,])), nombres_NA(as.character(df[3,])),
                                nombres_NA(as.character(df[4,]))), " NA")
nombres[nombres==
          "Tasas de Interés Tasa de política monetaria (8) de Entidades Financieras (2); (4) TNA"] <- 
  "Tasa de política monetaria"
nombres[nombres==
          "Tasas de Interés Tasa de política monetaria (8) de Entidades Financieras (2); (4) TEA"] <- 
  "Tasa de política monetaria TEA"
nombres[12:44] <- str_remove_all(nombres[12:44], " TEA")
nombres[4:5] <- str_remove_all(nombres[4:5], " FCI \\(9\\)")
names(df) <- nombres
df <- df[-c(1:5),] %>% formatear()
df$`Tasas de Interés LEBAC en Pesos / LELIQ (11) 6 M`[df$Fecha>="2022-01-11"] <- 
  zoo::na.locf0(df$`Tasas de Interés LEBAC en Pesos / LELIQ (11) 6 M`[df$Fecha>="2022-01-11"])
INSTRUMENTOS_BCRA <- df

names(BASE_MONETARIA)[2:ncol(BASE_MONETARIA)] <- paste("BASE_MONETARIA",names(BASE_MONETARIA))[2:ncol(BASE_MONETARIA)]
names(RESERVAS)[2:ncol(RESERVAS)] <- paste("RESERVAS",names(RESERVAS))[2:ncol(RESERVAS)]
names(DEPOSITOS)[2:ncol(DEPOSITOS)] <- paste("DEPOSITOS",names(DEPOSITOS))[2:ncol(DEPOSITOS)]
names(PRESTAMOS)[2:ncol(PRESTAMOS)] <- paste("PRESTAMOS",names(PRESTAMOS))[2:ncol(PRESTAMOS)]
names(TASAS_DE_MERCADO)[2:ncol(TASAS_DE_MERCADO)] <- paste("TASAS_DE_MERCADO",names(TASAS_DE_MERCADO))[2:ncol(TASAS_DE_MERCADO)]
names(INSTRUMENTOS_BCRA)[2:ncol(INSTRUMENTOS_BCRA)] <- paste("INSTRUMENTOS_BCRA",names(INSTRUMENTOS_BCRA))[2:ncol(INSTRUMENTOS_BCRA)]

SERIESE <- full_join(BASE_MONETARIA, RESERVAS, by="Fecha")
SERIESE <- full_join(SERIESE, DEPOSITOS, by="Fecha")
SERIESE <- full_join(SERIESE, PRESTAMOS, by="Fecha")
SERIESE <- full_join(SERIESE, TASAS_DE_MERCADO, by="Fecha")
SERIESE <- full_join(SERIESE, INSTRUMENTOS_BCRA, by="Fecha")
SERIESE <- arrange(SERIESE,Fecha)

SERIESE$LEGAR_LEMIN <- SERIESE$`INSTRUMENTOS_BCRA Saldos LEBAC y NOBAC en Pesos, LEGAR y LEMIN (incluye ajustables por Tipo de Cambio; no incluye LELIQ) (3) Total (2)`
SERIESE$LEGAR_LEMIN[SERIESE$Fecha<"2021-02-02"] <- 0
SERIESE$`INSTRUMENTOS_BCRA Saldos LEBAC y NOBAC en Pesos, LEGAR y LEMIN (incluye ajustables por Tipo de Cambio; no incluye LELIQ) (3) Total (2)`[SERIESE$Fecha>"2021-02-02"] <- 0

save(SERIESE,file=paste0(dir_datos,"series.Rda"))

