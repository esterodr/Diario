load(paste0(dir_datos,"Pefil_pasivos_remunerados.Rda"))

descargar_archivo("https://www.bcra.gob.ar/Pdfs/PoliticaMonetaria/Infopases.pdf","Infopases.pdf")

texto <- pdf_text(paste0(dir_archivos,"Infopases.pdf"))
fecha <- str_extract(texto,"[0-9]{1,2} de [a-z]+ de [0-9]{4}")
dia <- str_extract(fecha,"^[0-9]+")
anio <- str_extract(fecha,"[0-9]+$")
mes <- unlist(str_split(fecha," "))[3]
mes <- case_when(mes == "enero" ~ "01",
                 mes == "febrero" ~ "02",
                 mes == "marzo" ~ "03",
                 mes == "abril" ~ "04",
                 mes == "mayo" ~ "05",
                 mes == "junio" ~ "06",
                 mes == "julio" ~ "07",
                 mes == "agosto" ~ "08",
                 mes == "septiembre" ~ "09",
                 mes == "octubre" ~ "10",
                 mes == "noviembre" ~ "11",
                 mes == "diciembre" ~ "12",)
fecha <- paste(anio,mes,dia,sep="-")

file.copy(paste0(dir_archivos,"Infopases.pdf"), dir_pases)
file.rename(paste0(dir_pases,"Infopases.pdf"),paste0(dir_pases,"Infopases_",fecha,".pdf"))

cuadro1 <- str_split(str_sub(texto,str_locate(texto, 
                                              "Cuadro I ")[2],str_locate(texto, 
                                                                         "Cuadro II ")[1]),"\n")
cuadro1 <- unlist(str_split(unlist(cuadro1)[4],"\\s+"))
publicos <- as.numeric(str_remove_all(cuadro1[which(str_detect(cuadro1,"%"))[1]+1],","))
privados <- as.numeric(str_remove_all(cuadro1[which(str_detect(cuadro1,"%"))[1]+2],","))

df <- tibble(Fecha = rep(as.Date(fecha),2),
             Instrumento = rep("Pases",2),
             Accion = rep("Suscripción",2),
             Ente = c("Públicos","Privados"),
             Monto = c(publicos,privados))

perfil_pas_rem <- rbind(perfil_pas_rem,df)
perfil_pas_rem <- perfil_pas_rem[!duplicated(perfil_pas_rem),]

cuadro1 <- str_split(str_sub(texto,str_locate(texto, 
                                              "Cuadro V ")[2],str_locate(texto, 
                                                                         "Cuadro VII ")[1]),"\n")
cuadro1 <- unlist(str_split(unlist(cuadro1)[4],"\\s+"))
fecha2 <- cuadro1[2]
anio <- paste0("20",str_extract(fecha2,"^[0-9]+"))
dia <- str_extract(fecha2,"[0-9]+$")
mes <- unlist(str_split(fecha2,"-"))[2]
mes <- case_when(mes == "ene" ~ "01",
                 mes == "feb" ~ "02",
                 mes == "mar" ~ "03",
                 mes == "abr" ~ "04",
                 mes == "may" ~ "05",
                 mes == "jun" ~ "06",
                 mes == "jul" ~ "07",
                 mes == "ago" ~ "08",
                 mes == "sep" ~ "09",
                 mes == "oct" ~ "10",
                 mes == "nov" ~ "11",
                 mes == "dic" ~ "12",)
fecha2 <- paste(anio,mes,dia,sep="-")
publicos <- as.numeric(str_remove_all(cuadro1[3],","))
privados <- as.numeric(str_remove_all(cuadro1[4],","))

df <- tibble(Fecha = rep(as.Date(fecha2),2),
             Instrumento = rep("Pases",2),
             Accion = rep("Vencimiento",2),
             Ente = c("Públicos","Privados"),
             Monto = c(publicos,privados))

perfil_pas_rem <- rbind(perfil_pas_rem,df)
perfil_pas_rem <- perfil_pas_rem[!duplicated(perfil_pas_rem),]

cuadro1 <- str_split(str_sub(texto,str_locate(texto, 
                                              "Cuadro IX ")[2],str_locate(texto, 
                                                                         "Gráfico III ")[1]),"\n")
cuadro1 <- unlist(cuadro1)[str_detect(trimws(unlist(cuadro1)),
                            "[0-9]+-[a-z]{3}-[0-9]{2}")]
leliq_ind <- which(lengths(str_extract_all(cuadro1,"[0-9]+-[a-z]{3}-[0-9]{2}"))>1)

cuadro_leliq <- unlist(str_split(trimws(cuadro1[leliq_ind]),"\\s+"))
fechas <- cuadro_leliq[seq(1,length(cuadro_leliq),8)]
fechas <- as.Date(str_replace_all(fechas, c(ene="01", feb="02", mar="03", abr="04",
                          may="05", jun="06", jul="07", ago="08",
                          sep="09", oct="10", nov="11", dic="12")), format="%d-%m-%y")
publicos <- as.numeric(str_remove_all(cuadro_leliq[seq(2,length(cuadro_leliq),8)],","))
privados <- as.numeric(str_remove_all(cuadro_leliq[seq(3,length(cuadro_leliq),8)],","))

df <- tibble(Fecha = c(fechas,fechas),
             Instrumento = rep("LELIQ",2*length(fechas)),
             Accion = rep("Vencimiento",2*length(fechas)),
             Ente = c(rep("Públicos",length(fechas)),rep("Privados",length(fechas))),
             Monto = c(publicos,privados))

perfil_pas_rem <- rbind(perfil_pas_rem,df)
perfil_pas_rem <- perfil_pas_rem[!duplicated(perfil_pas_rem),]

fechas <- cuadro_leliq[seq(5,length(cuadro_leliq),8)]
fechas <- as.Date(str_replace_all(fechas, c(ene="01", feb="02", mar="03", abr="04",
                                            may="05", jun="06", jul="07", ago="08",
                                            sep="09", oct="10", nov="11", dic="12")), format="%d-%m-%y")
publicos <- as.numeric(str_remove_all(cuadro_leliq[seq(6,length(cuadro_leliq),8)],","))
privados <- as.numeric(str_remove_all(cuadro_leliq[seq(7,length(cuadro_leliq),8)],","))

cuadro_notaleliq <- unlist(str_split(trimws(cuadro1[-leliq_ind]),"\\s+"))
leliq_ind <- which(str_detect(cuadro_notaleliq,"[0-9]+-[a-z]{3}-[0-9]{2}"))

fechas2 <- cuadro_notaleliq[leliq_ind]
fechas2 <- as.Date(str_replace_all(fechas2, c(ene="01", feb="02", mar="03", abr="04",
                                            may="05", jun="06", jul="07", ago="08",
                                            sep="09", oct="10", nov="11", dic="12")), format="%d-%m-%y")
publicos2 <- as.numeric(str_remove_all(cuadro_notaleliq[leliq_ind+1],","))
privados2 <- as.numeric(str_remove_all(cuadro_notaleliq[leliq_ind+2],","))

fechas <- c(fechas,fechas2)
publicos <- c(publicos,publicos2)
privados <- c(privados,privados2)

df <- tibble(Fecha = c(fechas,fechas),
             Instrumento = rep("NOTALIQ",2*length(fechas)),
             Accion = rep("Vencimiento",2*length(fechas)),
             Ente = c(rep("Públicos",length(fechas)),rep("Privados",length(fechas))),
             Monto = c(publicos,privados)) |> arrange(Fecha)

perfil_pas_rem <- rbind(perfil_pas_rem,df)
perfil_pas_rem <- perfil_pas_rem[!duplicated(perfil_pas_rem),]

save(perfil_pas_rem, file = paste0(dir_datos,"Pefil_pasivos_remunerados.Rda"))

rm(df,anio,cuadro1,cuadro_leliq,cuadro_notaleliq,dia,fecha,fecha2,fechas,fechas2,
   leliq_ind,mes,privados,privados2,publicos,publicos2,texto)
