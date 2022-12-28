load(paste0(dir_datos,"Pefil_pasivos_remunerados.Rda"))

perfil_pas_rem_G <- perfil_pas_rem |> filter(Instrumento %in% c("LELIQ","NOTALIQ")) |> 
  ggplot(aes(x=Fecha,y=Monto,fill=Ente)) + 
  geom_col() +
  labs(x="",y="Millones de pesos") +
  theme(legend.title = element_blank(),
        panel.background = element_blank())