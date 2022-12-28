m2soja <- SERIESE |> filter(Fecha>="2022-11-25") |> 
  select(`BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Sector Privado y Otros`,
         `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Tesoro Nacional`,
         `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otros`,
         `DEPOSITOS Pasivos Totales en Pesos (1) Total Depósitos`,
         `DEPOSITOS Pasivos Totales en Pesos (1) Cuenta Corriente (2)`,
         `DEPOSITOS Pasivos Totales en Pesos (1) Caja de Ahorros`,
         `PRESTAMOS Préstamos al Sector Privado TotalPesos`,
         `BASE_MONETARIA Variaciones Diarias Base Monetaria Circulación Monetaria Billetes y Monedas en Poder del Público(1)`,
         `DEPOSITOS Millones de Pesos M2`) |> 
  mutate("Compra de Reservas" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Sector Privado y Otros` +
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Tesoro Nacional`,
         "Desarme PF" = `DEPOSITOS Pasivos Totales en Pesos (1) Total Depósitos` -
           `DEPOSITOS Pasivos Totales en Pesos (1) Cuenta Corriente (2)` -
           `DEPOSITOS Pasivos Totales en Pesos (1) Caja de Ahorros`) |> 
  rename("Compra de Bonos" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otros`,
         "Préstamos al S.Priv." = `PRESTAMOS Préstamos al Sector Privado TotalPesos`,
         "Circulante" = `BASE_MONETARIA Variaciones Diarias Base Monetaria Circulación Monetaria Billetes y Monedas en Poder del Público(1)`,
         "M2" = `DEPOSITOS Millones de Pesos M2`) |> 
  select("Compra de Reservas","Compra de Bonos","Desarme PF","Préstamos al S.Priv.","Circulante","M2") |> 
  filter(!is.na(M2))

m2soja$`Compra de Reservas`[1] <- 0
m2soja$`Compra de Reservas` <- cumsum(m2soja$`Compra de Reservas`)
m2soja$`Compra de Bonos`[1] <- 0
m2soja$`Compra de Bonos` <- cumsum(m2soja$`Compra de Bonos`)
m2soja$`Desarme PF` <- m2soja$`Desarme PF`[1]-m2soja$`Desarme PF`
m2soja$`Préstamos al S.Priv.` <- m2soja$`Préstamos al S.Priv.`-m2soja$`Préstamos al S.Priv.`[1]
m2soja$Circulante[1] <- 0
m2soja$Circulante <- cumsum(m2soja$Circulante)
m2soja$M2 <- m2soja$M2-m2soja$M2[1]
m2soja$t <- seq(0,nrow(m2soja)-1,1)

m2soja <- m2soja |> pivot_longer(-t, names_to = "Factor", values_to = "Valor")
m2soja$Factor <- factor(m2soja$Factor, levels = c("M2","Circulante",
                                                  "Desarme PF","Préstamos al S.Priv.",
                                                  "Compra de Bonos","Compra de Reservas"))

m2soja_g <- ggplot(subset(m2soja,Factor!="M2"), 
                   aes(x=t,y=Valor,fill=Factor)) +
  geom_area() +
  geom_line(data=subset(m2soja,Factor=="M2"), color="blue", size=1)


