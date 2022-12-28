factores_BM <- function(fecha) {
  graf1 <- SERIESE |> filter(Fecha>=fecha) |> 
    select(`BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Total`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Sector Privado y Otros`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Tesoro Nacional`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otras Operaciones con el Tesoro Nacional Adelantos Transitorios`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otras Operaciones con el Tesoro Nacional Transferencia de Utilidades`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otras Operaciones con el Tesoro Nacional Resto`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos Pases`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos LELIQ y NOTALIQ`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos Redescuentos y Adelantos`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos Intereses, Primas y Remun. Ctas. Ctes.`,
           `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otros`) |> 
    rename("Total" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Total`,
           "Intereses" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos Intereses, Primas y Remun. Ctas. Ctes.`,
           "OMAs y otros" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otros`) |> 
    mutate("Compra de Reservas" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Sector Privado y Otros` +
             `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Compras Netas de Divisas Al Tesoro Nacional`,
           "Financiamiento al Gobierno" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otras Operaciones con el Tesoro Nacional Adelantos Transitorios` +
             `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otras Operaciones con el Tesoro Nacional Transferencia de Utilidades` +
             `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Otras Operaciones con el Tesoro Nacional Resto`,
           "LELIQ/Pases" = `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos Pases` +
             `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos LELIQ y NOTALIQ` +
             `BASE_MONETARIA Variaciones Diarias Factores de Explicación de la Base Monetaria Pases, LELIQ, NOTALIQ y Redescuentos Redescuentos y Adelantos`) |> 
    select("Total", "Compra de Reservas", "Financiamiento al Gobierno", "LELIQ/Pases", "Intereses", "OMAs y otros") |> 
    summarise(across(everything(), sum, na.rm = TRUE)) |> 
    pivot_longer(everything(), names_to = "Factor", values_to = "Valor") |> 
    mutate(Factor = factor(Factor, levels=c("Total","Compra de Reservas", "Financiamiento al Gobierno",
                                            "Intereses", "OMAs y otros","LELIQ/Pases"),
                           labels = c("Total", "Compra\nde\nReservas", "Financiamiento\nal\nGobierno",
                                      "Intereses", "OMAs\ny\notros","LELIQ/Pases"))) |> 
    ggplot(aes(x=Factor, y=Valor, fill=Factor, label = round(Valor,0))) +
    geom_col(color="black") +
    # scale_y_continuous(labels = scales::comma_format(big.mark = ".",
    #                                          decimal.mark = ",")) +
    scale_fill_manual(values = c("blue",rep("lightblue",5))) +
    geom_label(fill="white") +
    labs(x="",y="Millones de Pesos") +
    theme(panel.background = element_blank(),
          legend.position = "null",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text = element_text(size = 12))
  graf1
}

