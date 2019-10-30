


#' Plotte die Daten 
#' 
#' @param dat Die aufbereiteten Daten
#' @param interactive (FALSE) Bool
#' @param hoehe Die Höhe der interaktiven Grafik (kleiner als fig.heigth)
#' 
#' @return invisible 
#' 
plot_chart <- function(dat, interactive = FALSE, hoehe = 2.5) {
    p <- .prep_plot_linien(dat)
    
    if (interactive) {
        t_css <- paste0("background-color: lightgray;
                        font-family: Roboto, Arial, Helvetica, sans-serif;
                        padding: 10px;
                        border-radius: 5px;")
        pp <- ggiraph(code = {print(p)},
                      tooltip_opacity = 0.85,
                      tooltip_extra_css = t_css,
                      tooltip_offx = -60,
                      tooltip_offy = -70, 
                      pointsize = 11,
                      height = hoehe,
                      width  = 1.0
        )
        # print(pp)
        frameWidget(pp, width = "100%")
    } else {
        plot(p)
    }
}


# Den plot erstellen (inkl. Daten anpassen etc.)
.prep_plot_linien <- function(daten) {
    dat <- .add_tt(daten) %>% 
        mutate(Wert = Wert / 1e6) %>% 
        mutate(Szenario = fct_relevel(Szenario, c("Hoch", "Referenz", "Tief")))
    
    dat_hist <- dat %>% 
        filter(Szenario == "Referenz", Jahr < CONF$daten_grenze)
    
    p <- ggplot(dat, aes(x = Jahr, 
                         y = Wert, 
                         group = Szenario, 
                         color = Szenario)
                ) +
            geom_line(size = 1.0) + 
            geom_line(data = dat_hist, aes(x = Jahr, y = Wert), 
                      color = "darkgray", size = 1.5) + 
            geom_point_interactive(aes(tooltip = TT), 
                                   alpha = 0.01, 
                                   color = "white",
                                   size = 6) +
            scale_colour_manual(values = CONF$farben) +
        labs(x = NULL, 
             y = NULL, 
             title    = "Wohnbev\u00F6lkerung in der Schweiz",
             subtitle = "Mio. Einwohner", 
             caption  = "Quelle: Daten und Szenarien BfS") +
        theme_tsd(base_size = 10) 
    
    p
}


# Tooltips erstellen
.add_tt <- function(dat) {
    dat %>% 
        mutate(Wert_f = prettyNum(Wert, big.mark = "`")) %>% 
        mutate(TT = ifelse(Jahr < CONF$daten_grenze, 
                                        paste0("Daten\n", Jahr, ": ", Wert_f), 
                                        paste0("Szenario ", Szenario, "\n", 
                                              Jahr, ": ", Wert_f)))
}



# source("CONF.R")
# pfad_dat <- "../Daten_aufbereitet/su-d-01.02.04.04.xlsx"
# pfad_szen <- "../Daten_aufbereitet/je-d-01.03.02.01.xls"
# dat <- daten_einlesen(pfad_dat)
# szen <- szenarien_einlesen(pfad_szen)
# res <- daten_bereitstellen(dat, szen)
# plot_chart(res)
# plot_chart(res, interactive = TRUE)



