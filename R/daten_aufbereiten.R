

#' Die Daten und Szenarien zusammenfügen
#' 
#' @param dat Die eingelesenen Daten (Liste)
#' @param szen Die eingelesenen Szenarien (Liste)
#' 
#' @return Ein Tibble mit den Daten und Szenarien, drei Spalten (Szenario, Jahr, Wert)
#' 
daten_bereitstellen <- function(dat, szen) {
    daten <- .daten_aufbereiten(dat)
    szenarien <- .szenarien_aufbereiten(szen)
    
    # Daten vorbereiten
    szen_spalte <- rep(CONF$sheets$szenarien, each = nrow(daten) )
    dat_lang <- bind_rows(daten, daten)
    dat_lang <- bind_rows(dat_lang, daten)
    dat_lang$Szenario <- factor(szen_spalte, levels = CONF$sheets$szenarien)
                
    # Alles zusammenfügen
    res <- szenarien %>% 
            group_by(Szenario) %>% 
                bind_rows(dat_lang) %>%
                ungroup() %>% 
            arrange(Szenario, Jahr) 
    res
}



#' Die historischen Daten der Schweiz aufbereiten
#' 
#' @param dat Die eingelesenen Daten (eine Liste)
#' @param max_jahr (2015) Nur Daten kleiner als dieses Jahr zurückgeben, anschliessend Szenarien
#' @retur Ein tibble mit den Daten der Schweiz
#' 
.daten_aufbereiten <- function(dat) {
    jahre <- CONF$sheets$daten
    werte <- map_dbl(dat, ~ .x[[1, 2]])  # beachte '~' für functionx(x) x[[1, 2]]
    
    res <- tibble(Jahr = jahre, Wert = werte) %>% 
           filter(Jahr < CONF$daten_grenze)
    res
}


#' Die Szenarien für die Schweiz aufbereiten
#' 
#' @param dat Die eingelesenen Daten (eine Liste)
#' @retur Ein tibble mit den Szenarien der Schweiz
#' 
.szenarien_aufbereiten <- function(dat) {
    szen <- CONF$sheets$szenarien
    
    res <- map_df(dat, ~ .x[1, ])
    
    colnames(res)[1] <- "Szenario" # Erste Spalte umbenennen
    res[, 1] <- szen   # Namen der Szenarien einfügen
    
    res <- 
        res %>% 
        gather(key = Jahr, value = Wert, -Szenario) %>% 
        mutate(Szenario = factor(Szenario, levels = szen)) %>% 
        mutate(Wert = suppressWarnings(as.numeric(Wert))) %>% 
        mutate(Jahr = suppressWarnings(as.numeric(Jahr))) %>% 
        na.omit() %>% 
        filter(Jahr >= CONF$daten_grenze) %>% 
        arrange(Szenario)
    res
}





# source("CONF.R")
# pfad_dat <- "../Daten_aufbereitet/su-d-01.02.04.04.xlsx"
# dat <- daten_einlesen(pfad_dat)
# dat_aufbereitet <- .daten_aufbereiten(dat) %>% plot()

# pfad_szen <- "../Daten_aufbereitet/je-d-01.03.02.01.xls"
# szen <- szenarien_einlesen(pfad_szen)
# szen_aufbereitet <- .szenarien_aufbereiten(szen) %>% print(n = 10000)

# daten_bereitstellen(dat, szen) %>% print(n = 1000) %>%  plot()


