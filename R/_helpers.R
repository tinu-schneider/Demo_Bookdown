





gib_formatierten_wert_von <- function(daten, szenario, jahr) {
    wert <- .gib_wert_von(daten, szenario, jahr)
    wert <- prettyNum(wert, big.mark = "'")
    wert
}
    
.gib_wert_von <- function(daten, szenario, jahr) {
    wert <- daten %>% filter(Szenario == szenario, Jahr == jahr) %>% pull()
    if (length(wert) == 0) stop("Es gibt keine Wert zu dieser Abfrage")
    wert
}
# gib_formatierten_wert_von(res, "Referenz", 2015) %>%  print()
# gib_formatierten_wert_von(res, "halo", 1000) 
