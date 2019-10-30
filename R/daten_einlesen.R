

#' Die historischen Daten einlesen
#' 
#' @param pfad Der Pfad zur Datei inkl. Dateiname und Endung
#' @return Eine Liste mit den Tabellen (eine pro Jahr) als tibble
#' 
daten_einlesen <- function(pfad) {
    sheets <- as.character(CONF$sheets$daten)
    range  <- CONF$daten_range
    map(sheets, .sheet_einlesen, pfad, range = range)
}


#' Die Szenarien einlesen
#' 
#' @param pfad Der Pfad zur Datei inkl. Dateiname und Endung
#' @return Eine Liste mit den drei Szenarien als tibble
#' 
szenarien_einlesen <- function(pfad) {
    sheets <- CONF$sheets$szenarien
    skip   <- CONF$skip_zeilen$szenarien
    map(sheets, .sheet_einlesen, pfad, skip = skip)
}


#' Ein Excel-Sheet einlesen
#' 
#' @param sheet Der Name des Excel-Sheets
#' @param pfad Der volle Pfad inkl. Dateiname
#' @param ... Zusätzliche benannte Argumente 
#' 
#' @return Ein tibble mit den Daten
#' 
.sheet_einlesen <- function(sheet, pfad, ...) {
    read_excel(pfad, sheet = sheet, ...)
}



# source("CONF.R")
# pfad_dat <- "../Daten_aufbereitet/su-d-01.02.04.04.xlsx"
# dat <- daten_einlesen(pfad_dat)
# pfad_szen <- "../Daten_aufbereitet/je-d-01.03.02.01.xls"
# szen <- szenarien_einlesen(pfad_szen)






