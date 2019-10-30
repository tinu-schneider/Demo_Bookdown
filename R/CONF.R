

# Die globale Konfiguration für de Analyse

# testthat::test_file("tests.R")

suppressPackageStartupMessages(library(tidyverse))
library(readxl)
library(ggiraph)
library(widgetframe)


CONF <- list(
        jahre = list(
            daten = 1991:2016,
            szenarien = 2015:2045
        ),
        sheets = list(
            szenarien = c("Referenz", "Hoch", "Tief"),
            daten = 1991:2016
        ),
        skip_zeilen = list(
            szenarien = 2, 
            daten = 7
        ),
        daten_range = "A7:B38", 
        daten_grenze = 2015, 
        farben = c(
            Hoch     = "#E27D60",
            Referenz = "#41B3A3",
            Tief     = "#085DCB"
        )
)





