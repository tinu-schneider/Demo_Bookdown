

library(testthat)
source("CONF.R")
source("daten_einlesen.R")
source("daten_aufbereiten.R")
source("_helpers.R")




pfad_szen <- "../Daten_aufbereitet/je-d-01.03.02.01.xls"
pfad_dat  <- "../Daten_aufbereitet/su-d-01.02.04.04.xlsx"

szen <- szenarien_einlesen(pfad_szen)
dat  <- daten_einlesen(pfad_dat)
res  <- daten_bereitstellen(dat, szen)



context("Daten einlesen") 
test_that("Die Daten werden richtig eingelesen", {
    expect_error(.daten_einlesen(paste0(pfad_szen, "falsch.xls"), 1))
    expect_true(length(szen) ==  3)
    expect_true(length(dat)  == 26) # 1991:2016
})



context("Die Daten aufbereiten")
test_that("Die Daten sind richtig aufbereitet", {
    expect_equal(dim(res), c(165, 3))
    expect_false(any(is.na(res)))
    
    ref <- res %>% filter(Szenario == "Referenz")
    expect_equal(unique(ref$Jahr), 1991:2045)
    expect_false(any(duplicated(ref$Jahr)))
})





context("Helpers")
test_that("Formatierte Werte sind ok", {
    expect_equal(gib_formatierten_wert_von(res, "Referenz", 2014),  "8'139'631") # Daten
    expect_equal(gib_formatierten_wert_von(res, "Referenz", 2015),  "8'339'505") # Szenarien
    expect_equal(gib_formatierten_wert_von(res, "Referenz", 2045), "10'176'124")
    
    expect_error(gib_formatierten_wert_von(res, "halo", 1000))
})


