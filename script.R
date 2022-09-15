#bachelor 20200227
#import dataset

# Innlasting av pakker ----------------------------------------------------

if (!require("tidyverse"))
  install.packages("tidyverse")
library(tidyverse)

if (!require("essurvey"))
  install.packages("essurvey")
library(essurvey)

if (!require("lme4"))
  install.packages("lme4")
library(lme4)

if (!require("multilevel"))
  install.packages("multilevel")
library(multilevel)

if (!require("car"))
  install.packages("car")
library(car)


# Innlastning av ESS-datasett ---------------------------------------------


#Endre til ditt working directory her
setwd("~/Bachelor/Statistikk/wd")

# importering av ESS9 eller og andre versjoner av ESS 1-9.

# ESS krever en e-postadresse for innlogging.
essurvey::set_email("martin.s.mjelde@ntnu.no")

ESS9 <- essurvey::import_rounds(9)

# Vi endrer variabelen kjønn til å være en factor for lettere prosessering.

# Jeg opplevde også litt problemer med SPSS/STATA-formatet i R. Jeg valgte
# derfor å forsøke å gjøre det mest mulig kompatibelt ved å fjerne
# klassifiseringen der spørsmålsforklaring også var inkludert i variablene.

ESS9[] <- lapply(ESS9, unclass)



# Dette er et veldig stort datasett, det vil ta litt tid å åpne.

# For en samling av alle andre datasett som brukes her og de jeg har konvertert til .Rdata,
# altså kombinert datasett for ESS 1-8 og ESS 8 og ESS 7 med bakgrunnvariabler, se:
# https://studntnu-my.sharepoint.com/:f:/g/personal/martinmj_ntnu_no/Eq5tZxab9iRLipYBaq3gLa0BctG154S1GtAteEhq36NKSQ?e=pVgYny
# Det er en lenke til min OneDrive-konto med en kopi av datasettene.

# ESS1_8e01 <- read_dta("~/Bachelor/Statistikk/Datasett/ESS1-8e01_stata/ESS1-8e01.dta")

# save(ESS1_8e01, file = "ESS1_8.Rdata")

load("~/Bachelor/Statistikk/wd/ESS1_8.Rdata")

# Landgrupper -------------------------------------------------------------

# Deretter lager vi landgruppene

# Skandinavia regner jeg her som Norge (NO), Sverige (SE), Danmark (DK), Finland
# (FI) og Island (IS)
Skandinavia <- c("NO", "SE", "DK", "FI", "IS")

# I følge SNL regnes disse landene som Vest-Europa: Belgium (BE), France
# (FR), Ireland (IE), Luxembourg (LU), Monaco (ikke med), Netherlands (NL),
# United Kingdom (GN).

Vest_Europa <- c("GB", "FR", "NL", "BE", "IE", "LU")

# Øst-Europa - , Hungary "HU", Poland "PL", Romania "RO", Serbia "RS",
# Slovakia "SK", Slovenia "SI", Bosnia and Herzegovina (NA), Bulgaria "BG",
# "Russia "RU", Ukraine "UA"

Øst_Europa <-
  c("HU", "PL", "RO", "RS", "SK", "SI", "BG", "RU", "UA")

# Baltikum - Estonia "EE", Latvia (NA) og Lithuania "LT"

Baltikum <- c("EE", "LT")

# Sentraleuropa - Austria "AT", Germany "DE", Lichtenstein (NA), Switzerland
# "CH", Czech Republic "CZ"

Sentral_Europa <- c("AT", "DE", "CH", "CZ")

# Sør-Europa - Italy "IT", Croatia "HR", Turkey "TR", Cyprus "CY", Greece
# "GR", Spain "ES", Portugal "PT", Andorra

Sør_Europa <- c("IT", "HR", "TR", "CY", "GR", "ES", "PT")

# Deretter lager vi en ny variabel med verdiene 1 dersom det er skandinavia og 0
# hvis ikke. Deretter endres verdien 1 til "skandinavia".

# En kopi av land-variabelen
ESS9$landgrupper <- ESS9$cntry

ESS9$landgrupper[ESS9$landgrupper %in% Skandinavia] <- "Skandinavia"
ESS9$landgrupper[ESS9$landgrupper %in% Vest_Europa] <- "Vest-Europa"
ESS9$landgrupper[ESS9$landgrupper %in% Øst_Europa] <- "Øst-Europa"
ESS9$landgrupper[ESS9$landgrupper %in% Baltikum] <- "Baltikum"
ESS9$landgrupper[ESS9$landgrupper %in% Sentral_Europa] <-
  "Sentral-Europa"
ESS9$landgrupper[ESS9$landgrupper %in% Sør_Europa] <- "Sør-Europa"

ESS9$landgrupper <- factor(ESS9$landgrupper)

# Samme for ESS 1-8
ESS1_8e01$landgrupper <- ESS1_8e01$cntry

ESS1_8e01$landgrupper[ESS1_8e01$landgrupper %in% Skandinavia] <-
  "Skandinavia"
ESS1_8e01$landgrupper[ESS1_8e01$landgrupper %in% Vest_Europa] <-
  "Vest-Europa"
ESS1_8e01$landgrupper[ESS1_8e01$landgrupper %in% Øst_Europa] <-
  "Øst-Europa"
ESS1_8e01$landgrupper[ESS1_8e01$landgrupper %in% Baltikum] <-
  "Baltikum"
ESS1_8e01$landgrupper[ESS1_8e01$landgrupper %in% Sentral_Europa] <-
  "Sentral-Europa"
ESS1_8e01$landgrupper[ESS1_8e01$landgrupper %in% Sør_Europa] <-
  "Sør-Europa"

ESS1_8e01$landgrupper <- factor(ESS1_8e01$landgrupper)

# Deretter velger jeg å fjerne Israel "IL", som ikke er en del av Europa

ESS1_8e01[ESS1_8e01$cntry == "IL" ,] <- NA

# For å ha det mest mulig ryddig i arbeidsområdet sletter vi listene for landgruppene.
rm(Skandinavia, Vest_Europa, Sentral_Europa, Sør_Europa, Sør_Øst_Europa, Øst_Europa, Baltikum)

# Innlasting av datasett fra World Bank - GDP -----------------------------

# Datasett hentet fra World Bank, tilgjengeliggjøres på OneDrive med de andre datasettene.
library(readxl)
GDP_worldbank <-
  read_excel("~/Bachelor/Statistikk/Datasett/GDP_worldbank.xls",
             skip = 2)

population <- read_tsv(file = "Datasett/tps00001.tsv")


# GDP per capita ----------------------------------------------------------

# GDP for ESS1_8e01. Her endrer vi til 2016, for å reflektere at ESS8 kom ut da.


ESS1_8e01$gdp <- NA

ESS1_8e01$gdp[ESS1_8e01$cntry == "AT"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Austria"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,AT"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "BE"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Belgium"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,BE"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "BG"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Bulgaria"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,BG"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "CH"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Switzerland"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,CH"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "CY"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Cyprus"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,CY"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "CZ"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Czech Republic"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,CZ"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "DE"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Germany"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,DE"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "DK"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Denmark"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,DK"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "EE"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Estonia"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,EE"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "ES"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Spain"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,ES"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "FI"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Finland"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,FI"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "FR"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "France"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,FR"])

# Her er det tilsynelatende en feil i datasettet. Output fra
# as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,FR"])
# er NA, siden det står ""66918941 p"" Vi endrer det til 66918941

population$`2016`[population$`indic_de,geo\\time` == "JAN,FR"]

population$`2016`[population$`indic_de,geo\\time` == "JAN,FR"] <-
  66918941

ESS1_8e01$gdp[ESS1_8e01$cntry == "FR"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "France"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,FR"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "GB"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "United Kingdom"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,UK"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "GR"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Greece"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,EL"])

# Her er ikke Hellas "GR", men "EL", siden Eurostat går etter landnavn på
# nasjonalspråket deres. Hellas er da Ellinikí Dimokratía

ESS1_8e01$gdp[ESS1_8e01$cntry == "HR"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Croatia"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,HR"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "HU"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Hungary"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,HU"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "IE"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Ireland"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,IE"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "IS"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Iceland"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,IS"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "IT"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Italy"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,IT"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "LT"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Lithuania"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,LT"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "LU"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Luxembourg"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,LU"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "NL"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Netherlands"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,NL"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "NO"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Norway"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,NO"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "PL"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Poland"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,PL"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "PT"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Portugal"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,PT"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "RU"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Russian Federation"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,RU"])

# Russland har ikke data fra 2016 for population i Eurostat.Det 
# må derfor hente det manuelt fra World Bank.

ESS1_8e01$gdp[ESS1_8e01$cntry == "RU"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Russian Federation"] / 144478050

ESS1_8e01$gdp[ESS1_8e01$cntry == "SE"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Sweden"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,SE"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "SI"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Slovenia"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,SI"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "SK"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Slovak Republic"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,SK"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "TR"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Turkey"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,TR"])

ESS1_8e01$gdp[ESS1_8e01$cntry == "UA"] <-
  GDP_worldbank$`2016`[GDP_worldbank$`Country Name` == "Ukraine"] /
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,UA"])

ESS1_8e01$gdp <- as.numeric(ESS1_8e01$gdp)

table(ESS1_8e01$cntry[is.na(ESS1_8e01$gdp)], useNA = "always")

# Her får vi opp 14910 NAs, det er Israel, cntry "IL". De er markert med NA siden
# de ble fjernet tidligere.

ESS1_8e01$gdp <- ESS1_8e01$gdp / 1000


# GDP for ESS9

ESS9$gdp <- NA

ESS9$gdp[ESS9$cntry == "AT"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Austria"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,AT"])

ESS9$gdp[ESS9$cntry == "BE"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Belgium"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,BE"])

ESS9$gdp[ESS9$cntry == "BG"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Bulgaria"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,BG"])

# Bulgaria var ikke med i ESS8 og vi har derfor ikke deres GDP fra 2018. Vi
# henter den manuelt fra imf.org [1] og National Statistical Institute [2] i Bulgaria.
# [1] https://s.ntnu.no/bulgariagdp og [2] https://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&language=en&pcode=tps00001&plugin=1

# Vi henter data om innbyggertall fra tps000001

ESS9$gdp[ESS9$cntry == "BG"] <- 9267383 / 7000039


ESS9$gdp[ESS9$cntry == "CH"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Switzerland"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,CH"])

ESS9$gdp[ESS9$cntry == "CY"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Cyprus"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,CY"])

ESS9$gdp[ESS9$cntry == "CZ"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Czech Republic"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,CZ"])

ESS9$gdp[ESS9$cntry == "DE"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Germany"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,DE"])

ESS9$gdp[ESS9$cntry == "EE"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Estonia"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,EE"])

ESS9$gdp[ESS9$cntry == "FI"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Finland"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,FI"])

ESS9$gdp[ESS9$cntry == "FR"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "France"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,FR"])

ESS9$gdp[ESS9$cntry == "GB"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "United Kingdom"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,UK"])

ESS9$gdp[ESS9$cntry == "HU"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Hungary"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,HU"])

ESS9$gdp[ESS9$cntry == "IE"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Ireland"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,IE"])

ESS9$gdp[ESS9$cntry == "IT"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Italy"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,IT"])

ESS9$gdp[ESS9$cntry == "NL"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Netherlands"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,NL"])

ESS9$gdp[ESS9$cntry == "NO"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Norway"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,NO"])

ESS9$gdp[ESS9$cntry == "PL"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Poland"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,PL"])

ESS9$gdp[ESS9$cntry == "RS"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Serbia"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,RS"])

ESS9$gdp[ESS9$cntry == "SI"] <-
  GDP_worldbank$`2018`[GDP_worldbank$`Country Name` == "Slovenia"] /
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,SI"])

# Vi sjekker at alt ble riktig:

ESS9$gdp <- as.numeric(ESS9$gdp)

table(ESS9$cntry[ESS9$gdp == 0], useNA = "always")

# Her er verdien 0, siden vi ikke har noen NA-verdier (som i ESS1_8e01 der vi
# fjernet Israel).

ESS9$gdp <- ESS9$gdp / 1000

# Vi lager en variabel for etnisk fragmentering på landsbasis. Enkelte land
# mangler. De landene som mangler blir satt til NA. Landene det gjelder er:
# ESS9: Serbia (RS).
# ESS1_8e01: Hellas (GR), Russland (RU), Tyrkia (TR), Ukraina (UA)

# Måten jeg lager variabelen for etnisk fragmentering eller fraktalinisering er
# inspirert av  Drazanova, Lenka, 2019, "Historical Index of Ethnic
# Fractionalisation Dataset.pdf", Historical Index of Ethnic Fractionalization
# Dataset (HIEF), https://doi.org/10.7910/DVN/4JQRCL/2N5YVI, Harvard Dataverse,
# V1. Det Drazanova sin formel gjør har vi forenklet, så det er en forenklet
# versjon av hennes formel grunnet denne oppgavens begrensede omfang.

etnisitet <- read_tsv(file = "Datasett/tps00178.tsv")


# Etnisk fragmentering oppsett ----------------------------------------------------

ESS1_8e01$etnisitet <- NA


ESS1_8e01$etnisitet[ESS1_8e01$cntry == "AT"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,AT"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "BE"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,BE"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "BG"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,BG"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "CH"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,CH"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "CY"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,CY"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "CZ"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,CZ"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "DE"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,DE"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "DK"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,DK"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "EE"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,EE"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "ES"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,ES"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "FI"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,FI"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "FR"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,FR"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "GB"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,UK"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "GR"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,EL"]

# Legg merke til at Hellas, "GR" i ESS er "EL" i Eurostat, etter navnet på
# Hellas på gresk.

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "HR"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,HR"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "HU"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,HU"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "IE"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,IE"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "IS"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,IS"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "IT"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,IT"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "LT"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,LT"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "LU"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,LU"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "NL"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,NL"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "NO"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,NO"]

etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,PL"]

# endrer manuelt

etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,PL"] <- 626396

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "PL"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,PL"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "PT"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,PT"]

# Russland er ikke inkludert i Eurostat-datasettet og må derfor settes til NA.

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "RU"] <-
  NA

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "SE"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,SE"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "SI"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,SI"]

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "SK"] <-
  etnisitet$`2016`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,SK"]

# Tyrkia er heller ikke med. Settes derfor til NA.

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "TR"] <- NA

# Også Ungarn mangler. Settes derfor til NA.

ESS1_8e01$etnisitet[ESS1_8e01$cntry == "UA"] <- NA

table(ESS1_8e01$cntry[is.na(ESS1_8e01$etnisitet) == TRUE])

# Det har kommet med quotes rundt tallene og vi fjerner dem ved
# å endre til numeriske verdier i de aktuelle cellene.

ESS1_8e01$etnisitet <- as.numeric(ESS1_8e01$etnisitet)

# Samme for ESS9.

ESS9$etnisitet <- NA

ESS9$etnisitet[ESS9$cntry == "AT"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,AT"]

ESS9$etnisitet[ESS9$cntry == "BE"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,BE"]

ESS9$etnisitet[ESS9$cntry == "BG"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,BG"]

ESS9$etnisitet[ESS9$cntry == "CH"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,CH"]

ESS9$etnisitet[ESS9$cntry == "CY"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,CY"]

ESS9$etnisitet[ESS9$cntry == "CZ"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,CZ"]

ESS9$etnisitet[ESS9$cntry == "DE"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,DE"]

ESS9$etnisitet[ESS9$cntry == "EE"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,EE"]

ESS9$etnisitet[ESS9$cntry == "FI"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,FI"]

etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,FR"]

# Endrer manuelt.

etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,FR"] <- 8156208


ESS9$etnisitet[ESS9$cntry == "FR"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,FR"]

ESS9$etnisitet[ESS9$cntry == "GB"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,UK"]

ESS9$etnisitet[ESS9$cntry == "HU"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,HU"]

ESS9$etnisitet[ESS9$cntry == "IE"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,IE"]

ESS9$etnisitet[ESS9$cntry == "IT"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,IT"]

ESS9$etnisitet[ESS9$cntry == "NL"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,NL"]

ESS9$etnisitet[ESS9$cntry == "NO"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,NO"]

etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,PL"]

etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,PL"] <- 695850

# Også her må vi legge det til manuelt.

ESS9$etnisitet[ESS9$cntry == "PL"] <- etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,PL"]

ESS9$etnisitet[ESS9$cntry == "RS"] <-
  NA

ESS9$etnisitet[ESS9$cntry == "SI"] <-
  etnisitet$`2018`[etnisitet$`c_birth,age,unit,sex,geo\\time` == "FOR,TOTAL,NR,T,SI"]

table(ESS9$cntry[is.na(ESS9$etnisitet) == TRUE])

# Kun Serbia er NA. 

ESS9$etnisitet <- as.numeric(ESS9$etnisitet)

# Vi må også legge til innbyggertall


ESS1_8e01$innbyggere <- NA

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "AT"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,AT"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "BE"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,BE"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "BG"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,BG"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "CH"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,CH"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "CY"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,CY"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "CZ"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,CZ"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "DE"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,DE"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "DK"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,DK"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "EE"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,EE"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "ES"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,ES"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "FI"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,FI"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "FR"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,FR"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "GB"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,UK"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "GR"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,EL"])

# Her er ikke Hellas "GR", men "EL", siden Eurostat går etter landnavn på
# nasjonalspråket deres. Hellas er da Ellinikí Dimokratía

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "HR"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,HR"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "HU"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,HU"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "IE"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,IE"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "IS"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,IS"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "IT"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,IT"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "LT"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,LT"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "LU"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,LU"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "NL"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,NL"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "NO"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,NO"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "PL"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,PL"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "PT"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,PT"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "RU"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,RU"])

# Russland har ikke data fra 2016 for population i Eurostat.Det må derfor hente
# det manuelt fra World Bank.

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "RU"] <- 144478050

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "SE"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,SE"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "SI"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,SI"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "SK"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,SK"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "TR"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,TR"])

ESS1_8e01$innbyggere[ESS1_8e01$cntry == "UA"] <-
  as.numeric(population$`2016`[population$`indic_de,geo\\time` == "JAN,UA"])

ESS1_8e01$innbyggere <- as.numeric(ESS1_8e01$innbyggere)

table(ESS1_8e01$cntry[is.na(ESS1_8e01$innbyggere)], useNA = "always")

# Her får vi opp 14910 NAs, det er Israel, cntry "IL". De er markert med NA siden
# de ble fjernet tidligere.


# Innbyggertall for ESS9

ESS9$innbyggere <- NA

ESS9$innbyggere[ESS9$cntry == "AT"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,AT"])

ESS9$innbyggere[ESS9$cntry == "BE"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,BE"])

ESS9$innbyggere[ESS9$cntry == "BG"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,BG"])

ESS9$innbyggere[ESS9$cntry == "BG"] <- 7000039

ESS9$innbyggere[ESS9$cntry == "CH"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,CH"])

ESS9$innbyggere[ESS9$cntry == "CY"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,CY"])

ESS9$innbyggere[ESS9$cntry == "CZ"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,CZ"])

ESS9$innbyggere[ESS9$cntry == "DE"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,DE"])

ESS9$innbyggere[ESS9$cntry == "EE"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,EE"])

ESS9$innbyggere[ESS9$cntry == "FI"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,FI"])

population$`2018`[population$`indic_de,geo\\time` == "JAN,FR"]

# endrer manuelt

population$`2018`[population$`indic_de,geo\\time` == "JAN,FR"] <- 66918941

ESS9$innbyggere[ESS9$cntry == "FR"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,FR"])

ESS9$innbyggere[ESS9$cntry == "GB"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,UK"])

ESS9$innbyggere[ESS9$cntry == "HU"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,HU"])

ESS9$innbyggere[ESS9$cntry == "IE"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,IE"])

ESS9$innbyggere[ESS9$cntry == "IT"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,IT"])

ESS9$innbyggere[ESS9$cntry == "NL"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,NL"])

ESS9$innbyggere[ESS9$cntry == "NO"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,NO"])

ESS9$innbyggere[ESS9$cntry == "PL"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,PL"])

ESS9$innbyggere[ESS9$cntry == "RS"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,RS"])

ESS9$innbyggere[ESS9$cntry == "SI"] <-
  as.numeric(population$`2018`[population$`indic_de,geo\\time` == "JAN,SI"])

# Vi sjekker at alt ble riktig:

ESS9$innbyggere <- as.numeric(ESS9$innbyggere)

table(ESS9$cntry[is.na(ESS9$innbyggere)])



# Etnisk fragmentering ESS9 -----------------------------------------------

# Formelen for EF er EF_c = 1 - (X/N)*1^2 - (Y/N)*2^2 - (Z/N)*3^2

# Deretter lages EF_2

# For å illustrere formelen har vi eksempel med Norge. Kommentarene under
# hver gruppe er gjennomsnittet for den største gruppen i landet fra World
# Factbook, men deres data var tidvis veldig utdatert (enkelte land hadde
# data helt fra 2001). Derfor randomiserte jeg det.

# For Norge ville den egentlige verdien vært 0.2936617

1 - (450127 / 5295619) ^ 2 - (439536 / 5295619) ^ 2 - ((5295619 - 450127 - 439536) / 5295619) ^ 2

# Dersom vi grupperer det sammen heller enn i de randomiserte gruppene får vi
# Følgende resultat

ESS9$ef_2 <- 0

set.seed(100)

# Norge (NO)

NO_ets <- mean(ESS9$etnisitet[ESS9$cntry == "NO"], na.rm = TRUE)
NO_in <- mean(ESS9$innbyggere[ESS9$cntry == "NO"], na.rm = TRUE)

NO_m_ets1 <- mean(sample(1:NO_ets, 100))
# tilsvarer ca 10.89 % av totalbefolkningen, som er snittverdien for den største
# gruppen innvandrere til landene i undersøkelsen i følge World Factbook data er
# fra 2001-2018, der det er enkelte steder stor spredning mellom når landene fikk
# oppdatert sine data.

NO_m_ets2 <- mean(sample(1:NO_ets, 100)) / 2
# tilsvarer ca 4.01 % av totalbefolkningen, innvandrere fra den nest største gruppen
NO_m_ets3 <- mean(sample(1:NO_ets, 100)) / 4
# tilsvarer ca 2.18 % totalbefolkningen

NO_m_ets4 <- NO_ets - (NO_m_ets1 + NO_m_ets2 + NO_m_ets3)
# fanger opp resten

ESS9$ef_2[ESS9$cntry == "NO"]  <-
  1 - (NO_m_ets1 / NO_in) ^ 2 - (NO_m_ets2 / NO_in) ^ 2 - (NO_m_ets3 / NO_in) ^
  2 - (NO_m_ets4 / NO_in) ^ 2 - ((NO_in - NO_ets) / NO_in) ^ 2


# som vi ser er den egentlige verdien for Norge 0.2936617, mens den randomiserte verdien
# er ca. 0.278. Det gir en forskjell på 0.015 som virker akseptabelt. Forskjellen på den
# randomiserte verdien og verdien med kun én gruppe er med det nesten dobbelt så stor
# som differansen med de randomiserte gruppene. 0.015 for randomiserte grupper og
# 0.031 med kun én gruppe.

# Om vi tar Østerrike som en "kontrollvariabel" kan vi se om metoden
# stemmer der også

# Østerrike (AT)

AT_ets <- mean(ESS9$etnisitet[ESS9$cntry == "AT"], na.rm = TRUE)
AT_in <- mean(ESS9$innbyggere[ESS9$cntry == "AT"], na.rm = TRUE)

AT_m_ets1 <- mean(sample(1:AT_ets, 100))

AT_m_ets2 <- mean(sample(1:AT_ets, 100)) / 2

AT_m_ets3 <- mean(sample(1:AT_ets, 100)) / 4

AT_m_ets4 <- AT_ets - (AT_m_ets1 + AT_m_ets2 + AT_m_ets3)
# fanger opp resten

ESS9$ef_2[ESS9$cntry == "AT"]  <-
  1 - (AT_m_ets1 / AT_in) ^ 2 - (AT_m_ets2 / AT_in) ^ 2 - (AT_m_ets3 / AT_in) ^
  2 - (AT_m_ets4 / AT_in) ^ 2 - ((AT_in - AT_ets) / AT_in) ^ 2

# Befolkningen er delt opp i 80.8 % av hovedetnisitet, de neste er i synkende
# størrelse: 10, 2.6, 1.9, 1.8, 1.6, 1.3 og er med det 882227, 229379, 167623,
# 158800, 141156, 114689

1 - (882227 / AT_in) ^ 2 - (229379 / AT_in) ^ 2 - (167623 / AT_in) ^
  2 - (158800 / AT_in) ^ 2 - (141156 / AT_in) ^ 2 - (114689 / AT_in) ^
  2 - ((AT_in - AT_ets) / AT_in) ^ 2

# Med de reelle tallene får Østerrike en indeks på 0.3347534, mens den randome
# verdien vi fikk er ca. 0.334 og en forskjell på 0.0007534 må sies å være en suksess.

# Om vi forsøker det med kun 1 etnisk gruppe utenom majoriteten får vi 0.309817
# og med det en forskjell på 0.0249364 som er betydelig mer.

1 - (AT_ets / AT_in) ^ 2 - ((AT_in - AT_ets) / AT_in) ^ 2

# For å ha et litt ryddigere område sletter vi variablene vi ikke kommer til å bruke
# igjen. De har uansett blitt ført inn i datasettet. Dette gjøres også for de kommende
# landene.

rm(NO_m_ets1, NO_m_ets2, NO_m_ets3, NO_m_ets4, NO_ets, NO_in)
rm(AT_m_ets1, AT_m_ets2, AT_m_ets3, AT_m_ets4, AT_ets, AT_in)

# Belgia (BE)

BE_ets <- mean(ESS9$etnisitet[ESS9$cntry == "BE"], na.rm = TRUE)
BE_in <- mean(ESS9$innbyggere[ESS9$cntry == "BE"], na.rm = TRUE)

BE_m_ets1 <- mean(sample(1:BE_ets, 100))

BE_m_ets2 <- mean(sample(1:BE_ets, 100)) / 2

BE_m_ets3 <- mean(sample(1:BE_ets, 100)) / 4

BE_m_ets4 <- BE_ets - (BE_m_ets1 + BE_m_ets2 + BE_m_ets3)

ESS9$ef_2[ESS9$cntry == "BE"]  <-
  1 - (BE_m_ets1 / BE_in) ^ 2 - (BE_m_ets2 / BE_in) ^ 2 - (BE_m_ets3 / BE_in) ^
  2 - (BE_m_ets4 / BE_in) ^ 2 - ((BE_in - BE_ets) / BE_in) ^ 2

rm(BE_m_ets1, BE_m_ets2, BE_m_ets3, BE_m_ets4, BE_ets, BE_in)

# Bulgaria (BG)

BG_ets <- mean(ESS9$etnisitet[ESS9$cntry == "BG"], na.rm = TRUE)
BG_in <- mean(ESS9$innbyggere[ESS9$cntry == "BG"], na.rm = TRUE)

BG_m_ets1 <- mean(sample(1:BG_ets, 100))

BG_m_ets2 <- mean(sample(1:BG_ets, 100)) / 2

BG_m_ets3 <- mean(sample(1:BG_ets, 100)) / 4

BG_m_ets4 <- BG_ets - (BG_m_ets1 + BG_m_ets2 + BG_m_ets3)

ESS9$ef_2[ESS9$cntry == "BG"]  <-
  1 - (BG_m_ets1 / BG_in) ^ 2 - (BG_m_ets2 / BG_in) ^ 2 - (BG_m_ets3 / BG_in) ^
  2 - (BG_m_ets4 / BG_in) ^ 2 - ((BG_in - BG_ets) / BG_in) ^ 2

rm(BG_m_ets1, BG_m_ets2, BG_m_ets3, BG_m_ets4, BG_ets, BG_in)

# Sveits (CH)

CH_ets <- mean(ESS9$etnisitet[ESS9$cntry == "CH"], na.rm = TRUE)
CH_in <- mean(ESS9$innbyggere[ESS9$cntry == "CH"], na.rm = TRUE)

CH_m_ets1 <- mean(sample(1:CH_ets, 100))

CH_m_ets2 <- mean(sample(1:CH_ets, 100)) / 2

CH_m_ets3 <- mean(sample(1:CH_ets, 100)) / 4

CH_m_ets4 <- CH_ets - (CH_m_ets1 + CH_m_ets2 + CH_m_ets3)

ESS9$ef_2[ESS9$cntry == "CH"]  <-
  1 - (CH_m_ets1 / CH_in) ^ 2 - (CH_m_ets2 / CH_in) ^ 2 - (CH_m_ets3 / CH_in) ^
  2 - (CH_m_ets4 / CH_in) ^ 2 - ((CH_in - CH_ets) / CH_in) ^ 2

rm(CH_m_ets1, CH_m_ets2, CH_m_ets3, CH_m_ets4, CH_ets, CH_in)

# Kypros (CY)

CY_ets <- mean(ESS9$etnisitet[ESS9$cntry == "CY"], na.rm = TRUE)
CY_in <- mean(ESS9$innbyggere[ESS9$cntry == "CY"], na.rm = TRUE)

CY_m_ets1 <- mean(sample(1:CY_ets, 100))

CY_m_ets2 <- mean(sample(1:CY_ets, 100)) / 2

CY_m_ets3 <- mean(sample(1:CY_ets, 100)) / 4

CY_m_ets4 <- CY_ets - (CY_m_ets1 + CY_m_ets2 + CY_m_ets3)

ESS9$ef_2[ESS9$cntry == "CY"]  <-
  1 - (CY_m_ets1 / CY_in) ^ 2 - (CY_m_ets2 / CY_in) ^ 2 - (CY_m_ets3 / CY_in) ^
  2 - (CY_m_ets4 / CY_in) ^ 2 - ((CY_in - CY_ets) / CY_in) ^ 2

rm(CY_m_ets1, CY_m_ets2, CY_m_ets3, CY_m_ets4, CY_ets, CY_in)

# Tsjekkia (CZ)

CZ_ets <- mean(ESS9$etnisitet[ESS9$cntry == "CZ"], na.rm = TRUE)
CZ_in <- mean(ESS9$innbyggere[ESS9$cntry == "CZ"], na.rm = TRUE)

CZ_m_ets1 <- mean(sample(1:CZ_ets, 100))

CZ_m_ets2 <- mean(sample(1:CZ_ets, 100)) / 2

CZ_m_ets3 <- mean(sample(1:CZ_ets, 100)) / 4

CZ_m_ets4 <- CZ_ets - (CZ_m_ets1 + CZ_m_ets2 + CZ_m_ets3)

ESS9$ef_2[ESS9$cntry == "CZ"]  <-
  1 - (CZ_m_ets1 / CZ_in) ^ 2 - (CZ_m_ets2 / CZ_in) ^ 2 - (CZ_m_ets3 / CZ_in) ^
  2 - (CZ_m_ets4 / CZ_in) ^ 2 - ((CZ_in - CZ_ets) / CZ_in) ^ 2

rm(CZ_m_ets1, CZ_m_ets2, CZ_m_ets3, CZ_m_ets4, CZ_ets, CZ_in)

# Tyskland (DE)

DE_ets <- mean(ESS9$etnisitet[ESS9$cntry == "DE"], na.rm = TRUE)
DE_in <- mean(ESS9$innbyggere[ESS9$cntry == "DE"], na.rm = TRUE)

DE_m_ets1 <- mean(sample(1:DE_ets, 100))

DE_m_ets2 <- mean(sample(1:DE_ets, 100)) / 2

DE_m_ets3 <- mean(sample(1:DE_ets, 100)) / 4

DE_m_ets4 <- DE_ets - (DE_m_ets1 + DE_m_ets2 + DE_m_ets3)

ESS9$ef_2[ESS9$cntry == "DE"]  <-
  1 - (DE_m_ets1 / DE_in) ^ 2 - (DE_m_ets2 / DE_in) ^ 2 - (DE_m_ets3 / DE_in) ^
  2 - (DE_m_ets4 / DE_in) ^ 2 - ((DE_in - DE_ets) / DE_in) ^ 2

rm(DE_m_ets1, DE_m_ets2, DE_m_ets3, DE_m_ets4, DE_ets, DE_in)

# Estland (EE)

EE_ets <- mean(ESS9$etnisitet[ESS9$cntry == "EE"], na.rm = TRUE)
EE_in <- mean(ESS9$innbyggere[ESS9$cntry == "EE"], na.rm = TRUE)

EE_m_ets1 <- mean(sample(1:EE_ets, 100))

EE_m_ets2 <- mean(sample(1:EE_ets, 100)) / 2

EE_m_ets3 <- mean(sample(1:EE_ets, 100)) / 4

EE_m_ets4 <- EE_ets - (EE_m_ets1 + EE_m_ets2 + EE_m_ets3)

ESS9$ef_2[ESS9$cntry == "EE"]  <-
  1 - (EE_m_ets1 / EE_in) ^ 2 - (EE_m_ets2 / EE_in) ^ 2 - (EE_m_ets3 / EE_in) ^
  2 - (EE_m_ets4 / EE_in) ^ 2 - ((EE_in - EE_ets) / EE_in) ^ 2

rm(EE_m_ets1, EE_m_ets2, EE_m_ets3, EE_m_ets4, EE_ets, EE_in)

# Finland (FI)

FI_ets <- mean(ESS9$etnisitet[ESS9$cntry == "FI"], na.rm = TRUE)
FI_in <- mean(ESS9$innbyggere[ESS9$cntry == "FI"], na.rm = TRUE)

FI_m_ets1 <- mean(sample(1:FI_ets, 100))

FI_m_ets2 <- mean(sample(1:FI_ets, 100)) / 2

FI_m_ets3 <- mean(sample(1:FI_ets, 100)) / 4

FI_m_ets4 <- FI_ets - (FI_m_ets1 + FI_m_ets2 + FI_m_ets3)

ESS9$ef_2[ESS9$cntry == "FI"]  <-
  1 - (FI_m_ets1 / FI_in) ^ 2 - (FI_m_ets2 / FI_in) ^ 2 - (FI_m_ets3 / FI_in) ^
  2 - (FI_m_ets4 / FI_in) ^ 2 - ((FI_in - FI_ets) / FI_in) ^ 2

rm(FI_m_ets1, FI_m_ets2, FI_m_ets3, FI_m_ets4, FI_ets, FI_in)

# Storbritannia

GB_ets <- mean(ESS9$etnisitet[ESS9$cntry == "GB"], na.rm = TRUE)
GB_in <- mean(ESS9$innbyggere[ESS9$cntry == "GB"], na.rm = TRUE)

GB_m_ets1 <- mean(sample(1:GB_ets, 100))

GB_m_ets2 <- mean(sample(1:GB_ets, 100)) / 2

GB_m_ets3 <- mean(sample(1:GB_ets, 100)) / 4

GB_m_ets4 <- GB_ets - (GB_m_ets1 + GB_m_ets2 + GB_m_ets3)

ESS9$ef_2[ESS9$cntry == "GB"]  <-
  1 - (GB_m_ets1 / GB_in) ^ 2 - (GB_m_ets2 / GB_in) ^ 2 - (GB_m_ets3 / GB_in) ^
  2 - (GB_m_ets4 / GB_in) ^ 2 - ((GB_in - GB_ets) / GB_in) ^ 2

rm(GB_m_ets1, GB_m_ets2, GB_m_ets3, GB_m_ets4, GB_ets, GB_in)

# Ungarn (HU)

HU_ets <- mean(ESS9$etnisitet[ESS9$cntry == "HU"], na.rm = TRUE)
HU_in <- mean(ESS9$innbyggere[ESS9$cntry == "HU"], na.rm = TRUE)

HU_m_ets1 <- mean(sample(1:HU_ets, 100))

HU_m_ets2 <- mean(sample(1:HU_ets, 100)) / 2

HU_m_ets3 <- mean(sample(1:HU_ets, 100)) / 4

HU_m_ets4 <- HU_ets - (HU_m_ets1 + HU_m_ets2 + HU_m_ets3)

ESS9$ef_2[ESS9$cntry == "HU"]  <-
  1 - (HU_m_ets1 / HU_in) ^ 2 - (HU_m_ets2 / HU_in) ^ 2 - (HU_m_ets3 / HU_in) ^
  2 - (HU_m_ets4 / HU_in) ^ 2 - ((HU_in - HU_ets) / HU_in) ^ 2

rm(HU_m_ets1, HU_m_ets2, HU_m_ets3, HU_m_ets4, HU_ets, HU_in)

# Irland (IE)

IE_ets <- mean(ESS9$etnisitet[ESS9$cntry == "IE"], na.rm = TRUE)
IE_in <- mean(ESS9$innbyggere[ESS9$cntry == "IE"], na.rm = TRUE)

IE_m_ets1 <- mean(sample(1:IE_ets, 100))

IE_m_ets2 <- mean(sample(1:IE_ets, 100)) / 2

IE_m_ets3 <- mean(sample(1:IE_ets, 100)) / 4

IE_m_ets4 <- IE_ets - (IE_m_ets1 + IE_m_ets2 + IE_m_ets3)

ESS9$ef_2[ESS9$cntry == "IE"]  <-
  1 - (IE_m_ets1 / IE_in) ^ 2 - (IE_m_ets2 / IE_in) ^ 2 - (IE_m_ets3 / IE_in) ^
  2 - (IE_m_ets4 / IE_in) ^ 2 - ((IE_in - IE_ets) / IE_in) ^ 2

rm(IE_m_ets1, IE_m_ets2, IE_m_ets3, IE_m_ets4, IE_ets, IE_in)

# Italia

IT_ets <- mean(ESS9$etnisitet[ESS9$cntry == "IT"], na.rm = TRUE)
IT_in <- mean(ESS9$innbyggere[ESS9$cntry == "IT"], na.rm = TRUE)

IT_m_ets1 <- mean(sample(1:IT_ets, 100))

IT_m_ets2 <- mean(sample(1:IT_ets, 100)) / 2

IT_m_ets3 <- mean(sample(1:IT_ets, 100)) / 4

IT_m_ets4 <- IT_ets - (IT_m_ets1 + IT_m_ets2 + IT_m_ets3)

ESS9$ef_2[ESS9$cntry == "IT"]  <-
  1 - (IT_m_ets1 / IT_in) ^ 2 - (IT_m_ets2 / IT_in) ^ 2 - (IT_m_ets3 / IT_in) ^
  2 - (IT_m_ets4 / IT_in) ^ 2 - ((IT_in - IT_ets) / IT_in) ^ 2

rm(IT_m_ets1, IT_m_ets2, IT_m_ets3, IT_m_ets4, IT_ets, IT_in)

# Nederland (NL)

NL_ets <- mean(ESS9$etnisitet[ESS9$cntry == "NL"], na.rm = TRUE)
NL_in <- mean(ESS9$innbyggere[ESS9$cntry == "NL"], na.rm = TRUE)

NL_m_ets1 <- mean(sample(1:NL_ets, 100))

NL_m_ets2 <- mean(sample(1:NL_ets, 100)) / 2

NL_m_ets3 <- mean(sample(1:NL_ets, 100)) / 4

NL_m_ets4 <- NL_ets - (NL_m_ets1 + NL_m_ets2 + NL_m_ets3)

ESS9$ef_2[ESS9$cntry == "NL"]  <-
  1 - (NL_m_ets1 / NL_in) ^ 2 - (NL_m_ets2 / NL_in) ^ 2 - (NL_m_ets3 / NL_in) ^
  2 - (NL_m_ets4 / NL_in) ^ 2 - ((NL_in - NL_ets) / NL_in) ^ 2

rm(NL_m_ets1, NL_m_ets2, NL_m_ets3, NL_m_ets4, NL_ets, NL_in)

# Norge har alt blitt gjort

# Slovenia (SI)

SI_ets <- mean(ESS9$etnisitet[ESS9$cntry == "SI"], na.rm = TRUE)
SI_in <- mean(ESS9$innbyggere[ESS9$cntry == "SI"], na.rm = TRUE)

SI_m_ets1 <- mean(sample(1:SI_ets, 100))

SI_m_ets2 <- mean(sample(1:SI_ets, 100)) / 2

SI_m_ets3 <- mean(sample(1:SI_ets, 100)) / 4

SI_m_ets4 <- SI_ets - (SI_m_ets1 + SI_m_ets2 + SI_m_ets3)

ESS9$ef_2[ESS9$cntry == "SI"]  <-
  1 - (SI_m_ets1 / SI_in) ^ 2 - (SI_m_ets2 / SI_in) ^ 2 - (SI_m_ets3 / SI_in) ^
  2 - (SI_m_ets4 / SI_in) ^ 2 - ((SI_in - SI_ets) / SI_in) ^ 2

rm(SI_m_ets1, SI_m_ets2, SI_m_ets3, SI_m_ets4, SI_ets, SI_in)



# Etnisk fragmentering ESS1-8 ---------------------------------------------

ESS1_8e01$ef_2 <- NA

# Samme prosess for ESS1-8

# Landene som er med i begge

# Norge (NO)

NO_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "NO"], na.rm = TRUE)
NO_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "NO"], na.rm = TRUE)

NO_m_ets1 <- mean(sample(1:NO_ets, 100))

NO_m_ets2 <- mean(sample(1:NO_ets, 100)) / 2

NO_m_ets3 <- mean(sample(1:NO_ets, 100)) / 4

NO_m_ets4 <- NO_ets - (NO_m_ets1 + NO_m_ets2 + NO_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "NO"]  <-
  1 - (NO_m_ets1 / NO_in) ^ 2 - (NO_m_ets2 / NO_in) ^ 2 - (NO_m_ets3 / NO_in) ^
  2 - (NO_m_ets4 / NO_in) ^ 2 - ((NO_in - NO_ets) / NO_in) ^ 2

rm(NO_m_ets1, NO_m_ets2, NO_m_ets3, NO_m_ets4, NO_ets, NO_in)


# Østerrike (AT)

AT_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "AT"], na.rm = TRUE)
AT_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "AT"], na.rm = TRUE)

AT_m_ets1 <- mean(sample(1:AT_ets, 100))

AT_m_ets2 <- mean(sample(1:AT_ets, 100)) / 2

AT_m_ets3 <- mean(sample(1:AT_ets, 100)) / 4

AT_m_ets4 <- AT_ets - (AT_m_ets1 + AT_m_ets2 + AT_m_ets3)


ESS1_8e01$ef_2[ESS1_8e01$cntry == "AT"]  <-
  1 - (AT_m_ets1 / AT_in) ^ 2 - (AT_m_ets2 / AT_in) ^ 2 - (AT_m_ets3 / AT_in) ^
  2 - (AT_m_ets4 / AT_in) ^ 2 - ((AT_in - AT_ets) / AT_in) ^ 2

rm(AT_m_ets1, AT_m_ets2, AT_m_ets3, AT_m_ets4, AT_ets, AT_in)

# Belgia (BE)

BE_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "BE"], na.rm = TRUE)
BE_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "BE"], na.rm = TRUE)

BE_m_ets1 <- mean(sample(1:BE_ets, 100))

BE_m_ets2 <- mean(sample(1:BE_ets, 100)) / 2

BE_m_ets3 <- mean(sample(1:BE_ets, 100)) / 4

BE_m_ets4 <- BE_ets - (BE_m_ets1 + BE_m_ets2 + BE_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "BE"]  <-
  1 - (BE_m_ets1 / BE_in) ^ 2 - (BE_m_ets2 / BE_in) ^ 2 - (BE_m_ets3 / BE_in) ^
  2 - (BE_m_ets4 / BE_in) ^ 2 - ((BE_in - BE_ets) / BE_in) ^ 2

rm(BE_m_ets1, BE_m_ets2, BE_m_ets3, BE_m_ets4, BE_ets, BE_in)

# Bulgaria (BG)

BG_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "BG"], na.rm = TRUE)
BG_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "BG"], na.rm = TRUE)

BG_m_ets1 <- mean(sample(1:BG_ets, 100))

BG_m_ets2 <- mean(sample(1:BG_ets, 100)) / 2

BG_m_ets3 <- mean(sample(1:BG_ets, 100)) / 4

BG_m_ets4 <- BG_ets - (BG_m_ets1 + BG_m_ets2 + BG_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "BG"]  <-
  1 - (BG_m_ets1 / BG_in) ^ 2 - (BG_m_ets2 / BG_in) ^ 2 - (BG_m_ets3 / BG_in) ^
  2 - (BG_m_ets4 / BG_in) ^ 2 - ((BG_in - BG_ets) / BG_in) ^ 2

rm(BG_m_ets1, BG_m_ets2, BG_m_ets3, BG_m_ets4, BG_ets, BG_in)

# Sveits (CH)

CH_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "CH"], na.rm = TRUE)
CH_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "CH"], na.rm = TRUE)

CH_m_ets1 <- mean(sample(1:CH_ets, 100))

CH_m_ets2 <- mean(sample(1:CH_ets, 100)) / 2

CH_m_ets3 <- mean(sample(1:CH_ets, 100)) / 4

CH_m_ets4 <- CH_ets - (CH_m_ets1 + CH_m_ets2 + CH_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "CH"]  <-
  1 - (CH_m_ets1 / CH_in) ^ 2 - (CH_m_ets2 / CH_in) ^ 2 - (CH_m_ets3 / CH_in) ^
  2 - (CH_m_ets4 / CH_in) ^ 2 - ((CH_in - CH_ets) / CH_in) ^ 2

rm(CH_m_ets1, CH_m_ets2, CH_m_ets3, CH_m_ets4, CH_ets, CH_in)

# Kypros (CY)

CY_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "CY"], na.rm = TRUE)
CY_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "CY"], na.rm = TRUE)

CY_m_ets1 <- mean(sample(1:CY_ets, 100))

CY_m_ets2 <- mean(sample(1:CY_ets, 100)) / 2

CY_m_ets3 <- mean(sample(1:CY_ets, 100)) / 4

CY_m_ets4 <- CY_ets - (CY_m_ets1 + CY_m_ets2 + CY_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "CY"]  <-
  1 - (CY_m_ets1 / CY_in) ^ 2 - (CY_m_ets2 / CY_in) ^ 2 - (CY_m_ets3 / CY_in) ^
  2 - (CY_m_ets4 / CY_in) ^ 2 - ((CY_in - CY_ets) / CY_in) ^ 2

rm(CY_m_ets1, CY_m_ets2, CY_m_ets3, CY_m_ets4, CY_ets, CY_in)

# Tsjekkia (CZ)

CZ_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "CZ"], na.rm = TRUE)
CZ_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "CZ"], na.rm = TRUE)

CZ_m_ets1 <- mean(sample(1:CZ_ets, 100))

CZ_m_ets2 <- mean(sample(1:CZ_ets, 100)) / 2

CZ_m_ets3 <- mean(sample(1:CZ_ets, 100)) / 4

CZ_m_ets4 <- CZ_ets - (CZ_m_ets1 + CZ_m_ets2 + CZ_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "CZ"]  <-
  1 - (CZ_m_ets1 / CZ_in) ^ 2 - (CZ_m_ets2 / CZ_in) ^ 2 - (CZ_m_ets3 / CZ_in) ^
  2 - (CZ_m_ets4 / CZ_in) ^ 2 - ((CZ_in - CZ_ets) / CZ_in) ^ 2

rm(CZ_m_ets1, CZ_m_ets2, CZ_m_ets3, CZ_m_ets4, CZ_ets, CZ_in)

# Tyskland (DE)

DE_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "DE"], na.rm = TRUE)
DE_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "DE"], na.rm = TRUE)

DE_m_ets1 <- mean(sample(1:DE_ets, 100))

DE_m_ets2 <- mean(sample(1:DE_ets, 100)) / 2

DE_m_ets3 <- mean(sample(1:DE_ets, 100)) / 4

DE_m_ets4 <- DE_ets - (DE_m_ets1 + DE_m_ets2 + DE_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "DE"]  <-
  1 - (DE_m_ets1 / DE_in) ^ 2 - (DE_m_ets2 / DE_in) ^ 2 - (DE_m_ets3 / DE_in) ^
  2 - (DE_m_ets4 / DE_in) ^ 2 - ((DE_in - DE_ets) / DE_in) ^ 2

rm(DE_m_ets1, DE_m_ets2, DE_m_ets3, DE_m_ets4, DE_ets, DE_in)

# Estland (EE)

EE_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "EE"], na.rm = TRUE)
EE_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "EE"], na.rm = TRUE)

EE_m_ets1 <- mean(sample(1:EE_ets, 100))

EE_m_ets2 <- mean(sample(1:EE_ets, 100)) / 2

EE_m_ets3 <- mean(sample(1:EE_ets, 100)) / 4

EE_m_ets4 <- EE_ets - (EE_m_ets1 + EE_m_ets2 + EE_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "EE"]  <-
  1 - (EE_m_ets1 / EE_in) ^ 2 - (EE_m_ets2 / EE_in) ^ 2 - (EE_m_ets3 / EE_in) ^
  2 - (EE_m_ets4 / EE_in) ^ 2 - ((EE_in - EE_ets) / EE_in) ^ 2

rm(EE_m_ets1, EE_m_ets2, EE_m_ets3, EE_m_ets4, EE_ets, EE_in)

# Finland (FI)

FI_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "FI"], na.rm = TRUE)
FI_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "FI"], na.rm = TRUE)

FI_m_ets1 <- mean(sample(1:FI_ets, 100))

FI_m_ets2 <- mean(sample(1:FI_ets, 100)) / 2

FI_m_ets3 <- mean(sample(1:FI_ets, 100)) / 4

FI_m_ets4 <- FI_ets - (FI_m_ets1 + FI_m_ets2 + FI_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "FI"]  <-
  1 - (FI_m_ets1 / FI_in) ^ 2 - (FI_m_ets2 / FI_in) ^ 2 - (FI_m_ets3 / FI_in) ^
  2 - (FI_m_ets4 / FI_in) ^ 2 - ((FI_in - FI_ets) / FI_in) ^ 2

rm(FI_m_ets1, FI_m_ets2, FI_m_ets3, FI_m_ets4, FI_ets, FI_in)

# Storbritannia

GB_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "GB"], na.rm = TRUE)
GB_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "GB"], na.rm = TRUE)

GB_m_ets1 <- mean(sample(1:GB_ets, 100))

GB_m_ets2 <- mean(sample(1:GB_ets, 100)) / 2

GB_m_ets3 <- mean(sample(1:GB_ets, 100)) / 4

GB_m_ets4 <- GB_ets - (GB_m_ets1 + GB_m_ets2 + GB_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "GB"]  <-
  1 - (GB_m_ets1 / GB_in) ^ 2 - (GB_m_ets2 / GB_in) ^ 2 - (GB_m_ets3 / GB_in) ^
  2 - (GB_m_ets4 / GB_in) ^ 2 - ((GB_in - GB_ets) / GB_in) ^ 2

rm(GB_m_ets1, GB_m_ets2, GB_m_ets3, GB_m_ets4, GB_ets, GB_in)

# Ungarn (HU)

HU_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "HU"], na.rm = TRUE)
HU_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "HU"], na.rm = TRUE)

HU_m_ets1 <- mean(sample(1:HU_ets, 100))

HU_m_ets2 <- mean(sample(1:HU_ets, 100)) / 2

HU_m_ets3 <- mean(sample(1:HU_ets, 100)) / 4

HU_m_ets4 <- HU_ets - (HU_m_ets1 + HU_m_ets2 + HU_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "HU"]  <-
  1 - (HU_m_ets1 / HU_in) ^ 2 - (HU_m_ets2 / HU_in) ^ 2 - (HU_m_ets3 / HU_in) ^
  2 - (HU_m_ets4 / HU_in) ^ 2 - ((HU_in - HU_ets) / HU_in) ^ 2

rm(HU_m_ets1, HU_m_ets2, HU_m_ets3, HU_m_ets4, HU_ets, HU_in)

# Irland (IE)

IE_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "IE"], na.rm = TRUE)
IE_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "IE"], na.rm = TRUE)

IE_m_ets1 <- mean(sample(1:IE_ets, 100))

IE_m_ets2 <- mean(sample(1:IE_ets, 100)) / 2

IE_m_ets3 <- mean(sample(1:IE_ets, 100)) / 4

IE_m_ets4 <- IE_ets - (IE_m_ets1 + IE_m_ets2 + IE_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "IE"]  <-
  1 - (IE_m_ets1 / IE_in) ^ 2 - (IE_m_ets2 / IE_in) ^ 2 - (IE_m_ets3 / IE_in) ^
  2 - (IE_m_ets4 / IE_in) ^ 2 - ((IE_in - IE_ets) / IE_in) ^ 2

rm(IE_m_ets1, IE_m_ets2, IE_m_ets3, IE_m_ets4, IE_ets, IE_in)

# Italia

IT_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "IT"], na.rm = TRUE)
IT_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "IT"], na.rm = TRUE)

IT_m_ets1 <- mean(sample(1:IT_ets, 100))

IT_m_ets2 <- mean(sample(1:IT_ets, 100)) / 2

IT_m_ets3 <- mean(sample(1:IT_ets, 100)) / 4

IT_m_ets4 <- IT_ets - (IT_m_ets1 + IT_m_ets2 + IT_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "IT"]  <-
  1 - (IT_m_ets1 / IT_in) ^ 2 - (IT_m_ets2 / IT_in) ^ 2 - (IT_m_ets3 / IT_in) ^
  2 - (IT_m_ets4 / IT_in) ^ 2 - ((IT_in - IT_ets) / IT_in) ^ 2

rm(IT_m_ets1, IT_m_ets2, IT_m_ets3, IT_m_ets4, IT_ets, IT_in)

# Nederland (NL)

NL_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "NL"], na.rm = TRUE)
NL_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "NL"], na.rm = TRUE)

NL_m_ets1 <- mean(sample(1:NL_ets, 100))

NL_m_ets2 <- mean(sample(1:NL_ets, 100)) / 2

NL_m_ets3 <- mean(sample(1:NL_ets, 100)) / 4

NL_m_ets4 <- NL_ets - (NL_m_ets1 + NL_m_ets2 + NL_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "NL"]  <-
  1 - (NL_m_ets1 / NL_in) ^ 2 - (NL_m_ets2 / NL_in) ^ 2 - (NL_m_ets3 / NL_in) ^
  2 - (NL_m_ets4 / NL_in) ^ 2 - ((NL_in - NL_ets) / NL_in) ^ 2

rm(NL_m_ets1, NL_m_ets2, NL_m_ets3, NL_m_ets4, NL_ets, NL_in)

# Slovenia (SI)

SI_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "SI"], na.rm = TRUE)
SI_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "SI"], na.rm = TRUE)

SI_m_ets1 <- mean(sample(1:SI_ets, 100))

SI_m_ets2 <- mean(sample(1:SI_ets, 100)) / 2

SI_m_ets3 <- mean(sample(1:SI_ets, 100)) / 4

SI_m_ets4 <- SI_ets - (SI_m_ets1 + SI_m_ets2 + SI_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "SI"]  <-
  1 - (SI_m_ets1 / SI_in) ^ 2 - (SI_m_ets2 / SI_in) ^ 2 - (SI_m_ets3 / SI_in) ^
  2 - (SI_m_ets4 / SI_in) ^ 2 - ((SI_in - SI_ets) / SI_in) ^ 2

rm(SI_m_ets1, SI_m_ets2, SI_m_ets3, SI_m_ets4, SI_ets, SI_in)


# Landene som kun er med i ESS1_8e01

# Denmark

DK_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "DK"], na.rm = TRUE)
DK_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "DK"], na.rm = TRUE)

DK_m_ets1 <- mean(sample(1:DK_ets, 100))

DK_m_ets2 <- mean(sample(1:DK_ets, 100)) / 2

DK_m_ets3 <- mean(sample(1:DK_ets, 100)) / 4

DK_m_ets4 <- DK_ets - (DK_m_ets1 + DK_m_ets2 + DK_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "DK"]  <-
  1 - (DK_m_ets1 / DK_in) ^ 2 - (DK_m_ets2 / DK_in) ^ 2 - (DK_m_ets3 / DK_in) ^
  2 - (DK_m_ets4 / DK_in) ^ 2 - ((DK_in - DK_ets) / DK_in) ^ 2

rm(DK_m_ets1, DK_m_ets2, DK_m_ets3, DK_m_ets4, DK_ets, DK_in)

# Spain (ES)

ES_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "ES"], na.rm = TRUE)
ES_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "ES"], na.rm = TRUE)

ES_m_ets1 <- mean(sample(1:ES_ets, 100))

ES_m_ets2 <- mean(sample(1:ES_ets, 100)) / 2

ES_m_ets3 <- mean(sample(1:ES_ets, 100)) / 4

ES_m_ets4 <- ES_ets - (ES_m_ets1 + ES_m_ets2 + ES_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "ES"]  <-
  1 - (ES_m_ets1 / ES_in) ^ 2 - (ES_m_ets2 / ES_in) ^ 2 - (ES_m_ets3 / ES_in) ^
  2 - (ES_m_ets4 / ES_in) ^ 2 - ((ES_in - ES_ets) / ES_in) ^ 2

rm(ES_m_ets1, ES_m_ets2, ES_m_ets3, ES_m_ets4, ES_ets, ES_in)


# Frankrike (FR) har vi ikke data på hvor mange personer som har

FR_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "FR"], na.rm = TRUE)
FR_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "FR"], na.rm = TRUE)

FR_m_ets1 <- mean(sample(1:FR_ets, 100))

FR_m_ets2 <- mean(sample(1:FR_ets, 100)) / 2

FR_m_ets3 <- mean(sample(1:FR_ets, 100)) / 4

FR_m_ets4 <- FR_ets - (FR_m_ets1 + FR_m_ets2 + FR_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "FR"]  <-
  1 - (FR_m_ets1 / FR_in) ^ 2 - (FR_m_ets2 / FR_in) ^ 2 - (FR_m_ets3 / FR_in) ^
  2 - (FR_m_ets4 / FR_in) ^ 2 - ((FR_in - FR_ets) / FR_in) ^ 2

rm(FR_m_ets1, FR_m_ets2, FR_m_ets3, FR_m_ets4, FR_ets, FR_in)


# Hellas (GR)

GR_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "GR"], na.rm = TRUE)
GR_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "GR"], na.rm = TRUE)

GR_m_ets1 <- mean(sample(1:GR_ets, 100))

GR_m_ets2 <- mean(sample(1:GR_ets, 100)) / 2

GR_m_ets3 <- mean(sample(1:GR_ets, 100)) / 4

GR_m_ets4 <- GR_ets - (GR_m_ets1 + GR_m_ets2 + GR_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "GR"]  <-
  1 - (GR_m_ets1 / GR_in) ^ 2 - (GR_m_ets2 / GR_in) ^ 2 - (GR_m_ets3 / GR_in) ^
  2 - (GR_m_ets4 / GR_in) ^ 2 - ((GR_in - GR_ets) / GR_in) ^ 2

rm(GR_m_ets1, GR_m_ets2, GR_m_ets3, GR_m_ets4, GR_ets, GR_in)


# Kroatsia (HR)

HR_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "HR"], na.rm = TRUE)
HR_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "HR"], na.rm = TRUE)

HR_m_ets1 <- mean(sample(1:HR_ets, 100))

HR_m_ets2 <- mean(sample(1:HR_ets, 100)) / 2

HR_m_ets3 <- mean(sample(1:HR_ets, 100)) / 4

HR_m_ets4 <- HR_ets - (HR_m_ets1 + HR_m_ets2 + HR_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "HR"]  <-
  1 - (HR_m_ets1 / HR_in) ^ 2 - (HR_m_ets2 / HR_in) ^ 2 - (HR_m_ets3 / HR_in) ^
  2 - (HR_m_ets4 / HR_in) ^ 2 - ((HR_in - HR_ets) / HR_in) ^ 2

rm(HR_m_ets1, HR_m_ets2, HR_m_ets3, HR_m_ets4, HR_ets, HR_in)

# Island (IS)

IS_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "IS"], na.rm = TRUE)
IS_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "IS"], na.rm = TRUE)

IS_m_ets1 <- mean(sample(1:IS_ets, 100))

IS_m_ets2 <- mean(sample(1:IS_ets, 100)) / 2

IS_m_ets3 <- mean(sample(1:IS_ets, 100)) / 4

IS_m_ets4 <- IS_ets - (IS_m_ets1 + IS_m_ets2 + IS_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "IS"]  <-
  1 - (IS_m_ets1 / IS_in) ^ 2 - (IS_m_ets2 / IS_in) ^ 2 - (IS_m_ets3 / IS_in) ^
  2 - (IS_m_ets4 / IS_in) ^ 2 - ((IS_in - IS_ets) / IS_in) ^ 2

rm(IS_m_ets1, IS_m_ets2, IS_m_ets3, IS_m_ets4, IS_ets, IS_in)

# Litauen (LT)

LT_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "LT"], na.rm = TRUE)
LT_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "LT"], na.rm = TRUE)

LT_m_ets1 <- mean(sample(1:LT_ets, 100))

LT_m_ets2 <- mean(sample(1:LT_ets, 100)) / 2

LT_m_ets3 <- mean(sample(1:LT_ets, 100)) / 4

LT_m_ets4 <- LT_ets - (LT_m_ets1 + LT_m_ets2 + LT_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "LT"]  <-
  1 - (LT_m_ets1 / LT_in) ^ 2 - (LT_m_ets2 / LT_in) ^ 2 - (LT_m_ets3 / LT_in) ^
  2 - (LT_m_ets4 / LT_in) ^ 2 - ((LT_in - LT_ets) / LT_in) ^ 2

rm(LT_m_ets1, LT_m_ets2, LT_m_ets3, LT_m_ets4, LT_ets, LT_in)


# Luxembourg (LU)
LU_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "LU"], na.rm = TRUE)
LU_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "LU"], na.rm = TRUE)

LU_m_ets1 <- mean(sample(1:LU_ets, 100))

LU_m_ets2 <- mean(sample(1:LU_ets, 100)) / 2

LU_m_ets3 <- mean(sample(1:LU_ets, 100)) / 4

LU_m_ets4 <- LU_ets - (LU_m_ets1 + LU_m_ets2 + LU_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "LU"]  <-
  1 - (LU_m_ets1 / LU_in) ^ 2 - (LU_m_ets2 / LU_in) ^ 2 - (LU_m_ets3 / LU_in) ^
  2 - (LU_m_ets4 / LU_in) ^ 2 - ((LU_in - LU_ets) / LU_in) ^ 2

rm(LU_m_ets1, LU_m_ets2, LU_m_ets3, LU_m_ets4, LU_ets, LU_in)

# Polen (PL)

PL_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "PL"], na.rm = TRUE)
PL_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "PL"], na.rm = TRUE)

PL_m_ets1 <- mean(sample(1:PL_ets, 100))

PL_m_ets2 <- mean(sample(1:PL_ets, 100)) / 2

PL_m_ets3 <- mean(sample(1:PL_ets, 100)) / 4

PL_m_ets4 <- PL_ets - (PL_m_ets1 + PL_m_ets2 + PL_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "PL"]  <-
  1 - (PL_m_ets1 / PL_in) ^ 2 - (PL_m_ets2 / PL_in) ^ 2 - (PL_m_ets3 / PL_in) ^
  2 - (PL_m_ets4 / PL_in) ^ 2 - ((PL_in - PL_ets) / PL_in) ^ 2

rm(PL_m_ets1, PL_m_ets2, PL_m_ets3, PL_m_ets4, PL_ets, PL_in)


# Portugal (PT)

PT_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "PT"], na.rm = TRUE)
PT_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "PT"], na.rm = TRUE)

PT_m_ets1 <- mean(sample(1:PT_ets, 100))

PT_m_ets2 <- mean(sample(1:PT_ets, 100)) / 2

PT_m_ets3 <- mean(sample(1:PT_ets, 100)) / 4

PT_m_ets4 <- PT_ets - (PT_m_ets1 + PT_m_ets2 + PT_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "PT"]  <-
  1 - (PT_m_ets1 / PT_in) ^ 2 - (PT_m_ets2 / PT_in) ^ 2 - (PT_m_ets3 / PT_in) ^
  2 - (PT_m_ets4 / PT_in) ^ 2 - ((PT_in - PT_ets) / PT_in) ^ 2

rm(PT_m_ets1, PT_m_ets2, PT_m_ets3, PT_m_ets4, PT_ets, PT_in)

# Romania (RO) har vi ikke etnisitet-data på.


# Russland (RU) har vi ikke etnisitet-data på.


# Serbia (RS) har vi ikke etnisitet-data på.


# Sverige

SE_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "SE"], na.rm = TRUE)
SE_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "SE"], na.rm = TRUE)

SE_m_ets1 <- mean(sample(1:SE_ets, 100))

SE_m_ets2 <- mean(sample(1:SE_ets, 100)) / 2

SE_m_ets3 <- mean(sample(1:SE_ets, 100)) / 4

SE_m_ets4 <- SE_ets - (SE_m_ets1 + SE_m_ets2 + SE_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "SE"]  <-
  1 - (SE_m_ets1 / SE_in) ^ 2 - (SE_m_ets2 / SE_in) ^ 2 - (SE_m_ets3 / SE_in) ^
  2 - (SE_m_ets4 / SE_in) ^ 2 - ((SE_in - SE_ets) / SE_in) ^ 2

rm(SE_m_ets1, SE_m_ets2, SE_m_ets3, SE_m_ets4, SE_ets, SE_in)

# Slovakia (SK)

SK_ets <-
  mean(ESS1_8e01$etnisitet[ESS1_8e01$cntry == "SK"], na.rm = TRUE)
SK_in <-
  mean(ESS1_8e01$innbyggere[ESS1_8e01$cntry == "SK"], na.rm = TRUE)

SK_m_ets1 <- mean(sample(1:SK_ets, 100))

SK_m_ets2 <- mean(sample(1:SK_ets, 100)) / 2

SK_m_ets3 <- mean(sample(1:SK_ets, 100)) / 4

SK_m_ets4 <- SK_ets - (SK_m_ets1 + SK_m_ets2 + SK_m_ets3)

ESS1_8e01$ef_2[ESS1_8e01$cntry == "SK"]  <-
  1 - (SK_m_ets1 / SK_in) ^ 2 - (SK_m_ets2 / SK_in) ^ 2 - (SK_m_ets3 / SK_in) ^
  2 - (SK_m_ets4 / SK_in) ^ 2 - ((SK_in - SK_ets) / SK_in) ^ 2

rm(SK_m_ets1, SK_m_ets2, SK_m_ets3, SK_m_ets4, SK_ets, SK_in)

# Tyrkia (TR) har vi ikke etnisitet-data på.


# Ukraina (UA) har vi ikke etnisitet-data på.


# Kosovo (XK) har vi ikke etnisitet-data på.



# Lager nytt datasett uten outliers ---------------------------------------

# Vi sjekker deretter ESS9 for eventuelle outliers.

library(dplyr)

myvars <-
  c(
    "idno",
    "gndr",
    "cntry",
    "landgrupper",
    "domicil",
    "gdp",
    "imwbcnt",
    "imbgeco",
    "imueclt",
    "eduyrs",
    "agea",
    "happy",
    "ef_2",
    "dweight",
    "pweight"
    )


# Da jeg gjorde dette fant jeg ut at det ikke var forenelig med SPSS sin måte å
# sette labels på variablene sine. Jeg måtte derfor fjere dem ved hjelp av
# pakken "labelled". For sikkerhets skyld lager jeg et nytt datasett.

noout_ESS9 <- ESS9[myvars]

rm(myvars)


# Mahal måler avstand mellom ytterpunktene, altså en person som har svart veldig
# annerledes enn alle andre. Eksempelvis en person som har svart veldig lavt på
# agea, men veldig høyt på eduyrs.

mahal <- mahalanobis(
  noout_ESS9[, c(6:12)],
  colMeans(noout_ESS9[, c(6:12)], na.rm = TRUE),
  cov(noout_ESS9[, c(6:12)], use = "pairwise.complete.obs")
)

summary(mahal)

# Viser scoren jeg må kutte bort svar som faller under. Her går jeg for 99%
# konfidensintervall, siden dette kun er gjennomgangen av datasettet.

cutoff <- qchisq(1 - .001, ncol(noout_ESS9[, c(6:12)]))
cutoff

# Vi sjekker hvor mange svar som har en score høyere enn cutoff-scoren. FALSE
# betyr at de er det og vi kan kutte deres svar.

summary(mahal < cutoff)

# Dersom vi ikke hadde inkludert GDP i mahalanobis-distansen og sett på cutoff
# da, ville vi bare kuttet 19 færre svar, men selv om gdp ikke er på
# individnivå, vil det påvirke svarene til individer og jeg velger dermed
# likevel å kutte 180 svar. Dersom vi inkluderer imueclt og imbgeco i tillegg
# til imwbcnt (som vi senere kan smelte sammen til én variabel), får vi 282.
# Foreløpig gjør vi det.


noout_ESS9 <- noout_ESS9[mahal < cutoff , ]

rm(mahal)

# vi sjekker for multikolinearitet, altså graden av sammenheng mellom
# forklaringsvariablene våre.

korrelasjon <-
  cor(noout_ESS9[, c(6:12)], use = "pairwise.complete.obs")
symnum(korrelasjon)

# vi har 0,6 i korrelasjon mellom de tre innvanddrings-variablene våre. Det
# tilsier at det skal gå greit å kombinere dem. Eller er det ingen problematisk
# korrelasjon.

# Vi kan presentere det i en graf

if (!require("corrplot"))
  install.packages("corrplot")
library(corrplot)

korrelasjonsplot <- corrplot(
  korrelasjon,
  type = "upper",
  order = "hclust",
  sig.level = 0.05,
  tl.pos = "td",
  method = "circle",
  tl.col = "black"
)


# assumption

random <- rchisq(nrow(noout_ESS9), df = 9)
fake <- lm(random ~ ., data = noout_ESS9[, c(6:12)])
fitted = scale(fake$fitted.values)
standardized = rstudent(fake)

# linearity
qqnorm(standardized)
abline(0, 1)

# vi ser at det ikke er helt lineært og vi burde derfor basere oss på non-linear models.
# Senere i skriptet ser vi både på lineære og ulineære modeller.

# normality
hist(standardized)

# Vi ønsker at det er sentrert over 0 - det er det, og at mesteparten av dataen
# er mellom -2 og 2, noe det er.

plot(fitted, standardized)
abline(0, 0)
abline(v = 0)

rm(fake, fitted, korrelasjon)

# også her ser vi at mesteparten av dataen er mellom -2 og 2, som er bra. Vi har veldig
# store mengder data, så selv om det er data som faller utenfor så har vi veldig mye data
# innenfor. Det skal også nevnes at det her er basert på tilfeldig data, basert på vår ekte
# data. Det skal ikke forandre seg drastisk fra gang til gang, men vil variere.

# Jeg har lyst til å se på de tre innvandringsvariablene i én variabel for en
# litt mer oversiktlig og kanskje mer nøyaktig analyse. Først sjekker vi hvordan
# de er i forhold til hverandre.

p_innvandring <- ggplot(data = noout_ESS9, aes(eduyrs)) +
  geom_smooth(aes(y = imwbcnt, color = "Immigrants make country worse or better"),
              method = "gam") +
  geom_smooth(aes(y = imbgeco, color = "Immigrants are bad or good for the economy"),
              method = "gam") +
  geom_smooth(
    aes(y = imueclt, color = "Country's cultural life undermined or enriched by immigrants"),
    method = "gam"
  ) +
  ylim(0, 10) +
  ylab("Skala fra 0-10") +
  xlab("Antall år fullført utdanning") +
  ggtitle("Innvandringsvariablene og utdanningsår. GAM-smoothing uten outliers.") +
  theme_bw()
p_innvandring

# De er ganske like i utgangspunktet.

# Vi kombinerer de tre innvandringsvariablene til én.

noout_ESS9$innvandring <-
  (noout_ESS9$imwbcnt + noout_ESS9$imueclt + noout_ESS9$imbgeco)/3

# Vi kan også smelte dem sammen og med det beholde verdiene, men i én variabel
# heller enn tre. Det betyr at for hver person har man tatt svarene deres på 
# imwbcnt,imbgeco og imueclt og samlet dem med svarene på de andre spørsmålene.
# Det gir en enkel måte å se på de tre svarene samlet, men uten å endre på dem.

if (!require("reshape2"))
  install.packages("reshape2")
library(reshape2)

names(noout_ESS9)

noout_ESS9lang <- melt(
  noout_ESS9,
  id.vars = c(
    "idno",
    "gndr",
    "cntry",
    "landgrupper",
    "domicil",
    "gdp",
    "eduyrs",
    "agea",
    "happy",
    "ef_2",
    "dweight",
    "pweight"
  ),
  measure.vars = c("imwbcnt", "imbgeco", "imueclt")
)

names(noout_ESS9lang)[14] <- "innvandring"

# Modellering uten outliers ------------------------------------------------------

library(nlme)

nullmodell <-
  lme(
    innvandring ~ 1,
    random = ~ 1 |
      cntry,
    data = noout_ESS9,
    na.action = "na.exclude",
    control = list(opt = "optim"),
    weights = ~ 1 / dweight
  )

summary(nullmodell)

library(multilevel)

gmean_nullmodell <- GmeanRel(nullmodell)
gmean_nullmodell$ICC
mean(gmean_nullmodell$MeanRel)

plot.lme(nullmodell,idResType = "pearsons")


# Vi inkluderer utdanningsår 

modell1 <- lme(
  innvandring ~ eduyrs,
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
  
)

summary(modell1)

modell2 <- lme(
  innvandring ~ eduyrs,
  data = noout_ESS9lang,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(modell2)

modell3 <- lme(
  innvandring ~ eduyrs + agea,
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)
summary(modell3)


modell4 <- lme(
  innvandring ~ eduyrs + agea + gdp,
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)
summary(modell4)

modell5 <- lme(
  innvandring ~ eduyrs + agea + gdp + happy,
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(modell5)

noout_ESS9$skandinavia <- NA

noout_ESS9$skandinavia[noout_ESS9$landgrupper == "Skandinavia"] <- 1
noout_ESS9$skandinavia[noout_ESS9$landgrupper != "Skandinavia"] <- 0


modell6 <- lme(
  innvandring ~ eduyrs + agea + happy + gdp + ef_2 + as.factor(landgrupper),
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(modell6)

library(car)

Anova(modell6)
VarCorr(modell6)

ICClme <-
  function(out) {
    varests <- as.numeric(VarCorr(out)[1:2])
    return(paste("ICC =", varests[1] / sum(varests)))
  }

ICClme(modell6)


modell7 <- lme(
  innvandring ~ eduyrs + agea + happy + gdp + ef_2 + skandinavia + happy*ef_2,
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(modell7)
VarCorr(modell7)
Anova(modell7)


modell6lang <- lme(
  innvandring ~ eduyrs + agea + gdp  + happy + ef_2,
  data = noout_ESS9lang,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  weights = ~ 1/dweight
)

summary(modell6lang)

library(car)

Anova(modell7lang)
VarCorr(modell7lang)

ICClme(modell7lang)

lang_varians <- aov(innvandring ~ as.factor(cntry), data = noout_ESS9lang)
kort_varians <- aov(innvandring ~ as.factor(cntry), data = noout_ESS9)

ICC1(lang_varians)
ICC2(lang_varians)

ICC1(kort_varians)
ICC2(kort_varians)


modell8 <-
  lm(
    innvandring ~ eduyrs + agea + happy,
    data = noout_ESS9,
    weights = dweight,
    na.action = "na.exclude"
  )

summary(modell8)
Anova(modell8)

# Vi endrer til lmer fra lme4-pakken. Den er vel så enkel å bruke, men gir 
# litt enklere oversikt.

nullmodell3 <-
  lmer(innvandring ~ (1 |
                        cntry) + (1 | landgrupper), data = noout_ESS9)

summary(nullmodell3)

VarCorr(nullmodell3) 

ranef(nullmodell3)

noout_ESS9$ef_2 <- noout_ESS9$ef_2 / 100

n3_modell_landgrupper <-
  lmer(
    innvandring ~ eduyrs + agea + happy + gdp + ef_2 + as.factor(landgrupper) + (1 |cntry) + (1 | landgrupper),
    weights = dweight,
    data = noout_ESS9)

summary(n3_modell_landgrupper)
Anova(n3_modell_landgrupper)
VarCorr(n3_modell_landgrupper)


n2_modell_land <-
  lmer(
    innvandring ~ eduyrs + agea + happy + gdp + ef_2 + skandinavia + (1 | cntry),
    weights = dweight,
    data = noout_ESS9)

summary(n2_modell_land)
Anova(n2_modell_land)


n2_modell_land <- confint.merMod(testmodell2)

write.table(n2_modell_land, "n2_modell_land.txt", sep = ",")


n2_modell_land_slope <-
  lmer(
    innvandring ~ eduyrs + agea + happy + gdp + ef_2 + eduyrs + skandinavia + (eduyrs | cntry),
    weights = dweight,
    data = noout_ESS9)

summary(n2_modell_land_slope)
Anova(n2_modell_land_slopen2_modell_land_slope)
AIC(n2_modell_land_slope)

n3_modell_land_ost <-
  lmer(
    innvandring ~ eduyrs + agea + happy + gdp + ef_2 + osteuropa +  (1 |
                                                                                   cntry) + (1 |
                                                                                               landgrupper),
    weights = dweight,
    data = noout_ESS9)

summary(n3_modell_land_ost)
Anova(n3_modell_land_ost)


modell9 <-
  lmer(
    innvandring ~ eduyrs + agea + gdp + happy + ef_2 + as.factor(landgrupper) + (1 | cntry),
    data = noout_ESS9,
    weights = dweight, 
    na.action =  "na.exclude"
  )

summary(modell9)
anova(modell9)
ICClme(modell9)
coef(modell9)

modell9_ci <- confint.merMod(modell9, level = 0.95)
modell9_ci

write.table(modell9_ci, file = "modell9_ci.txt", sep = ",")



modell10 <- lme(
  innvandring ~ eduyrs + agea + gdp + happy + ef_2,
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(modell10)
Anova(modell10)
coef(modell10)

# Samspillsledd

modell11 <- lme(
  innvandring ~ eduyrs + agea + happy + gdp + ef_2 + gdp * ef_2 + happy * ef_2,
  data = noout_ESS9,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(modell11)


tapply(noout_ESS9lang$innvandring, noout_ESS9lang$gndr, mean)
tapply(noout_ESS9lang$innvandring, noout_ESS9lang$landgruppe, mean)
tapply(noout_ESS9lang$innvandring, noout_ESS9lang$cntry, mean)
tapply(noout_ESS9$innvandring, noout_ESS9$ef_2, mean)


# Enkel analyse av valgte variabler ---------------------------------------

# Et blikk på variablene vi skal se på.

hist_innvandring <-
  ggplot(noout_ESS9, aes(x = innvandring, weight = dweight)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(noout_ESS9$innvandring, na.rm = TRUE),
                  sd = sd(noout_ESS9$innvandring, na.rm = TRUE)
                )) +
  geom_bar(aes(x = innvandring, weight = dweight, (y = (..count..) / sum(..count..))), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Relativ frekvens") +
  xlab("Samlet innvandringsvariabel") +
  theme_bw()
hist_innvandring

prop.table(table(noout_ESS9lang$innvandring)) * 100

# Alder

hist_agea <- ggplot(noout_ESS9, aes(x = agea, weight = dweight)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(noout_ESS9$agea, na.rm = TRUE),
                  sd = sd(noout_ESS9$agea, na.rm = TRUE)
                )) +
  geom_bar(aes(x = agea, weight = dweight, (y = (..count..) / sum(..count..))), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Relativ frekvens")+
  xlab("Alder")+
  theme_bw()

hist_agea

# Aldersvaribelen er nok nær nok til å anses som normalfordelt

hist_lykke <- ggplot(noout_ESS9, aes(x = happy, weight = dweight)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(noout_ESS9$happy, na.rm = TRUE),
                  sd = sd(noout_ESS9$happy, na.rm = TRUE)
                )) +
  geom_bar(aes(x = happy, weight = dweight, (y = (..count..) / sum(..count..))), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Relativ frekvens")+
  xlab("Lykke")+
  theme_bw()

hist_lykke


# For ESS1-8:

hist_imwbcnt_1_8 <-
  ggplot(ESS1_8e01, aes(x = imwbcnt, weight = dweight)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(ESS1_8e01$imwbcnt, na.rm = TRUE),
                  sd = sd(ESS1_8e01$imwbcnt, na.rm = TRUE)
                )) +
  geom_bar(aes(x = imwbcnt, weight = dweight, (y = (..count..) / sum(..count..))), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Relativ frekvens") +
  xlab("Immigrants make country worse or better place to live") +
  theme_bw()
hist_imwbcnt_1_8

prop.table(table(ESS1_8e01$imwbcnt)) * 100


# imsmetn: Allow many/few immigrants of same race/ethnic group as majority

hist_imsmetn_1_8 <-
  ggplot(ESS1_8e01, aes(x = imsmetn, weight = dweight)) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(ESS1_8e01$imsmetn, na.rm = TRUE),
                  sd = sd(ESS1_8e01$imsmetn, na.rm = TRUE)
                )) +
  geom_bar(aes(x = imsmetn, weight = dweight, (y = (..count..) / sum(..count..))), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Relativ frekvens") +
  xlab("Allow many/few immigrants of same race/ethnic group as majority") +
  theme_bw()
hist_imsmetn_1_8

prop.table(table(ESS1_8e01$imsmetn)) * 100

# Litt mer detaljert blikk

norsk_innvandring_eduyrs <-
  lm(innvandring ~ agea + eduyrs + ef_2, data = subset(noout_ESS9lang, cntry == "NO"))
summary(norsk_innvandring_eduyrs)

# Her kan vi også legge til flere andre punkter i subsettet for å finne mer
# nøyaktige data. For eksempel kan vi se på personer under 60

summary(lm(imwbcnt ~ agea + eduyrs, data = subset(ESS9, cntry == "DE" &
                                                    agea < 60)))

summary(lm(
  imwbcnt ~ agea + eduyrs,
  data = subset(ESS9, landgrupper == "Skandinavia" & gndr == 2)
))
# Her er det relevant at de er kvinner (2), da det er signifikant på 0.01 - nivå, som vist med *.
# Med andre ord vil kvinner bli mer kritiske til innvandring når de blir eldre, men med svært lite.

# Om vi gjør det samme for menn ser vi noe det samme.
summary(lm(
  imwbcnt ~ agea + eduyrs,
  data = subset(ESS9, landgrupper == "Skandinavia" & gndr == 1)
))


# Vi kan også legge til bosted dersom det er interessant
summary(lm(
  imwbcnt ~ agea + eduyrs,
  data = subset(ESS9, landgrupper == "Skandinavia" &
                  gndr == 1 & domicil == 1)
))

# Men aldersvariabelen er ikke lenger signifikant.


# Litt mer detaljert blikk på dataene -------------------------------------



# Hvordan ser det da altså ut i Skandinavia?

skandinavia_p_agea <-
  ggplot(
    subset(noout_ESS9, landgrupper == "Skandinavia"),
    aes(x = innvandring, weight = dweight)
  ) +
  stat_function(fun = dnorm,
                args = list(
                  mean = mean(noout_ESS9lang$innvandring, na.rm = TRUE),
                  sd = sd(noout_ESS9lang$innvandring, na.rm = TRUE)
                )) +
  geom_bar(aes(x = innvandring, weight = dweight, (y = (..count..) / sum(..count..))), alpha = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Relativ frekvens") +
  xlab("Immigrants make country worse or better place to live") +
  ggtitle("Skandinavia - innvandring") +
  theme_bw()
skandinavia_p_agea

prop.table(table(noout_ESS9lang$innvandring[noout_ESS9lang$landgrupper == "Skandinavia"])) *
  100


# Vi lager en graf for Skandinavere og alder

skandinavia_p_innvandring_agea <-
  ggplot(
    subset(noout_ESS9, landgrupper == "Skandinavia"),
    aes(agea, innvandring, weight = dweight)
  ) +
  geom_count() +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red",
    fill = "#CC79A7"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "green",
    fill = "#009E73"
  ) +
  xlab("Age of respondent") +
  ylab("Immigrants make country worse or better place to live") +
  ggtitle("Skandinavia - innvandring og age of respondent") +
  theme_bw()
skandinavia_p_innvandring_agea

# Samme for antall år fullført utdanning
skandinavia_p_innvandring_eduyrs <-
  ggplot(
    subset(noout_ESS9lang, landgrupper == "Skandinavia"),
    aes(eduyrs, innvandring, weight = dweight)
  ) +
  geom_count() +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red",
    fill = "#CC79A7"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "green",
    fill = "#009E73"
  ) +
  facet_grid(rows = vars(gndr)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  ggtitle("Skandinavia - innvandring og antall år utdanning") +
  theme_bw()

skandinavia_p_innvandring_eduyrs

# Flernivåmodeller --------------------------------------------------------

if (!require("multilevel"))
  install.packages("multilevel")
library(multilevel)

#https://federicovegetti.github.io/teaching/heidelberg_2018/lab/sst_lab_day2.html

# Uten random intercept
m1_norandom <-
  lm(innvandring ~  eduyrs + happy + agea + gdp + ef_2,
     data = noout_ESS9lang,
     weights = dweight)

summary(m1_norandom)
anova(m1_norandom)

# Vi kan teste for ulineratitet ved crPlots og CeresPlots. Dette gjør vi på
# modellen uten random effects.

ulinearitet <-
  crPlots(m1_norandom, 
          main = "innvandring ~  eduyrs + happy + agea + gdp, ingen random effekt", 
          weights = noout_ESS9lang$dweight)

# For ceresPlots kunne jeg ikke har med weights, noe som var litt dumt.
ceresPlots(m1_norandom, main = "innvandring ~  eduyrs + happy + agea + gdp, ingen random effekt", weights = noout_ESS9lang$dweight)

if (!require("gvlma"))
  install.packages("gvlma")
library(gvlma)
gvmodel1_norandom <- gvlma(m1_norandom)

summary(gvmodel1_norandom)

# dette er en veldig effektiv måte å gjøre det på, men den krever veldig stor
# plass til plottet. Dersom du øker størrelsen på det lille vinduet der plots
# vanligvis dukker opp vil alle plotsene dukke opp der etter litt tid. Men det
# tar nok et par minutt.

par(mar = rep(2, 4))

plot.gvlma(gvmodel1_norandom, weights = noout_ESS9lang$dweight)

# For å se variansen kan vi regne på det manuelt.

# VPC = Var(u0)/(Var(e)+Var(u0)) der Var(u0) er variansen for til nivå
# 2-restleddet og Var(e) er variansen til nivå 1.

# Dette kan vi også illustrere ved ICC, Intraclass Correlation Coefficient.

m1_1edition <-
  lmer(
    imwbcnt ~  eduyrs + happy + agea + inprdsc + ef_2 + (1 | essround),
    data = ESS1_8e01,
    weights = dweight,
    na.action = na.exclude
  )


summary(m1_1edition)
Anova(m1_1edition)

VarCorr(m1_1edition) %>%
  as_data_frame() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  select(grp, icc)

# Hvor mye av variansen kan forklares med gruppemedlemskap?
aov_landgrupper <-
  aov(innvandring ~ as.factor(landgrupper), data = noout_ESS9)
ICC1(aov_landgrupper)
ICC2(aov_landgrupper)


mult_landgrupper <- mult.icc(noout_ESS9lang[, c("gndr",
                                                "gdp",
                                                "innvandring",
                                                "eduyrs",
                                                "agea",
                                                "happy",
                                                "ef_2")], 
                             grpid = noout_ESS9lang$landgrupper)
mult_landgrupper

write.table(x =mult_landgrupper,
            file = "mult_landgrupper.txt", 
            sep = "\t")

# Denne tar en del tid.
innvandring_boot_icc <-
  boot.icc(noout_ESS9lang$innvandring[!is.na(noout_ESS9lang$innvandring)], 
            noout_ESS9lang$landgrupper[!is.na(noout_ESS9lang$innvandring)], 1000)

quantile(innvandring_boot_icc,c(0.05,.95))


# Vi tar utgangspunkt i modell6, men legger til 

modell6optim <- lme(
  innvandring ~ eduyrs + gdp + gndr + agea + ef_2,
  data = noout_ESS9lang,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  weights = ~1/dweight,
  control = list(opt="optim")
)


gmean_modell6 <- GmeanRel(modell6)
names(gmean_modell6)

# ICC-estimat for modell 6
gmean_modell6$ICC

# Gjennomsnitt for reliabilitet

mean(gmean_modell6$MeanRel)


gmean_modell6kort <- GmeanRel(modell6kort)

# ICC-estimat for modell 6 - kort
gmean_modell6kort$ICC

# Gjennomsnitt for reliabilitet

mean(gmean_modell6kort$MeanRel)


# Denne grafen viser hver landgruppe sitt gjennomsnittsvar på
# innvandringsvariabelen og linjen representerer en tilfeldig distribusjon der
# et likt antall pseudo-grupper som antallet grupper ble laget 1000 ganger og
# gjennomsnittsverdien på dem er da oppgitt. Den prikkete linjen er
# konfidensintervallen på 95 %.

library(multilevel)

innvandring_ran_landgrupper <- graph.ran.mean(
  x = noout_ESS9lang$innvandring,
  grpid =  noout_ESS9lang$landgrupper,
  nreps = 1000,
  bootci = TRUE,
  graph = FALSE
)

# Endre graph = FALSE til TRUE for å få en graf heller enn en tabell.
# Tabellen brukes for å skrives ut.

write.table(
  innvandring_ran_landgrupper,
  file = "innvandring_ran_landgrupper.txt",
  sep = ",",
  quote = FALSE,
  row.names = FALSE
)

# Hvis vi gjør det samme på landene ser vi at vi har truffet ganske bra på
# landgruppene, men det er uansett noen outliers i begge ender av skalaen.

innvandring_ran_land <- graph.ran.mean(
  x = noout_ESS9lang$innvandring,
  grpid = noout_ESS9lang$cntry,
  nreps = 1000,
  bootci = TRUE,
  graph = FALSE
)

write.table(
  innvandring_ran_land,
  file = "innvandring_ran_land.csv",
  sep = ",",
  quote = FALSE,
  row.names = FALSE
)


# Grafer ------------------------------------------------------------------

library(lattice)
lattice_eduyrs <- xyplot(
  innvandring ~ eduyrs |
    as.factor(landgrupper),
  data = noout_ESS9lang,
  type = c("p", "g", "r"),
  col = "dark blue",
  col.line = "black",
  xlab = "Antall år fullført utdanning",
  ylab = "Samlet innvandringsvariabel"
)
lattice_eduyrs



lattice_agea <- xyplot(
  innvandring ~ eduyrs + agea |
    as.factor(landgrupper),
  data = noout_ESS9lang,
  type = c("p", "g", "r"),
  col = "dark blue",
  col.line = "black",
  xlab = "Etnisk fragmentering",
  ylab = "Samlet innvandringsvariabel"
)

lattice_ef_2 <- xyplot(
  innvandring ~ ef_2 |
    as.factor(landgrupper),
  data = noout_ESS9,
  type = c("p", "g", "r"),
  col = "dark blue",
  col.line = "black",
  xlab = "Etnisk fragmentering",
  ylab = "Samlet innvandringsvariabel")

print(lattice_ef_2)

p1_eduyrs_lm_loess <-
  ggplot(subset(noout_ESS9, !is.na(noout_ESS9)), aes(eduyrs, innvandring, weight = dweight)) +
  geom_count() +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red",
    fill = "#CC79A7"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "green",
    fill = "#009E73"
  ) +
  facet_grid(rows = vars(landgrupper)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  xlim(1, 30) +
  ylim(1, 10) +
  theme_bw()
p1_eduyrs_lm_loess


p2_eduyrs_lm <-
  ggplot(ESS1_8e01, aes(eduyrs, imwbcnt, weight = dweight)) +
  geom_count() +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    fill  = "#CC79A7"
  ) +
  facet_grid(rows = vars(landgrupper)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  ggtitle("Utdanning sin påvirkning på holdninger til innvandring") +
  theme_bw()
p2_eduyrs_lm

# Fra hver utgave

p2_edition_lm <-
  ggplot(ESS1_8e01, aes(x = eduyrs, y = imwbcnt, weight = dweight)) +
  geom_count() +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    fill  = "#CC79A7"
  ) +
  facet_grid(rows = vars(essround)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  ggtitle("Utdanning sin påvirkning på holdninger til innvandring, hver enkelt utgave") +
  xlim(0, 50) +
  ylim(0, 10) +
  theme_bw()
p2_edition_lm


p3_edition_lm_skandinavia <-
  ggplot(
    subset(ESS1_8e01, landgrupper == "Skandinavia"),
    aes(eduyrs, imwbcnt, weight = dweight)
  ) +
  geom_count() +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red",
    fill  = "#CC79A7"
  ) +
  facet_grid(rows = vars(essround)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  ylim(0,10) +
  xlim(0,50)+
  ggtitle("Utdanning sin påvirkning på holdninger til innvandring, hver enkelt utgave - Skandinavia") +
  theme_bw()
p3_edition_lm_skandinavia

# Om vi deler inn i enda flere facets for å se på kjønn i tillegg til
# landgrupper kan det være interessant, men det vil være lite oversiktlig.
# Vi lager derfor noen nye grafer med mer informasjon på én graf.

p3_eduyrs_lm <-
  ggplot(ESS1_8e01, aes(eduyrs, imwbcnt, weight = dweight &
                          pweight)) +
  geom_count() +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "green",
    fill  = "red"
  ) +
  facet_grid(rows = vars(gndr)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  ggtitle("Utdanning sin påvirkning på holdninger til innvandring") +
  theme_bw()
p3_eduyrs_lm

p3_eduyrs <-
  ggplot(
    noout_ESS9,
    aes(
      x = eduyrs,
      y = innvandring,
      color = ef_2,
      weight = dweight
    )
  ) +
  geom_count() +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "lightgreen", alpha = 0.2) +
  facet_wrap(landgrupper~cntry) +
  ylim(0, 10) +
  ylab("Innvandring - samlevariabel") + 
  xlab("Antall år fullført utdanning")+
  ggtitle("Antall år fullført utdanning, innvandring og etnisk fragmentering, LM og Loess")+
  theme_bw()
p3_eduyrs

p4_agea <-
  ggplot(
    noout_ESS9,
    aes(
      x = agea,
      y = innvandring,
      color = ef_2,
      weight = dweight
    )
  ) +
  geom_count() +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  facet_wrap(landgrupper~cntry) +
  ylim(0, 10) +
  ylab("Innvandring - samlevariabel") + 
  xlab("Antall år fullført utdanning")+
  ggtitle("Alder, innvandring og etnisk fragmentering, loess")+
  theme_bw()
p4_agea

p5_agea <-
  ggplot(
    noout_ESS9,
    aes(
      x = agea,
      y = innvandring,
      color = ef_2,
      weight = dweight
    )
  ) +
  geom_count() +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  facet_wrap(landgrupper~cntry) +
  ylim(0, 10) +
  ylab("Innvandring - samlevariabel") + 
  xlab("Antall år fullført utdanning")+
  ggtitle("Alder, innvandring og etnisk fragmentering, loess")+
  theme_bw()
p4_agea

# Når vi skal ha land-variablene med blir det mange linjer samtidig.
# For å bedre se forskjell legger jeg til et sett med nye farger som
# skal være enkel å se, også for fargeblinde.

cbPalette <-
  c(
    "#000000",
    "#c2a5cf",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#b35806",
    "#e08214",
    "#fdb863",
    "#fee0b6",
    "#762a83",
    "#d8daeb",
    "#b2abd2",
    "#8073ac",
    "#542788",
    "#8c510a"
  )

p4_eduyrs_loess_cntry <-
  ggplot(
    noout_ESS9,
    aes(
      x = eduyrs,
      y = innvandring,
      color = cntry,
      weight = dweight
    )
  ) +
  scale_colour_manual(values=cbPalette)+
  geom_smooth(method = "lm", se = FALSE) +
  ylim(0,10) +
  xlab("Antall år fullført utdanning") +
  ylab("Innvandring - samlevariabel")+
  ggtitle("Forskjeller mellom land, antall år utdanning og innvandring") +
  theme_bw() 
p4_eduyrs_loess_cntry


p5_eduyrs <-
  ggplot(noout_ESS9,
         aes(
           x = ef_2,
           y = innvandring,
           fill = landgrupper,
           color = landgrupper,
           weight = dweight
         )) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw()
p5_eduyrs

p6_eduyrs_lm <-
  ggplot(ESS9,
         aes(
           x = eduyrs,
           y = imwbcnt,
           fill = gndr,
           color = gndr,
           weight = dweight
         )) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()
p6_eduyrs_lm


# Vi kan også dele dem inn i aldersgrupper for å få en litt renere graf, men
# ofrer nøyaktighet.

ESS9$aldersgruppe <-
  findInterval(ESS9$agea, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))
table(ESS9$aldersgruppe)

p7_aldersgruppe_loess <-
  ggplot(ESS9,
         aes(
           x = aldersgruppe,
           y = imwbcnt,
           fill = gndr,
           color = gndr,
           weight = dweight
         )) +
  geom_count() +
  geom_smooth(method = "loess", se = TRUE) +
  theme_bw()
p7_aldersgruppe_loess

p1_edition <-
  ggplot(ESS9, aes(x = agea, y = imwbcnt, color = edition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)
p1_edition

p2_land <-
  ggplot(ESS9, aes(x = agea, y = imwbcnt, color = landgrupper)) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()
p2_land

p1_eduyrs_lm_loess_landgrupper <-
  ggplot(noout_ESS9, aes(eduyrs, innvandring, weight = dweight, color = ef_2)) +
  geom_count() +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red",
    fill = "#CC79A7", show.legend = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "green",
    fill = "#009E73", show.legend = TRUE
  ) +
  facet_wrap(vars(landgrupper)) +
  xlab("Years of full-time education completed") +
  ylab("Innvandring - samlevariabel") +
  xlim(1, 30) +
  ylim(1, 10) +
  theme_bw()
p1_eduyrs_lm_loess_landgrupper

p1_eduyrs_lm_loess_gndr <-
  ggplot(noout_ESS9, aes(eduyrs, innvandring, weight = dweight, color = ef_2)) +
  geom_count() +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red",
    fill = "#CC79A7", show.legend = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "green",
    fill = "#009E73", show.legend = TRUE
  ) +
  facet_grid(rows = vars(gndr)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  xlim(1, 30) +
  ylim(1, 10) +
  ggtitle("Antall år utdanning og etnisk fragmentering sin påvirkning på innvandring. LM og Loess")+
  theme_bw()
p1_eduyrs_lm_loess_gndr

p1_eduyrs_lm_loess_domicil <-
  ggplot(noout_ESS9, aes(eduyrs, innvandring, weight = dweight, color = ef_2)) +
  geom_count() +
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = "red",
    fill = "#CC79A7", show.legend = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "green",
    fill = "#009E73", show.legend = TRUE
  ) +
  facet_grid(rows = vars(domicil)) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  xlim(1, 30) +
  ylim(1, 10) +
  theme_bw()
p1_eduyrs_lm_loess_domicil

p1_eduyrs_lm_cntry <-
  ggplot(noout_ESS9, aes(eduyrs, innvandring, weight = dweight)) +
  geom_count()+
  geom_smooth(noout_ESS9, aes(fill = cntry,
    method = "lm",
    se = TRUE, 
    show.legend = TRUE, 
    fullrange = TRUE
  )) +
  xlab("Years of full-time education completed") +
  ylab("Immigrants make country worse or better place to live") +
  xlim(1, 30) +
  ylim(1, 10) +
  theme_bw()
p1_eduyrs_lm_cntry


# ESS8 --------------------------------------------------------------------


# Vi lager et datasett for ESS8 uten outliers.

myvars <-
  c(
    "idno",
    "gndr",
    "cntry",
    "landgrupper",
    "domicil",
    "gdp",
    "imwbcnt",
    "imbgeco",
    "imueclt",
    "eduyrs",
    "agea",
    "happy",
    "ef_2",
    "dweight",
    "pweight"
  )


# Da jeg gjorde dette fant jeg ut at det ikke var forenelig med SPSS sin måte å
# sette labels på variablene sine. Jeg måtte derfor fjere dem ved hjelp av
# pakken "labelled". For sikkerhets skyld lager jeg et nytt datasett.

ESS8 <- subset(ESS1_8e01, ESS1_8e01$essround == 8)

noout_ESS8 <- ESS8[myvars]

rm(myvars)

# Mahal måler avstand mellom ytterpunktene, altså en person som har svart veldig
# annerledes enn alle andre. Eksempelvis en person som har svart veldig lavt på
# agea, men veldig høyt på eduyrs.

mahal <- mahalanobis(
  noout_ESS8[, c(6:13)],
  colMeans(noout_ESS8[, c(6:13)], na.rm = TRUE),
  cov(noout_ESS8[, c(6:13)], use = "pairwise.complete.obs")
)

summary(mahal)

# Viser scoren jeg må kutte bort svar som faller under. Her går jeg for 99%
# konfidensintervall, siden dette kun er gjennomgangen av datasettet.

cutoff <- qchisq(1 - .001, ncol(noout_ESS8[, c(6:12)]))
cutoff

# Vi sjekker hvor mange svar som har en score høyere enn cutoff-scoren. FALSE
# betyr at de er det og vi kan kutte deres svar.

summary(mahal < cutoff)

# Dersom vi ikke hadde inkludert GDP i mahalanobis-distansen og sett på cutoff
# da, ville vi bare kuttet 19 færre svar, men selv om gdp ikke er på
# individnivå, vil det påvirke svarene til individer og jeg velger dermed
# likevel å kutte 180 svar. Dersom vi inkluderer imueclt og imbgeco i tillegg
# til imwbcnt (som vi senere kan smelte sammen til én variabel), får vi 415.
# Foreløpig gjør vi det.


noout_ESS8 <- noout_ESS8[mahal < cutoff , ]

rm(mahal)

# vi sjekker for multikolinearitet, altså graden av sammenheng mellom
# forklaringsvariablene våre.

korrelasjon <-
  cor(noout_ESS8[, c(6:13)], use = "pairwise.complete.obs")
symnum(korrelasjon)

# vi har 0,6 i korrelasjon mellom de tre innvanddrings-variablene våre. Det
# tilsier at det skal gå greit å kombinere dem. Eller er det ingen problematisk
# korrelasjon.

# Vi kan presentere det i en graf

if (!require("corrplot"))
  install.packages("corrplot")
library(corrplot)

korrelasjonsplot <- corrplot(
  korrelasjon,
  type = "upper",
  order = "hclust",
  sig.level = 0.05,
  tl.pos = "td",
  method = "circle",
  tl.col = "black"
)


# assumption

random <- rchisq(nrow(noout_ESS8), df = 9)
fake <- lm(random ~ ., data = noout_ESS8[, c(6:12)])
fitted = scale(fake$fitted.values)
standardized = rstudent(fake)

# linearity
qqnorm(standardized)
abline(0, 1)

# vi ser at det ikke er helt lineært og vi burde derfor basere oss på non-linear models.
# Senere i skriptet ser vi både på lineære og ulineære modeller.

# normality
hist(standardized)

# Vi ønsker at det er sentrert over 0 - det er det, og at mesteparten av dataen
# er mellom -2 og 2, noe det er.

plot(fitted, standardized)
abline(0, 0)
abline(v = 0)

rm(fake, fitted, korrelasjon)

# også her ser vi at mesteparten av dataen er mellom -2 og 2, som er bra. Vi har veldig
# store mengder data, så selv om det er data som faller utenfor så har vi veldig mye data
# innenfor. Det skal også nevnes at det her er basert på tilfeldig data, basert på vår ekte
# data. Det skal ikke forandre seg drastisk fra gang til gang, men vil variere.

# Jeg har lyst til å se på de tre innvandringsvariablene i én variabel for en
# litt mer oversiktlig og kanskje mer nøyaktig analyse. Først sjekker vi hvordan
# de er i forhold til hverandre.

p_innvandring <- ggplot(data = noout_ESS8, aes(eduyrs)) +
  geom_smooth(aes(y = imwbcnt, color = "Immigrants make country worse or better"),
              method = "gam") +
  geom_smooth(aes(y = imbgeco, color = "Immigrants are bad or good for the economy"),
              method = "gam") +
  geom_smooth(
    aes(y = imueclt, color = "Country's cultural life undermined or enriched by immigrants"),
    method = "gam"
  ) +
  ylim(0, 10) +
  ylab("Skala fra 0-10") +
  xlab("Antall år fullført utdanning") +
  ggtitle("Innvandringsvariablene og utdanningsår. GAM-smoothing uten outliers.") +
  theme_bw()
p_innvandring

# De er ganske like i utgangspunktet.

# Vi kombinerer de tre innvandringsvariablene til én.

noout_ESS8$innvandring <-
  (noout_ESS8$imwbcnt + noout_ESS8$imueclt + noout_ESS8$imbgeco)/3

# Vi kan også smelte dem sammen og med det beholde verdiene, men i én variabel
# heller enn tre. Det betyr at for hver person har man tatt svarene deres på 
# imwbcnt,imbgeco og imueclt og samlet dem med svarene på de andre spørsmålene.
# Det gir en enkel måte å se på de tre svarene samlet, men uten å endre på dem.

if (!require("reshape2"))
  install.packages("reshape2")
library(reshape2)

names(noout_ESS8)

noout_ESS8lang <- melt(
  noout_ESS8,
  id.vars = c(
    "idno",
    "gndr",
    "cntry",
    "landgrupper",
    "domicil",
    "gdp",
    "eduyrs",
    "agea",
    "happy",
    "ef_2",
    "dweight",
    "pweight"
  ),
  measure.vars = c("imwbcnt", "imbgeco", "imueclt")
)

names(noout_ESS8lang)[14] <- "innvandring"

# Modellering uten outliers ------------------------------------------------------

library(nlme)

nullmodell <-
  lme(
    innvandring ~ 1,
    random = ~ 1 |
      landgrupper,
    data = noout_ESS8,
    na.action = "na.exclude",
    control = list(opt = "optim"),
    weights = ~ 1 / dweight
  )

summary(nullmodell)

library(multilevel)

gmean_nullmodell <- GmeanRel(nullmodell)
gmean_nullmodell$ICC
mean(gmean_nullmodell$MeanRel)

plot.lme(nullmodell)

# Flernivå og eduyrs som uavhengig

ess8_modell1 <- lme(
  innvandring ~ eduyrs,
  data = noout_ESS8,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
  
)

summary(ess8_modell1)

ess8_modell2 <- lme(
  innvandring ~ eduyrs,
  data = noout_ESS8lang,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(ess8_modell2)

ess8_modell3 <- lme(
  innvandring ~ eduyrs + agea,
  data = noout_ESS8,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)
summary(ess8_modell3)


ess8_modell4 <- lme(
  innvandring ~ eduyrs + agea + gdp,
  data = noout_ESS8,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)
summary(ess8_modell4)

ess8_modell5 <- lme(
  innvandring ~ eduyrs + gdp + gndr,
  data = noout_ESS8,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)
summary(ess8_modell5)

ess8_modell6 <- lme(
  innvandring ~ eduyrs + agea + gdp + gndr + happy,
  data = noout_ESS8,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(ess8_modell6)

noout_ESS8$skandinavia <- NA
noout_ESS8$skandinavia[noout_ESS8$landgrupper == "Skandinavia"] <- 1
noout_ESS8$skandinavia[noout_ESS8$landgrupper != "Skandinavia"] <- 0

ess8_modell7 <- lme(
  innvandring ~ eduyrs + agea + happy + gdp + ef_2 + skandinavia,
  data = noout_ESS8,
  na.action = "na.exclude",
  random = ~ 1 | cntry,
  control = list(opt = "optim"),
  weights = ~ 1 / dweight
)

summary(ess8_modell7)

library(car)

Anova(ess8_modell7)
VarCorr(ess8_modell7)

ICClme <-
  function(out) {
    varests <- as.numeric(VarCorr(out)[1:2])
    return(paste("ICC =", varests[1] / sum(varests)))
  }

ICClme(ess8_modell7)


ess8_modell7lang <- lme(
  innvandring ~ eduyrs + agea + gdp + gndr + happy + ef_2,
  data = noout_ESS8lang,
  na.action = "na.exclude",
  random = ~ 1 | landgrupper,
  weights = ~ 1/dweight
)

summary(ess8_modell7lang)

library(car)

Anova(ess8_modell7lang)
VarCorr(ess8_modell7lang)

ICClme(ess8_modell7lang)

lang_varians <- aov(innvandring ~ as.factor(landgrupper), data = noout_ESS8lang)
kort_varians <- aov(innvandring ~ as.factor(landgrupper), data = noout_ESS8)

ICC1(lang_varians)
ICC2(lang_varians)

ICC1(kort_varians)
ICC2(kort_varians)


ess8_modell8 <-
  lm(
    innvandring ~ eduyrs + agea + gdp + gndr + happy + ef_2,
    data = noout_ESS8lang,
    weights = dweight,
    na.action = "na.exclude"
  )

summary(ess8_modell8)
Anova(ess8_modell8)

# Vi endrer til lmer fra lme4-pakken. Den er vel så enkel å bruke,

ess8_modell9 <-
  lmer(
    innvandring ~ eduyrs + agea + gdp + gndr + happy + ef_2 + (1 |
                                                                 landgrupper),
    data = noout_ESS8,
    weights = dweight, 
    na.action =  "na.exclude"
  )

summary(ess8_modell9)
Anova(ess8_modell9)


tapply(noout_ESS8lang$innvandring, noout_ESS8lang$gndr, mean)
tapply(noout_ESS8lang$innvandring, noout_ESS8lang$landgruppe, mean)
tapply(noout_ESS8lang$innvandring, noout_ESS8lang$cntry, mean)
tapply(noout_ESS8$innvandring, noout_ESS8$ef_2, mean)
