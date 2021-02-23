############
#Paper code#
########################
#By Rodrigo Mont'Alegre#
########################


library(data.table)
library(ggplot2)
library(writexl)
library(curl)
library(tidyr)


if (Sys.info()["user"] == "Rodrigo") {
  setwd("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/POL60700 Modul Democracy in the digital age/Paper/data")
}

###########################################
#Importing and modifying the v-dem dataset#
###########################################

vdem <- fread("V-Dem-CY-Full+Others-v10.csv",
              sep = ",",
              encoding = "UTF-8")


vdem_2019 <- vdem[year == "2019", .(country_name,
                                    country_text_id,
                                    year,
                                    histname,
                                    v2x_regime
                                    )]

vdem_2019$regime_type <- NA

for(i in 1:nrow(vdem_2019)) {
  if(vdem_2019[i, "v2x_regime"] == 3){
    vdem_2019$regime_type[i] <- "liberal_democracy"
  }else if(vdem_2019[i, "v2x_regime"] == 2){
    vdem_2019$regime_type[i] <- "electoral_democracy"
  }else if(vdem_2019[i, "v2x_regime"] == 1){
    vdem_2019$regime_type[i] <- "electoral_autocracy"
  }else if(vdem_2019[i, "v2x_regime"] == 0){
    vdem_2019$regime_type[i] <- "closed_autocracy"
  }
}

vdem_2019_dem <- vdem_2019[vdem_2019$v2x_regime == 3 | vdem_2019$v2x_regime == 2] #all countries classified as a type of democracy

vdem_2019_aut <- vdem_2019[vdem_2019$v2x_regime == 1 | vdem_2019$v2x_regime == 0] #all countries classified as a type of autocracy

pop_dt <- fread("populism_dataset.csv",
                encoding = "UTF-8")

names(pop_dt) <- tolower(gsub(" ", "_", names(pop_dt)))

pop_dt[16, "country"] <- "United States of America"

pop_dt <- pop_dt[years_in_office %like% "-$"]

populist_governments <- pop_dt$country

vdem_2019_dem$populist <- NA

for(i in 1:nrow(vdem_2019_dem)){ 
  if(vdem_2019_dem[i, "country_name"] %in% populist_governments){
  vdem_2019_dem[i, "populist"] <- as.integer(1)
  }else{
  vdem_2019_dem[i, "populist"] <- as.integer(0)
  }
}


#################################################
#Reading in and formatting the CoronaNet dataset#
#################################################

cn <- fread("coronanet_release.csv")

cn_2020 <- cn[date_announced <= "2020-12-31" 
              & date_start <= "2020-12-31" 
              & init_country_level == "National"]

for(i in 1:nrow(cn_2020)) {
  if(cn_2020[i, country == "Czechia"]){
    cn_2020$country[i] <- "Czech Republic"
  }else if(cn_2020[i, country == "Gambia"]){
    cn_2020$country[i] <- "The Gambia"
  }else if(cn_2020[i, country == "Timor Leste"]){
    cn_2020$country[i] <- "Timor-Leste"
  }
}

unique(cn_2020$country)

democratic_countries <- vdem_2019_dem$country_name

cn_2020 <- cn_2020[country %in% democratic_countries] #only democratic countries as classified by the regimes of the world dataset

#Now splitting the coronaNet data into populist and non-populist lead countries

cn_2020_populist <- cn_2020[country %in% populist_governments]

cn_2020_npopulist <- cn_2020[!(country %in% populist_governments)]

##################################################
#Importing and modfiying COVID-19 timeseries data#
##################################################

covid_19_ts <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid_19_ts <- melt(covid_19_ts, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "day", value.name ="n_infections")

covid_19_ts <- covid_19_ts[, c("Lat", "Long") := NULL]

colnames(covid_19_ts)[1:2] <- c("province_or_state", "country_or_region")

covid_19_ts[, n_infections := sum(n_infections), by = list(country_or_region, day)]

covid_19_ts[, province_or_state := NULL]

covid_19_ts <- unique(covid_19_ts, by = c("country_or_region", "day"))

unique(cn$country)
