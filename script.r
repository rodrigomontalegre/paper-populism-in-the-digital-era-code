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

covid_19_ts_us <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")


#Modifying covid_19_ts
covid_19_ts <- melt(covid_19_ts, 
                    id.vars = c("Province/State", 
                                "Country/Region", 
                                "Lat", 
                                "Long"), 
                    variable.name = "day", 
                    value.name ="n_infections"
                    )

covid_19_ts <- covid_19_ts[, c("Lat", "Long") := NULL]

colnames(covid_19_ts)[1:2] <- c("province_or_state", 
                                "country_or_region")

covid_19_ts[, n_infections := sum(n_infections), by = list(country_or_region, day)]

covid_19_ts[, province_or_state := NULL]

covid_19_ts <- unique(covid_19_ts, by = c("country_or_region", 
                                          "day"))

#modifying covid_19_ts_us
covid_19_ts_us[, c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Lat", "Long_", "Combined_Key") := NULL]

covid_19_ts_us <- melt(covid_19_ts_us, id.vars = c("Province_State", 
                                                   "Country_Region"), 
                       variable.name = "day", 
                       value.name = "n_infections")

colnames(covid_19_ts_us)[1:2] <- c("province_or_state", 
                                "country_or_region")

covid_19_ts_us[, n_infections := sum(n_infections), by = list(country_or_region, 
                                                              day)]

covid_19_ts_us <- unique(covid_19_ts_us, by = c("country_or_region", 
                                          "day"))

covid_19_ts_us[, province_or_state := NULL]

for(i in 1:nrow(covid_19_ts_us)){
  covid_19_ts_us$country_or_region[i] <- "United States of America"
}

#Concatenating the global and us datasets and standardizing country names
covid_19 <- rbind(covid_19_ts, covid_19_ts_us)

for(i in 1:nrow(covid_19)){
  if(covid_19[i, country_or_region] == "Cabo Verde"){
    covid_19$country_or_region[i] <- "Cape Verde"
  } else if(covid_19[i, country_or_region] == "Czechia"){
    covid_19$country_or_region[i] <- "Czech Republic"
  } else if(covid_19[i, country_or_region] == "Cote d'Ivoire"){
    covid_19$country_or_region[i] <- "Ivory Coast"
  } else if(covid_19[i, country_or_region] == "Korea, South"){
    covid_19$country_or_region[i] <- "South Korea"
  } else if(covid_19[i, country_or_region] == "Taiwan*"){
    covid_19$country_or_region[i] <- "Taiwan"
  } else if(covid_19[i, country_or_region] == "Gambia"){
    covid_19$country_or_region[i] <- "The Gambia" 
  }
}

covid_19 <- covid_19[country_or_region %in% democratic_countries]

covid_19 <- setorder(covid_19, country_or_region)

covid_19$day <- as.factor(covid_19$day)

covid_19$day <- strptime(as.character(covid_19$day), "%m/%d/%y")