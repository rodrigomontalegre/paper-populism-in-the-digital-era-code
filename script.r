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

#Merging and calculating the daily number

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

covid_19$day <- as.Date(as.character(covid_19$day), format = "%m/%d/%y")

covid_19 <- covid_19[day %between% c("2020/01/01", "2020/12/30")]

#Calculating the daily number

covid_19 <- covid_19[, daily := n_infections - shift(n_infections), by = country_or_region]

#Adding population information

population <- fread("WPP2019_TotalPopulationBySex.csv", encoding = "UTF-8")

population <- population[Time == 2020 & Variant == "Medium", list(Location, Time, PopTotal, Variant)]

population[, Variant := NULL]

pop_kosovo <- data.table(Location = "Kosovo", Time = 2020, PopTotal = 1794.248) #Kosovo population data is from 2019 but assigned 2020 for script

population <- rbind(population, pop_kosovo)

for(i in 1:nrow(population)){
  if(population[i, Location] == "China, Taiwan Province of China"){
    population$Location[i] <- "Taiwan"
  } else if(population[i, Location] == "Gambia"){
    population$Location[i] <- "The Gambia" 
  } else if(population[i, Location] == "Cabo Verde"){
    population$Location[i] <- "Cape Verde"
  } else if(population[i, Location] == "Côte d'Ivoire"){
    population$Location[i] <- "Ivory Coast"
  } else if(population[i, Location] == "Czechia"){
    population$Location[i] <- "Czech Republic"
  } else if(population[i, Location] == "Republic of Korea"){
    population$Location[i] <- "South Korea"
  } else if(population[i, Location] == "Republic of Moldova"){
    population$Location[i] <- "Moldova"
  }
}
  
population <- population[Time == 2020 & Location %in% democratic_countries, list(Location, Time, PopTotal)]

pop_dem <- unique(population$Location)

sort(pop_dem) == sort(democratic_countries)

#Merging population and covid-19 datasets into dt and calculating per daily infections per 100,000 inhabitants

dt <- merge(covid_19, population, by.x = "country_or_region", by.y = "Location")

dt[, per_cap:= round(((daily / PopTotal * 1000) / 100), digits = 3)] #infections per 1000 people

dt[, per_cap_weekly_avg := round(frollmean(per_cap, n = 7, fill = NA), digits = 3), by = country_or_region] #rolling weekly average of infections per 1000 people

##############################################################
#Standardizing and merging COVID-19 dataset and v-dem dataset#
##############################################################

dt_2 <- merge(vdem_2019_dem, 
              dt, 
              by.x = "country_name", 
              by.y = "country_or_region"
              )

dt_2[, c("country_text_id", 
         "histname", 
         "year", 
         "Time") := NULL]

dt_2[, pop_percent := round((n_infections / (PopTotal * 1000) * 100), digits = 2)]

dt_3 <- dt_2[, list(average_n_infections = mean(n_infections, na.rm = TRUE),
                    median_n_infections = median(n_infections, na.rm = TRUE),
                    sd_n_infections = sd(n_infections, na.rm = TRUE),
                    average_daily = mean(daily, na.rm = TRUE),
                    median_daily = median(daily, na.rm = TRUE),
                    sd_daily = sd(daily, na.rm = TRUE), 
                    average_per_cap = mean(per_cap, na.rm = TRUE), 
                    median_per_cap = median(per_cap, na.rm = TRUE), 
                    average_weekly = mean(per_cap_weekly_avg, na.rm = TRUE)), 
             by = populist] #summary statistics by populist and non-populist

dt_2[, c("daily_average_per_group", 
         "daily_average_pop_percent") := list(round(mean(daily, na.rm = TRUE), digits = 2), 
                                              round(mean(pop_percent), digits = 2)), 
        by = c("populist", "day")]

#############################################################################################
#Calculating time to implement policy in days compared to first case and 35 cases per 100000#
#############################################################################################

cn_2020_populist[, c("record_id", 
                     "update_level", 
                     "ISO_A3", 
                     "ISO_A2", 
                     "init_country_level", 
                     "domestic_policy", 
                     "province", "ISO_L2", 
                     "city", 
                     "institution_status", 
                     "index_high_est", 
                     "index_med_est", 
                     "index_low_est", 
                     "index_country_rank") := NULL]

keycol <- c("country", "date_start")

setorderv(cn_2020_populist, keycol)

dt_4 <- cn_2020_populist[, head(.SD, 1L), by = c("country", "type")]

cn_2020_npopulist[, c("record_id", 
                      "update_level", 
                      "ISO_A3", 
                      "ISO_A2", 
                      "init_country_level", 
                      "domestic_policy", 
                      "province", 
                      "ISO_L2", 
                      "city", 
                      "institution_status", 
                      "index_high_est", 
                      "index_med_est", 
                      "index_low_est", 
                      "index_country_rank") := NULL]

setorderv(cn_2020_npopulist, keycol)

dt_5 <- cn_2020_npopulist[, head(.SD, 1L), by = c("country", "type")]

date_first_case <- dt_2[which(n_infections != 0)][, min(day), by = country_name]

date_35per100k_case <- dt_2[which(per_cap > 0.35)][, min(day), by = country_name]

colnames(date_first_case)[2] <- "first_case"

colnames(date_35per100k_case)[2] <- "cases35per100k"

dates_cases <- merge(date_first_case,
                     date_35per100k_case,
                     by = "country_name")

dt_4 <- merge(dt_4, 
              dates_cases,
              by.x = "country",
              by.y = "country_name")

dt_4[, c("diff_1_policy_days", 
         "diff_35per100k_policy_days") := (as.numeric(date_start) - as.numeric(first_case)), (as.numeric(date_start) - as.numeric(cases35per100k))] #second column something needs to be changed

dt_5 <- merge(dt_5,
              dates_cases,
              by.x = "country",
              by.y = "country_name")

dt_5[, diff_1_policy_days := as.numeric(date_start) - as.numeric(first_case)]

dt_6 <- dt_4[, list(average_diff_1_policy_days = mean(diff_1_policy_days),
                    median_diff_1_policy_days = median(diff_1_policy_days),
                    min = min(diff_1_policy_days),
                    max = max(diff_1_policy_days)), by = type] #summary stats for populsits

dt_6[order(type)]

dt_7 <- dt_5[, list(average_diff_1_policy_days = mean(diff_1_policy_days),
                    median_diff_1_policy_days = median(diff_1_policy_days),
                    min = min(diff_1_policy_days),
                    max = max(diff_1_policy_days)), by = type] #summary stats for non-populists
dt_7[order(type)]

################
#Creating plots#
################

plot_1 <- ggplot(dt_2, aes(x = day, 
                           y = daily_average_per_group, 
                           color = populist)) + 
  geom_point(size = 0.7,
             alpha = 0.7) +
  geom_smooth() +
  labs(title = "Populists vs. Non-Populists (Daily Average)",
       x = "Day",
       y = "Average Number of Cases") +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3),
        panel.background = element_blank()) +
  scale_color_manual(name = "Government Type",
                     labels = c("Non-Populist", "Populist"), 
                     values = c("red", "blue"))

plot_2 <- ggplot(dt_2, aes(x = day, 
                           y = daily_pop_percent, 
                           color = populist)) +
  geom_point(size = 0.7,
             alpha = 0.7) +
  geom_smooth
  labs(title = "Percentage of Total Cases in Population",
       x = "Day",
       y = "Cases versus Population in Percent") +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3),
        panel.background = element_blank()) +
  scale_color_manual(name = "Government Type",
                     labels = c("Non-Populist", "Populist"), 
                     values = c("red", "blue"))


