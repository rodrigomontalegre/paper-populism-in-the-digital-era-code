############
#Paper code#
########################
#By Rodrigo Mont'Alegre#
########################


library(data.table)
library(ggplot2)
library(curl)
library(tidyr)
library(gt)

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
         "daily_average_pop_percent",
         "daily_median_per_group") := list(round(mean(daily, na.rm = TRUE), digits = 2), 
                                           round(mean(pop_percent, na.rm = TRUE), digits = 2),
                                           round(median(daily, na.rm = TRUE), digits = 2)), 
        by = c("populist", "day")]

dt_2[, n_average := round(mean(n_infections, na.rm = TRUE), digits = 2), by = c("populist", "day")]

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


dt_4[, diff_1_policy_days := round(as.numeric(date_start) - as.numeric(first_case), 2)]

dt_4[, diff_35per100k_policy_days := round(as.numeric(date_start) - as.numeric(cases35per100k), 2)]

dt_5 <- merge(dt_5,
              dates_cases,
              by.x = "country",
              by.y = "country_name")

dt_5[, diff_1_policy_days := round(as.numeric(date_start) - as.numeric(first_case), 2)]

dt_5[, diff_35per100k_policy_days := round(as.numeric(date_start) - as.numeric(cases35per100k), 2)]

dt_6 <- dt_4[, list(p_avg_fc = round(mean(diff_1_policy_days), 2),
                    p_med_fc = round(median(diff_1_policy_days), 2),
                    p_sd_fc = round(sd(diff_1_policy_days), 2),
                    p_min_fc = min(diff_1_policy_days),
                    p_max_fc = max(diff_1_policy_days),
                    p_avg_35 = round(mean(diff_35per100k_policy_days), 2),
                    p_med_35 = round(median(diff_35per100k_policy_days), 2),
                    p_sd_35 = round(sd(diff_35per100k_policy_days), 2),
                    p_min_35 = min(diff_35per100k_policy_days),
                    p_max_35 = max(diff_35per100k_policy_days)), by = type] #summary stats for populists

dt_6[order(type)]

dt_7 <- dt_5[, list(np_avg_fc = round(mean(diff_1_policy_days), 2),
                    np_med_fc = round(median(diff_1_policy_days), 2),
                    np_sd_fc = round(sd(diff_1_policy_days), 2),
                    np_min_fc = min(diff_1_policy_days),
                    np_max_fc = max(diff_1_policy_days),
                    np_avg_35 = round(mean(diff_35per100k_policy_days), 2),
                    np_med_35 = round(median(diff_35per100k_policy_days), 2),
                    np_sd_35 = round(sd(diff_35per100k_policy_days), 2),
                    np_min_35 = min(diff_35per100k_policy_days),
                    np_max_35 = max(diff_35per100k_policy_days)), by = type] #summary stats for non-populists
dt_7[order(type)]

dt_days <- merge(dt_6,
                 dt_7,
                 by = "type")

setcolorder(dt_days, c("type", 
                       "p_avg_fc", 
                       "np_avg_fc", 
                       "p_med_fc", 
                       "np_med_fc", 
                       "p_sd_fc", 
                       "np_sd_fc", 
                       "p_min_fc", 
                       "np_min_fc", 
                       "p_max_fc", 
                       "np_max_fc", 
                       "p_avg_35", 
                       "np_avg_35", 
                       "p_med_35", 
                       "np_med_35", 
                       "p_sd_35", 
                       "np_sd_35", 
                       "p_min_35", 
                       "np_min_35", 
                       "p_max_35", 
                       "np_max_35"))

dt_days[, 6:11 := NULL][, 10:15 := NULL]

dt_days[p_avg_fc > np_avg_fc]

dt_days[p_med_fc < np_med_fc]

dt_days[p_med_35 > np_med_35]

dt_days[p_avg_35 < np_avg_35]

################
#Creating plots#
################

plot_1 <- ggplot(dt_2, aes(x = day,
                           y = daily_average_per_group,
                           color = populist)) +
  geom_line(size = 1, alpha = 1) +
  labs(title = "Daily Average of Cases in 2020",
       x = "Day",
       y = "Number of Cases") +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3), 
        panel.background = element_rect(fill = "white", 
                                        color = "grey", 
                                        size = 1), 
        panel.grid.major.y = element_line(size = 1, 
                                          linetype = "dashed", 
                                          color = "grey"),
        panel.grid.major.x = element_line(),
        legend.position = c(0.15, 0.85)) +
  scale_color_manual(name = "Government Type",
                     labels = c("Not populist", "Populist"), 
                     values = c("red", "blue")) 


plot_2 <- ggplot(dt_2, aes(x = day,
                           y = daily_median_per_group,
                           color = populist)) +
  geom_line(size = 1, alpha = 1) +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3), 
        panel.background = element_rect(fill = "white", 
                                        color = "grey", 
                                        size = 1), 
        panel.grid.major.y = element_line(size = 1, 
                                          linetype = "dashed", 
                                          color = "grey"),
        panel.grid.major.x = element_line(),
        legend.position = c(0.15, 0.85)) +
  labs(title = "Daily Median of Cases in 2020",
       x = "Day",
       y = "Number of Cases") +
  scale_color_manual(name = "Government Type",
                     labels = c("Not populist", "Populist"), 
                     values = c("red", "blue"))


plot_3 <- ggplot(dt_2, aes(x = day, 
                           y = daily_average_pop_percent, 
                           color = populist)) +
  geom_line(size = 1,
             alpha = 1) +
  labs(title = "Percentage of Total Cases in Population",
       x = "Day",
       y = "Cases versus Population in Percent") +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3), 
        panel.background = element_rect(fill = "white", 
                                        color = "grey", 
                                        size = 1), 
        panel.grid.major.y = element_line(size = 1, 
                                          linetype = "dashed", 
                                          color = "grey"),
        panel.grid.major.x = element_line(),
        legend.position = c(0.15, 0.75)) +
  scale_color_manual(name = "Government Type",
                     labels = c("Not populist", "Populist"), 
                     values = c("red", "blue"))

table_1 <- gt(dt_days) %>% tab_header(
  title = "From Threshold to Implementation",
  subtitle = "Number of days between first case, 35 per 100,000, and policy start")

########################
#Wilcoxon Rank Sum Test#
########################

#preparing the dataset

dt_names <- unique(dt_2[, list(country_name, populist)])

dt_testing <- rbind(dt_4, dt_5)

dt_testing <- dt_testing[, list(country, 
                                type, 
                                date_start, 
                                first_case, 
                                cases35per100k, 
                                diff_1_policy_days, 
                                diff_35per100k_policy_days)]

dt_testing <- merge(dt_testing,
                    dt_names,
                    by.x = "country",
                    by.y = "country_name") #datatable with relevant information for wilcoxon test

dt_test_fc <- dt_days[, list(type, p_avg_fc, np_avg_fc)]

dt_test_fc$p_value <- NA

for(i in 1:nrow(dt_test_fc)){
  if(dt_test_fc[i, type] == "Anti-Disinformation Measures"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                          data = dt_testing[type == "Anti-Disinformation Measures"])$p.value
  }else if(dt_test_fc[i, type] == "Closure and Regulation of Schools"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                          data = dt_testing[type == "Closure and Regulation of Schools"])$p.value
  }else if(dt_test_fc[i, type] == "Curfew"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Curfew"])$p.value
  }else if(dt_test_fc[i, type] == "Declaration of Emergency"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Declaration of Emergency"])$p.value
  }else if(dt_test_fc[i, type] == "External Border Restrictions"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "External Border Restrictions"])$p.value
  }else if(dt_test_fc[i, type] == "Health Monitoring"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Health Monitoring"])$p.value
  }else if(dt_test_fc[i, type] == "Health Resources"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Health Resources"])$p.value
  }else if(dt_test_fc[i, type] == "Health Testing"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Health Testing"])$p.value
  }else if(dt_test_fc[i, type] == "Hygiene"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Hygiene"])$p.value
  }else if(dt_test_fc[i, type] == "Internal Border Restrictions"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Internal Border Restrictions"])$p.value
  }else if(dt_test_fc[i, type] == "Lockdown"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Lockdown"])$p.value
  }else if(dt_test_fc[i, type] == "New Task Force, Bureau or Administrative Configuration"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "New Task Force, Bureau or Administrative Configuration"])$p.value
  }else if(dt_test_fc[i, type] == "Other Policy Not Listed Above"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Other Policy Not Listed Above"])$p.value
  }else if(dt_test_fc[i, type] == "Public Awareness Measures"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Public Awareness Measures"])$p.value
  }else if(dt_test_fc[i, type] == "Quarantine"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Quarantine"])$p.value
  }else if(dt_test_fc[i, type] == "Restriction and Regulation of Businesses"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Restriction and Regulation of Businesses"])$p.value
  }else if(dt_test_fc[i, type] == "Restriction and Regulation of Government Services"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Restriction and Regulation of Government Services"])$p.value
  }else if(dt_test_fc[i, type] == "Restrictions of Mass Gatherings"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Restrictions of Mass Gatherings"])$p.value
  }else if(dt_test_fc[i, type] == "Social Distancing"){
    dt_test_fc$p_value[i] <- wilcox.test(diff_1_policy_days ~ populist, 
                                         data = dt_testing[type == "Social Distancing"])$p.value
  }
}

table_2 <- gt(dt_test_fc) %>% tab_header(
  title = "P-Values for Averages between Populists and Non Populists",
  subtitle = "Average number of days between policy start and first confirmed case"
)

dt_test_35per100k <- dt_days[, list(type, p_avg_35, np_avg_35)]

dt_test_35per100k$p_value <- NA

for(i in 1:nrow(dt_test_35per100k)){
  if(dt_test_35per100k[i, type] == "Anti-Disinformation Measures"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Anti-Disinformation Measures"])$p.value
  }else if(dt_test_35per100k[i, type] == "Closure and Regulation of Schools"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Closure and Regulation of Schools"])$p.value
  }else if(dt_test_35per100k[i, type] == "Curfew"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Curfew"])$p.value
  }else if(dt_test_35per100k[i, type] == "Declaration of Emergency"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Declaration of Emergency"])$p.value
  }else if(dt_test_35per100k[i, type] == "External Border Restrictions"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "External Border Restrictions"])$p.value
  }else if(dt_test_35per100k[i, type] == "Health Monitoring"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Health Monitoring"])$p.value
  }else if(dt_test_35per100k[i, type] == "Health Resources"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Health Resources"])$p.value
  }else if(dt_test_35per100k[i, type] == "Health Testing"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Health Testing"])$p.value
  }else if(dt_test_35per100k[i, type] == "Hygiene"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Hygiene"])$p.value
  }else if(dt_test_35per100k[i, type] == "Internal Border Restrictions"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Internal Border Restrictions"])$p.value
  }else if(dt_test_35per100k[i, type] == "Lockdown"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Lockdown"])$p.value
  }else if(dt_test_35per100k[i, type] == "New Task Force, Bureau or Administrative Configuration"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "New Task Force, Bureau or Administrative Configuration"])$p.value
  }else if(dt_test_35per100k[i, type] == "Other Policy Not Listed Above"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Other Policy Not Listed Above"])$p.value
  }else if(dt_test_35per100k[i, type] == "Public Awareness Measures"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Public Awareness Measures"])$p.value
  }else if(dt_test_35per100k[i, type] == "Quarantine"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Quarantine"])$p.value
  }else if(dt_test_35per100k[i, type] == "Restriction and Regulation of Businesses"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Restriction and Regulation of Businesses"])$p.value
  }else if(dt_test_35per100k[i, type] == "Restriction and Regulation of Government Services"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Restriction and Regulation of Government Services"])$p.value
  }else if(dt_test_35per100k[i, type] == "Restrictions of Mass Gatherings"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Restrictions of Mass Gatherings"])$p.value
  }else if(dt_test_35per100k[i, type] == "Social Distancing"){
    dt_test_35per100k$p_value[i] <- wilcox.test(diff_35per100k_policy_days ~ populist, 
                                         data = dt_testing[type == "Social Distancing"])$p.value
  }
}

table_3 <- gt(dt_test_35per100k) %>% tab_header(
  title = "P-Values for Averages between Populists and Non Populists",
  subtitle = "Average number of days between policy start and first date of 35 cases per 100,000 inhabitants")
