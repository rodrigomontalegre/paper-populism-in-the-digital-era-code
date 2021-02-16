#Paper code
#By Rodrigo Mont'Alegre

library(data.table)
library(ggplot2)
library(writexl)
if (Sys.info()["user"] == "Rodrigo") {
  setwd("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/POL60700 Modul Democracy in the digital age/Paper/data")
}



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

cn_2020 <- merge(cn_2020,
                 vdem_2019_dem,
                 by.x = "country",
                 by.y = "country_name",
                 all = FALSE)

unique(cn_2020$country)

pop_dt <- fread("populism_dataset.csv",
                encoding = "UTF-8")

names(pop_dt) <- tolower(gsub(" ", "_", names(pop_dt)))

pop_dt[16, "country"] <- "United States of America"

pop_dt <- pop_dt[years_in_office %like% "-$"]


#democratic_countries <- vdem_2019_dem$country_text_id
