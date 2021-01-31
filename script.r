#Paper code
#By Rodrigo Mont'Alegre

library(data.table)
library(ggplot2)



if (Sys.info()["user"] == "Rodrigo") {
  setwd("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/POL60700 Modul Democracy in the digital age/Paper/data")
}

cn <- fread("coronanet_release.csv")

vdem <- fread("V-Dem-CY-Core-v10.csv")

vdem_2019 <- vdem[year == "2019", .(country_name,
                                    country_text_id,
                                    year,
                                    histname,
                                    v2elmulpar_osp,
                                    v2elfrfair_osp,
                                    v2x_polyarchy,
                                    v2cltrnslw_osp, #transparent law enforcement
                                    v2clacjstm_osp,
                                    v2clacjstw_osp,
                                    v2x_liberal
                                    )]

vdem_2019$regime_type <- NA

for(i in 1:nrow(vdem_2019)) {
  if(vdem_2019[i, 5] > 2 
     && vdem_2019[i, 6] > 2 
     && vdem_2019[i, 7] > 5 
     && vdem_2019[i, 8] > 3 
     && vdem_2019[i, 9] > 3 
     && vdem_2019[i, 9] > 3 
     && vdem_2019[i, 10] > 0.8) {
    vdem_2019$regime_type[i] <- "liberal_democracy"
  }
  else if

