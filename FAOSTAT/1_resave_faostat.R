source('../0_settings.R')

library(tools)
library(dplyr)

countries <- read.csv(file.path(prefix, "GRP", "DataTables", 
                                "GRP_Countries.csv"), stringsAsFactors=FALSE)
countries$Country_Name[countries$Country_Name == 'Sudan'] <- 'Sudan (former)'
countries$Country_Name[countries$Country_Name == 'Ivory Coast'] <- "Côte d'Ivoire"

reformat_fao_data <- function(faofile, d) {
    d <- read.csv(file.path(prefix, "GRP", "FAOSTAT", 
                            faofile))
    save(d, file=file.path(prefix, "GRP", "FAOSTAT", 
                           paste0(file_path_sans_ext(faofile), ".RData")))
    names(d) <- gsub('[.]', '', names(d))
    d$ISO3 <- countries$ISO3[match(d$Country, countries$Country_Name)]
    d <- filter(d, ISO3 %in% countries$ISO3)
    save(d, file=file.path(prefix, "GRP", "FAOSTAT", 
                           paste0(file_path_sans_ext(faofile), "_GRP.RData")))
    write.csv(d, file=file.path(prefix, "GRP", "FAOSTAT", 
                           paste0(file_path_sans_ext(faofile), "_GRP.csv")), 
              row.names=FALSE)
}

reformat_fao_data("Production_Crops_E_All_Data.csv")
reformat_fao_data("Value_of_Production_E_All_Data.csv")
reformat_fao_data("Trade_Crops_Livestock_E_All_Data.csv")
