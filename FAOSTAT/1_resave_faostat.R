source('../0_settings.R')

library(tools)
library(dplyr)

countries <- read.csv(file.path(prefix, "GRP", "DataTables", 
                                "GRP_Countries.csv"), stringsAsFactors=FALSE)

# Read the FAOSTAT numeric country code to ISO3 mapping table
fao_iso_key <- read.csv(file.path(prefix, "GRP", "FAOSTAT", "FAOSTAT_ISO_CODES.csv"))

reformat_fao_data <- function(faofile, d) {
    d <- read.csv(file.path(prefix, "GRP", "FAOSTAT", 
                            faofile))
    save(d, file=file.path(prefix, "GRP", "FAOSTAT", 
                           paste0(file_path_sans_ext(faofile), ".RData")))
    names(d) <- gsub('[.]', '', names(d))
    # Filter out CountryCode values above 5000 as these are aggregated metrics
    d <- filter(d, CountryCode < 5000)
    d <- left_join(d, select(fao_iso_key, FAOSTAT, ISO3), by=c("CountryCode"="FAOSTAT"))
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
