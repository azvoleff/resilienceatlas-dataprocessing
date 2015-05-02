source('../0_settings.R')

p <- read.csv(file.path(prefix, "GRP", "FAOSTAT", 
                        "Production_Crops_E_All_Data.csv"))
save(p, file=file.path(prefix, "GRP", "FAOSTAT", 
                       "Production_Crops_E_All_Data.RData"))

v <- read.csv(file.path(prefix, "GRP", "FAOSTAT", 
                        "Value_of_Production_E_All_Data.csv"))
save(v, file=file.path(prefix, "GRP", "FAOSTAT", 
                       "Value_of_Production_E_All_Data.RData"))

t <- read.csv(file.path(prefix, "GRP", "FAOSTAT", 
                        "Trade_Crops_Livestock_E_All_Data.csv"))
save(t, file=file.path(prefix, "GRP", "FAOSTAT", 
                       "Trade_Crops_Livestock_E_All_Data.RData"))
