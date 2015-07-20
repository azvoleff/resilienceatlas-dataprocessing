source('../0_settings.R')

library(dplyr)

in_folder <- file.path(prefix, "GRP", "SPAM")
out_folder <- file.path(prefix, "GRP", "SPAM")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

# Function to ensure allocation IDs are properly formatted for joins
format_alloc_ID <- function(d) {
    d$alloc_key <- sprintf('%08i', as.numeric(as.character(d$alloc_key)))
    d <- rename(d, alloc_id=alloc_key)
    d
}

AfricaA <- read.csv(file.path(in_folder, "Sub-Saharan Africa", "spam2005V2r0_SSA_A.csv"))
AfricaH <- read.csv(file.path(in_folder, "Sub-Saharan Africa", "spam2005V2r0_SSA_H.csv"))
AfricaP <- read.csv(file.path(in_folder, "Sub-Saharan Africa", "spam2005V2r0_SSA_P.csv"))
AfricaV <- read.csv(file.path(in_folder, "Sub-Saharan Africa", "spam2005V2r0_SSA_V.csv"))
AfricaY <- read.csv(file.path(in_folder, "Sub-Saharan Africa", "spam2005V2r0_SSA_Y.csv"))

AsiaA <- read.csv(file.path(in_folder, "Asia", "spam2005V2r0_ASIA_A.csv"))
AsiaH <- read.csv(file.path(in_folder, "Asia", "spam2005V2r0_ASIA_H.csv"))
AsiaP <- read.csv(file.path(in_folder, "Asia", "spam2005V2r0_ASIA_P.csv"))
AsiaV <- read.csv(file.path(in_folder, "Asia", "spam2005V2r0_ASIA_V.csv"))
AsiaY <- read.csv(file.path(in_folder, "Asia", "spam2005V2r0_ASIA_Y.csv"))

A <- rbind(AfricaA, AsiaA)
H <- rbind(AfricaH, AsiaH)
P <- rbind(AfricaP, AsiaP)
V <- rbind(AfricaV, AsiaV)
Y <- rbind(AfricaY, AsiaY)

A <- format_alloc_ID(A)
H <- format_alloc_ID(H)
P <- format_alloc_ID(P)
V <- format_alloc_ID(V)
Y <- format_alloc_ID(Y)

# grp_list <- read.csv(file.path(prefix, "GRP", "DataTables", "GRP_Countries.csv"))
#
# stopifnot(all(grp_list$ISO3 %in% A$iso3))
# grp_list$ISO3 %in% A$iso3
# grp_list$ISO3[!(grp_list$ISO3 %in% A$iso3)]

# A <- filter(A, iso3 %in% grp_list$ISO3)
# H <- filter(H, iso3 %in% grp_list$ISO3)
# P <- filter(P, iso3 %in% grp_list$ISO3)
# V <- filter(V, iso3 %in% grp_list$ISO3)
# Y <- filter(Y, iso3 %in% grp_list$ISO3)

write.csv(A, file.path(out_folder, "spam2005V2r0_GRP_PhysicalArea.csv"), row.names=FALSE)
write.csv(H, file.path(out_folder, "spam2005V2r0_GRP_HarvestedArea.csv"), row.names=FALSE)
write.csv(P, file.path(out_folder, "spam2005V2r0_GRP_Production.csv"), row.names=FALSE)
write.csv(V, file.path(out_folder, "spam2005V2r0_GRP_ValueOfProduction.csv"), row.names=FALSE)
write.csv(Y, file.path(out_folder, "spam2005V2r0_GRP_Yield.csv"), row.names=FALSE)

