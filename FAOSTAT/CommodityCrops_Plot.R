source('../0_settings.R')

library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(foreach)

load(file.path(prefix, "GRP", "FAOSTAT",
               "Production_Crops_E_All_Data.RData"))

load(file.path(prefix, "GRP", "FAOSTAT", 
               "Value_of_Production_E_All_Data.RData"))

load(file.path(prefix, "GRP", "FAOSTAT", 
               "Trade_Crops_Livestock_E_All_Data.RData"))


countries <- c('Eritrea',
               'Djibouti',
               'Ethiopia',
               'Somalia',
               'Uganda',
               'Kenya')

names(p) <- gsub('[.]', '', names(p))
names(v) <- gsub('[.]', '', names(v))

# Eliminate footnote columns from value and production data
p_filtered <- filter(p, Country %in% countries) %>% select(-ends_with('F'))
v_filtered  <- filter(v, Country %in% countries) %>% select(-ends_with('F'))

# Eliminate unneeded ElementGroup column from trade data
t_filtered <-filter(t, Country %in% countries) %>% select(-ElementGroup, -Flag)

# Melt value and production data to long format
p_filtered <- tbl_df(melt(p_filtered,
                          id.vars=c("CountryCode", "Country", "ItemCode", 
                                    "Item", "ElementCode", "Element", "Unit"),
                          variable.name="Year", value.name="Value"))
v_filtered <- tbl_df(melt(v_filtered,
                          id.vars=c("CountryCode", "Country", "ItemCode", 
                                    "Item", "ElementCode", "Element", "Unit"), 
                          variable.name="Year", value.name="Value"))

d <- rbind(p_filtered, v_filtered)
d$Year <- as.numeric(gsub('Y', '', d$Year))

# Ensure d and t_filtered have same columns, albeit in different order
stopifnot(ncol(d) == ncol(t_filtered))
stopifnot(names(d) %in% names(t_filtered))

# Make d and t_filtered column order match
t_filtered <- t_filtered[match(names(t_filtered), names(d))]
stopifnot(names(d) == names(t_filtered))

d <- rbind(d, t_filtered)

d$Country <- factor(d$Country)

# Filter out production indices and totals:
d <- filter(d, !grepl('Total', Item), !grepl('PIN', Item))
# Remove cereals total as rice milled equivalent
d <- filter(d, !ItemCode == 1817)
# Filter out local currencies
d <- filter(d, !grepl('SLC', Element))

# Ensure that all values within each element are measured in the same units:
unit_check <- filter(d, !is.na(Value), Value > 0) %>%
    group_by(Country, Element, Year) %>%
    mutate(n_units=length(unique(Unit)))
stopifnot(all(unit_check$n_units == 1))

filter(d, !is.na(Value), Value > 0) %>%
    group_by(Country, Element, Year) %>%
    mutate(rank=abs(rank(Value, ties.method="min") - n()) + 1) %>%
    filter(rank <= 5) %>%
    group_by(Country, Element) %>%
    filter(Year == max(Year)) %>%
    arrange(Country, Element, rank) -> top5

filter(d, !is.na(Value), Value > 0) %>%
    group_by(Element, Year, ItemCode, Item) %>%
    summarise(Value=sum(Value, na.rm=TRUE)) %>%
    group_by(Element, Year) %>%
    mutate(rank=abs(rank(Value, ties.method="min") - n()) + 1) %>%
    filter(rank <= 5) %>%
    group_by(Element) %>%
    filter(Year == max(Year)) %>%
    arrange(Element, rank) -> top5_Horn

# Relabel for prettier plots
top5$Item <- as.character(top5$Item)
top5$Item[top5$Item == "Rice – total  (Rice milled equivalent)"] <- "Rice"
top5$Item[top5$Item == "Sugar Raw Centrifugal"] <- "Sugar raw"
top5$Item[top5$Item == "Vegetables, fresh nes"] <- "Vegetables"

top5_Horn$Item <- as.character(top5_Horn$Item)
top5_Horn$Item[top5_Horn$Item == "Rice – total  (Rice milled equivalent)"] <- "Rice"
top5_Horn$Item[top5_Horn$Item == "Sugar Raw Centrifugal"] <- "Sugar raw"
top5_Horn$Item[top5_Horn$Item == "Vegetables, fresh nes"] <- "Vegetables"

###############################################################################
### Plot Imports
ps <- foreach(country=levels(top5$Country)) %do% {
    this_d <- filter(top5, Element == "Import Value", Country == country)
    ggplot(this_d,
           aes(reorder(Item, rank), Value/1000, fill=factor(rank))) +
        theme_bw(base_size=8) +
        geom_bar(stat="identity", position="dodge") +
        ggtitle(country) +
        guides(fill=FALSE) +
        scale_fill_manual(values=rev(brewer.pal(5,"Blues"))) +
        ylab("Value (US$, millions)") +
        theme(axis.title.x=element_blank())
}
p <- arrangeGrob(ps[[1]], ps[[2]], ps[[3]], ps[[4]], ps[[5]], ps[[6]], 
                 ncol=3)
ggsave(paste0('imports_HornCountries.png'), p, dpi=300, width=8, height=6)
ggsave(paste0('imports_HornCountries.eps'), p, dpi=300, width=8, height=6)

ggplot(filter(top5_Horn, Element == "Import Value"),
       aes(reorder(Item, rank), Value/1000, fill=factor(rank))) +
    theme_bw() +
    geom_bar(stat="identity", position="dodge") +
    guides(fill=FALSE) +
    scale_fill_manual(values=rev(brewer.pal(5,"Blues"))) +
    ylab("Value (US$, millions)") +
    theme(axis.title.x=element_blank())
ggsave(paste0('imports_HornAll.png'), dpi=300, width=8, height=6)
ggsave(paste0('imports_Horn.eps'), dpi=300, width=8, height=6)

###############################################################################
### Plot Exports
ps <- foreach(country=levels(top5$Country)) %do% {
    this_d <- filter(top5, Element == "Export Value", Country == country)
    ggplot(this_d,
           aes(reorder(Item, rank), Value/1000, fill=factor(rank))) +
        theme_bw(base_size=8) +
        geom_bar(stat="identity", position="dodge") +
        ggtitle(country) +
        guides(fill=FALSE) +
        scale_fill_manual(values=rev(brewer.pal(5,"Blues"))) +
        ylab("Value (US$, millions)") +
        theme(axis.title.x=element_blank())
}
p <- arrangeGrob(ps[[1]], ps[[2]], ps[[3]], ps[[4]], ps[[5]], ps[[6]], 
                 ncol=3)
ggsave(paste0('exports_HornCountries.png'), p, dpi=300, width=8, height=6)
ggsave(paste0('exports_HornCountries.eps'), p, dpi=300, width=8, height=6)

ggplot(filter(top5_Horn, Element == "Export Value"),
       aes(reorder(Item, rank), Value/1000, fill=factor(rank))) +
    theme_bw() +
    geom_bar(stat="identity", position="dodge") +
    guides(fill=FALSE) +
    scale_fill_manual(values=rev(brewer.pal(5,"Blues"))) +
    ylab("Value (US$, millions)") +
    theme(axis.title.x=element_blank())
ggsave(paste0('exports_HornAll.png'), dpi=300, width=8, height=6)
ggsave(paste0('exports_Horn.eps'), dpi=300, width=8, height=6)
