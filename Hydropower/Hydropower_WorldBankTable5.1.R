library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)

d <- read.csv("World_Bank_Table5.1.csv")

data.frame(Basin=d$Basin, Low_loss=d$PV_CC_Low - d$PV_No_CC)
data.frame(Basin=d$Basin, Low_gain=d$PV_CC_High - d$PV_No_CC)

d <- melt(d, value.name="PV")
d <- filter(d, Basin %in%c("Eastern Nile", "Nile Equatorial Lakes"))

d$variable <- ordered(d$variable,
                      levels=c("PV_No_CC", "PV_CC_Low", "PV_CC_High"),
                      labels=c("No climate change",
                               "Worst climate scenario",
                               "Best climate scenario"))

# This is plotting the present value of planned hydro and irrigation expansion 
# revenues over the 2015 to 2050 period in billions of US $
ggplot(d, aes(variable, PV, fill=Basin)) +
    theme_bw() +
    geom_bar(stat="identity", position="dodge") +
    theme(axis.title.x=element_blank(),
          legend.position=c(.4, .83)) +
    ylab("Present value (US$, billions)") +
    scale_fill_brewer()
ggsave("Horn_hydro_PV_under_cc.png", height=4, width=6, dpi=400)
ggsave("Horn_hydro_PV_under_cc.eps", height=4, width=6, dpi=400)
