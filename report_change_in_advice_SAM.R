## Prepare plots and tables for report

## Before: .RData in output folder
## After: plots in report/change_in_advice

rm(list=ls())

# Libraries
library(TAF)
library(ggplot2)

# Directories
mkdir("report/change_in_advice")
output.dir <- "report/change_in_advice/"

#==================================================================
# Load change in advice tables 
#==================================================================

load("output/change_in_advice.RData")

# Settings
ay <- 2026 # this year's assessment/intermediate year

#==================================================================
# Compare forecast summaries
#==================================================================

#  combine forecast assumptions
dat <- rbind(res_new[["summary"]],res_old[["summary"]])

# plot
png(paste0(output.dir,"Change in advice - forecast assumptions.png"),width = 11, height = 7, units = "in", res = 300)

p1 <- ggplot(dat,aes(x=Year,y=Value,colour=WG,shape=Type))+geom_point(size=3)+
  facet_wrap(~Variable,scales="free_y")+
  theme_bw(base_size=14)+labs(x="",y="",colour="",shape="")+ 
  scale_shape_manual(values=c(16, 2, 0))+ylim(0,NA)
print(p1)
dev.off()

#==================================================================
# Forecast stockwts 
#==================================================================

# combine results
dat <- rbind(res_new[["mean weights"]],res_old[["mean weights"]])

png(paste0(output.dir,"Change in advice - stock mean weights.png"),width = 11, height = 7, units = "in", res = 300)

p1 <- ggplot(data=dat, aes(x=Age, y=stock.wt,colour=WG,group=WG)) + 
  facet_wrap(~Year,nrow = 2)+ geom_point()+ylim(0,NA)+
  geom_line() + theme_bw(base_size=14)+ labs(colour="",x="",y="mean weight (kg)")
print(p1)
dev.off()

#==================================================================
# Forecast selectivity 
#==================================================================

# combine results
dat <- rbind(res_new[["selectivity"]],res_old[["selectivity"]])

png(paste0(output.dir,"Change in advice - selectivity.png"),width = 11, height = 7, units = "in", res = 300)

p1 <- ggplot(data=dat, aes(x=Age, y=sel,colour=WG,group=WG)) + 
  geom_point()+ geom_line() + facet_wrap(~Year)+ylim(0,NA)+
  theme_bw(base_size=14)+ labs(colour="",x="",y="Selectivity") 
print(p1)
dev.off()

#==================================================================
# Forecast N at age
#==================================================================

# combine results
dat <- rbind(res_new[["natage"]],res_old[["natage"]])

# restrict number of years
dat <- dat[dat$Year >=(ay-2),]

png(paste0(output.dir,"Change in advice - Stock numbers-at-age.png"),width = 11, height = 7, units = "in", res = 300)

p1 <- ggplot(dat,aes(x=Age,y=N,group=interaction(Type,WG),colour=WG,shape=Type))+ 
  geom_line()+geom_point(size=3)+labs(colour="",y="Abundance (thousands)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw(base_size=14)+ ylim(0,NA)+
  scale_shape_manual(values=c(16, 2, 0))

print(p1)
dev.off()

#==================================================================
# Forecast Ratio of N at age
#==================================================================

dat.now <- res_new[["natage"]][,c("Year","Age","N")]
dat.prev <- res_old[["natage"]][,c("Year","Age","N")]

colnames(dat.now)[colnames(dat.now) %in% "N"] <- "N_now"
colnames(dat.prev)[colnames(dat.prev) %in% "N"] <- "N_prev"

# Select overlapping years
comp.yrs <- intersect(dat.now$Year,dat.prev$Year)
dat.prev <- dat.prev[dat.prev$Year %in% comp.yrs,]
dat.now <- dat.now[dat.now$Year %in% comp.yrs,]

# Find ratio
dat <- merge(dat.now,dat.prev)
dat$ratio <- dat$N_now/dat$N_prev

png(paste0(output.dir,"Change in advice - Stock numbers-at-age ratio"),width = 11, height = 7, units = "in", res = 300)

p1 <- ggplot(dat,aes(x = Age, y = rev(Year))) +
  geom_tile(aes(fill = ratio), alpha = 0.7, show.legend = FALSE) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue",
                       midpoint = 1, na.value = "grey") +
  geom_text(aes(label = sprintf(ratio, fmt = "%.1f")), size = 3.5) +
  labs(x = "Age", y = "Year") + theme_bw(base_size=14)
print(p1)
dev.off()

#==================================================================
# Forecast Biomass at age
#==================================================================

# combine results
dat <- rbind(res_new[["batage"]],res_old[["batage"]])

# restrict number of years
dat <- dat[dat$Year >=(ay-2),]

png(paste0(output.dir,"Change in advice - Stock biomass-at-age.png"),width = 11, height = 7, units = "in", res = 300)

p1 <- ggplot(dat,aes(x=Age,y=biomass,group=interaction(Type,WG),colour=WG,shape=Type))+ 
  geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (tonnes)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw(base_size=14)+ ylim(0,NA)+
  scale_shape_manual(values=c(16, 2, 0))

print(p1)
dev.off()

#==================================================================
# Forecast Ratio of Biomass at age
#==================================================================

dat.now <- res_new[["batage"]][,c("Year","Age","biomass")]
dat.prev <- res_old[["batage"]][,c("Year","Age","biomass")]

colnames(dat.now)[colnames(dat.now) %in% "biomass"] <- "B_now"
colnames(dat.prev)[colnames(dat.prev) %in% "biomass"] <- "B_prev"

# Select overlapping years
comp.yrs <- intersect(dat.now$Year,dat.prev$Year)
dat.prev <- dat.prev[dat.prev$Year %in% comp.yrs,]
dat.now <- dat.now[dat.now$Year %in% comp.yrs,]

# Find ratio
dat <- merge(dat.now,dat.prev)
dat$ratio <- dat$B_now/dat$B_prev

png(paste0(output.dir,"Change in advice - Stock biomass-at-age ratio"),width = 11, height = 7, units = "in", res = 300)

p1 <- ggplot(dat,aes(x = Age, y = rev(Year))) +
  geom_tile(aes(fill = ratio), alpha = 0.7, show.legend = FALSE) +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkblue",
                       midpoint = 1, na.value = "grey") +
  geom_text(aes(label = sprintf(ratio, fmt = "%.1f")), size = 3.5) +
  labs(x = "Age", y = "Year") + theme_bw(base_size=14)
print(p1)
dev.off()


