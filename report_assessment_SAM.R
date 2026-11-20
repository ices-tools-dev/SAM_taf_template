## Extract results of interest, report tables

## Before: sam fit object in model/assessment
## After: csv tables of assessment output in report/assessment
##        Additional plots and tables, as made in the SAM assessments in
##        ices-taf, use data/*.csv

# Libraries
library(TAF)
library(stockassessment)

# Directories
mkdir("report/assessment")

# load fit
fit <- readRDS("model/assessment/fit.rds")

# load retro
RETRO <- readRDS("model/assessment/retro_fit.rds")

# leave one out runs
LO <- readRDS("model/assessment/leaveoneout.rds")

# load residuals
RES <- readRDS("model/assessment/residuals.rds")
RESP <- readRDS("model/assessment/procres.rds")

#define colours
colours<- palette.colors(palette="Okabe_Ito")

###########################################################################################

# plot input stock weights at age
sw <- (fit$data$stockMeanWeight)*1000

png(paste('report/assessment/input_stockweights.png',sep=""),  width=2800, height=2300, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
matplot(fit$data$years, sw, type="l", lty="solid", lwd=2,col=colours[1:dim(sw)[2]], xlab="Year", bty="l",ylab="Stock weights (g)")
legend("topleft", legend=as.character(colnames(sw)), lwd=2, lty=1, bty="n",ncol=3,col=colours[1:dim(sw)[2]])
dev.off()

#plot input maturity
mat <- (fit$data$propMat)

png(paste('report/assessment/input_maturity.png',sep=""),  width=2800, height=2300, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
matplot(fit$data$years, mat, type="l", lty="solid", lwd=2, col=colours[1:dim(mat)[2]], xlab="Year", bty="l",ylab="Proportion mature") 
        legend("topleft", legend=as.character(colnames(mat)), lwd=2, lty=1, bty="n",ncol=3, col=colours[1:dim(mat)[2]])

dev.off()

#plot input natural mortality
mor <- (fit$data$natMor)

png(paste('report/assessment/input_naturalmortality.png',sep=""),  width=2800, height=2300, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
matplot(fit$data$years, mor, type="l", lty="solid", lwd=2, col=colours[1:dim(mor)[2]], xlab="Year", bty="l",ylab="Natural mortality") 
legend("topleft", legend=as.character(colnames(mor)), lwd=2, lty=1, bty="n",ncol=3, col=colours[1:dim(mor)[2]])
dev.off()

###############################################################################################################

#plot results
#biomass proportions at age

png('report/assessment/totalbiomass at age prop.png',  width=3000, height=2500, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
barplot(prop.table(t(fit$data$stockMeanWeight*ntable(fit)),margin=2),xlab="Year",ylab="Proportion Total stock biomass at age", space=0,legend=T,col=colours[1:dim(sw)[2]], args.legend = list(x = "topleft"))
dev.off()

png('report/assessment/SSB at age prop.png',  width=3000, height=2500, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
barplot(prop.table(t(fit$data$stockMeanWeight*fit$data$propMat*ntable(fit)),margin=2),ylab="Proportion SSB at age", xlab="Year",space=0,legend=T,col=colours[1:dim(sw)[2]], args.legend = list(x = "topleft"))
dev.off()


# stock summary
taf.png("report/assessment/Stocksummary", width=2200, height=1500)
par(mfrow=c(2,2),mar = c(4,4,1,1))
catchplot(fit,xlab="Year",las=0)
recplot(fit,xlab="Year",las=0,drop=1,addCI=TRUE)
fbarplot(fit,xlab="Year",partial=F,addCI=TRUE)
ssbplot(fit,addCI=T,xlab="Year", las=0)
dev.off()

#process residuals
attr(RESP, 'fleetNames')[[2]]<- c("Joint sample residuals log(F)") 
taf.png("report/assessment/Process Residuals",width=1600,height=1500)
plot(RESP)
dev.off()

#observation residuals
taf.png("report/assessment/Obs Residuals",width=1300,height=1300)
plot(RES)
dev.off()

#observed vs predicted values for each fleet
for(f in 1:fit$data$noFleets){
  taf.png(paste("report/assessment/Fleet_",f,"_observed_predicted_fleet_",f,sep=""))
  stockassessment::fitplot(fit, fleets=f)
  dev.off()
}

# leave one out runs
taf.png("report/assessment/Leaveoneout", width=2200, height=1500)
par(mfrow=c(2,2),mar = c(5,5,1,1))
catchplot(LO, xlab="Year")
recplot(LO, xlab="Year", drop=1)
fbarplot(LO, xlab="Year")
ssbplot(LO,xlab="Year")
dev.off()

# retros
# decide for each variable whether final year is included in mohn's rho calculation
mlag0<-stockassessment::mohn(RETRO, lag=0)
mlag1<-stockassessment::mohn(RETRO, lag=1)

taf.png("report/assessment/Retro")
par(mfrow=c(2,2),mar = c(4,4,1,1))
catchplot(RETRO,xlab="Year",las=0)
recplot(RETRO,xlab="Year", las=0, drop=1)
legend("topright", legend=round(mlag1[1],3), bty="n")
fbarplot(RETRO, las=0, drop=1, xlab="Year")
legend("topright", legend=round(mlag1[3],3), bty="n")
ssbplot(RETRO,xlab="Year", las=0, drop=0)
legend("topright", legend=round(mlag0[2],3), bty="n")
dev.off()

#spanwing stock recruit realtionship
taf.png("report/assessment/SR")
srplot(fit)
dev.off()

# SD in log(R)
taf.png("report/assessment/sdR")
idx <- names(fit$sdrep$value) == "logR"
sdLogR<-fit$sdrep$sd[idx]
plot(fit$data$years,sdLogR[1:fit$data$noYears], ylim=c(0,0.5),xlab="Year",ylab="sdLogR", pch=16)
dev.off()

# plot recruits/SSB

ssb<-summary(fit)[,"SSB"]
rec<-ntable(fit)[,1]
taf.png("report/assessment/Rec_per_SSB.png")
plot(fit$data$years,log(rec/ssb),type='b',xlab="Year",ylab="ln(Recruits/SSB) ",cex.lab=1.5)
dev.off()


# Selectivity of the Fishery
taf.png("report/assessment/Fig_Fishselectivity")
sel <- t(faytable(fit)/fbartable(fit)[,1])
sel[is.na(sel)]<-0
op <- par(mfrow=c(3,3), mai=c(0.4,0.4,0.3,0.3), oma=c(3,3,0,0))
age.sel<-as.integer(rownames(sel))
for(i in round(seq(1,dim(sel)[2],length=9))){
  plot(age.sel, sel[,i], type="l", xlab="", ylab="", lwd=1.5, ylim=c(0,max(sel)))
  if (i+1<dim(sel)[2])try(lines(age.sel, sel[,i+1], col="red", lwd=1.5))
  if (i+2<dim(sel)[2]) try(lines(age.sel, sel[,i+2], col="blue", lwd=1.5))
  if (i+3<dim(sel)[2]) try(lines(age.sel, sel[,i+3], col="green", lwd=1.5))
  legend("topleft",paste(c(colnames(sel)[i],colnames(sel)[i+1],colnames(sel)[i+2],colnames(sel)[i+3])), lty=rep(1,4), col=c("black","red","blue","green"),bty="n")
}
mtext("Age", 1, outer=T, line=1)
mtext("F/Fbar", 2, outer=T, line=1)

dev.off()


taf.png("report/assessment/Fig_partial_F")
F<-faytable(fit)
matplot(rownames(F),F,lty=1:ncol(F),col=colours[1:ncol(F)], type='l', xlab='Year', lwd=3)
legend('topright', col=colours[1:ncol(F)], lty=1:ncol(F), legend=colnames(F), bty='n', lwd=3)

dev.off()


###########################################################################################################
#Tables

# F at age
faytab <- faytable(fit)
ftab<-round(faytab,3)
ftab <- data.frame(Year = rownames(ftab), ftab, check.names = FALSE)
write.taf(ftab,"F_SAM.csv", dir="report/assessment")

# N at age
ntab <- ntable(fit)
ntab <-round(ntab)
ntab <- data.frame(Year = rownames(ntab), ntab, check.names = FALSE)
write.taf(ntab,"N_SAM.csv", dir="report/assessment")

# Summary Table
tsb    <- round(tsbtable(fit))

tabsummary<- data.frame(summary(fit), tsb)
tsb<-data.frame(tsb)

colnames(tabsummary)<-c("R", "R_Low","R_High","SSB","SSB_Low","SSB_High","Fbar","Fbar_Low","Fbar_High","TSB","TSB_Low","TSB_High")
write.taf(data.frame(Year = rownames(tabsummary), tabsummary),
          "Summary_SAM.csv", dir="report/assessment")

# summary catch
catab  <- catchtable(fit)
colnames(catab) <- c("Catch","Catch_Low", "Catch_High")
catab<-round(catab)
write.taf(data.frame(Year = rownames(catab), catab),
          "Summary_SAM_catch.csv", dir="report/assessment")

ptab <- partable(fit)
ptab<-xtab2taf(ptab)
# signif, not round: some parameters, such as catchabilities, are near zero
ptab[,-1]<-signif(ptab[,-1],3)
colnames(ptab)[1]<-"Parameter"
write.taf(ptab,"SAM_model_parameters.csv",dir="report/assessment")

###########################################################################################################
# Additional plots and tables, as made in the SAM assessments in ices-taf

age_colours <- function(n) rep_len(colours, n)

plot_at_age <- function(name, x, ylab) {
  n <- ncol(x) - 1
  taf.png(file.path("report/assessment", name))
  par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
  matplot(x$Year, x[-1], type = "l", lty = "solid", lwd = 2,
          col = age_colours(n), xlab = "Year", bty = "l", ylab = ylab)
  legend("topleft", legend = names(x)[-1], lwd = 2, lty = 1, bty = "n",
         ncol = 3, col = age_colours(n))
  dev.off()
}

# Catch numbers at age, bubble size proportional to catch
catage <- read.taf("data/catage.csv")
cn <- as.matrix(catage[-1])
taf.png("report/assessment/input_catage")
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
symbols(rep(catage$Year, ncol(cn)), rep(as.numeric(colnames(cn)), each = nrow(cn)),
        circles = sqrt(as.vector(cn) / max(cn, na.rm = TRUE)), inches = 0.12,
        bg = adjustcolor(colours[7], 0.5), fg = colours[7],
        xlab = "Year", ylab = "Age", bty = "l")
dev.off()

# Catch, landing and discard weights at age
plot_at_age("input_catchweights", read.taf("data/wcatch.csv"), "Catch weights (kg)")
plot_at_age("input_landingweights", read.taf("data/wlandings.csv"), "Landing weights (kg)")
plot_at_age("input_discardweights", read.taf("data/wdiscards.csv"), "Discard weights (kg)")

# Proportion discarded at age, by year and as a distribution by age
landfrac <- read.taf("data/landfrac.csv")
propdis <- landfrac
propdis[-1] <- ifelse(cn > 0, 1 - as.matrix(landfrac[-1]), NA)
plot_at_age("input_propdiscarded", propdis, "Proportion discarded")
taf.png("report/assessment/input_propdiscarded_age")
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
boxplot(propdis[-1], xlab = "Age", ylab = "Proportion discarded", bty = "l",
        col = adjustcolor(colours[3], 0.5))
dev.off()

# Survey indices by year, by cohort, and internal consistency between ages
for (file in dir("data", pattern = "^survey_.*\\.csv$")) {
  survey <- read.taf(file.path("data", file))
  name <- sub("\\.csv$", "", file)
  idx <- as.matrix(survey[-1])
  idx[idx <= 0] <- NA
  ages <- as.numeric(colnames(idx))

  plot_at_age(paste0(name, "_year"), cbind(survey[1], log(idx)), "log(index)")

  taf.png(file.path("report/assessment", paste0(name, "_cohort")))
  par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
  cohort <- outer(survey$Year, ages, "-")
  plot(range(cohort), range(log(idx), na.rm = TRUE), type = "n",
       xlab = "Cohort", ylab = "log(index)", bty = "l")
  for (a in seq_along(ages)) {
    lines(cohort[, a], log(idx[, a]), lwd = 2, col = age_colours(length(ages))[a])
  }
  legend("topleft", legend = ages, lwd = 2, lty = 1, bty = "n", ncol = 3,
         col = age_colours(length(ages)))
  dev.off()

  if (length(ages) > 1) {
    by_cohort <- tapply(log(as.vector(idx)),
                        list(as.vector(cohort), rep(ages, each = nrow(idx))),
                        mean)
    taf.png(file.path("report/assessment", paste0(name, "_consistency")))
    pairs(by_cohort, labels = paste("Age", ages), gap = 0.3,
          panel = function(x, y) {
            points(x, y, pch = 16, col = adjustcolor(colours[6], 0.6))
            ok <- is.finite(x) & is.finite(y)
            if (sum(ok) > 2) abline(lm(y[ok] ~ x[ok]), col = colours[7], lwd = 2)
          })
    dev.off()
  }
}

# Data availability
taf.png("report/assessment/dataplot")
dataplot(fit)
dev.off()

# Observation correlation
taf.png("report/assessment/corplot")
corplot(fit)
dev.off()

# Survey catchability at age
taf.png("report/assessment/catchability")
qtableplot(qtable(fit))
dev.off()

# Stock numbers at age
taf.png("report/assessment/natage", width = 2200, height = 1500)
par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
barplot(t(ntable(fit)), xlab = "Year", ylab = "Stock numbers at age", space = 0,
        legend = TRUE, col = age_colours(ncol(ntable(fit))),
        args.legend = list(x = "topleft", bty = "n", ncol = 3))
dev.off()

# Jitter, when run in model_assessment_SAM.R
if (file.exists("model/assessment/jitter.rds")) {
  taf.png("report/assessment/jitter")
  plot(readRDS("model/assessment/jitter.rds"))
  dev.off()
}

# Model table and catchability
write.taf(read.taf("output/assessment/modtab.csv"), "SAM_modeltable.csv",
          dir = "report/assessment")
qtab <- read.taf("output/assessment/qtab.csv")
qtab[-1] <- signif(qtab[-1], 3)
write.taf(qtab, "SAM_catchability.csv", dir = "report/assessment")

# Summary as in the advice sheets: Fbar rounded with icesRound
smry <- tabsummary
smry_adv <- data.frame(Year = rownames(smry),
                       Recruitment = round(smry$R),
                       SSB = round(smry$SSB),
                       Fbar = icesAdvice::icesRound(smry$Fbar))
smry_adv <- merge(smry_adv,
                  data.frame(Year = rownames(catab), Catch = catab[, "Catch"]),
                  all.x = TRUE)
write.taf(smry_adv, "Summary_SAM_advice.csv", dir = "report/assessment")
