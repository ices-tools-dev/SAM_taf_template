## Extract results of interest, report tables

## Before: sam fit object in model/assessment
## After: csv tables of assessment output in report/assessment

rm(list=ls())

# Libraries
library(TAF)
library(stockassessment)

# Directories
mkdir("report/assessment")

# load fit
load("model/assessment/fit.rds", verbose = TRUE)

# load retro
load("model/assessment/retro_fit.rds", verbose = TRUE)

# leave one out runs
load("model/assessment/leaveoneout.RData")

# load residuals
load("model/assessment/residuals.RData")

#load data
load("model/assessment/data.RData")

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
rec<-ntable(fit)[,"0"]
taf.png("report/assessment/Rec_per_SSB.png")
plot(fit$data$years,log(rec/ssb),type='b',xlab="Year",ylab="ln(Recruits/SSB) ",cex.lab=1.5)
dev.off()


# Selectivity of the Fishery
taf.png("Fig_Fishselectivity")
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


taf.png("Fig_partial_F")
F<-faytable(fit)
matplot(rownames(F),F,lty=1:ncol(F),col=colours[1:ncol(F)], type='l', xlab='Year', lwd=3)
legend('topright', col=colours[1:ncol(F)], lty=1:ncol(F), legend=colnames(F), bty='n', lwd=3)

dev.off()


###########################################################################################################
#Tables

# F at age
faytab <- faytable(fit)
ftab<-round(faytab,3)
write.taf(ftab,"F_SAM.csv", dir="report/assessment", row.names=TRUE)

# N at age
ntab <- ntable(fit)
ntab <-round(ntab)
write.taf(ntab,"N_SAM.csv", dir="report/assessment", row.names=TRUE)

# Summary Table
tsb    <- round(tsbtable(fit))

tabsummary<- data.frame(summary(fit), tsb)
tsb<-data.frame(tsb)

colnames(tabsummary)<-c("R", "R_Low","R_High","SSB","SSB_Low","SSB_High","Fbar","Fbar_Low","Fbar_High","TSB","TSB_Low","TSB_High")
write.taf(tabsummary,"Summary_SAM.csv", dir="report/assessment", row.names=TRUE)

# summary catch
catab  <- catchtable(fit)
colnames(catab) <- c("Catch","Catch_Low", "Catch_High")
catab<-round(catab)
write.taf(catab,"Summary_SAM_catch.csv", dir="report/assessment", row.names=TRUE)

ptab <- partable(fit)
ptab<-xtab2taf(ptab)
ptab[,-1]<-round(ptab[,-1],3)
colnames(ptab)[1]<-" "
write.taf(ptab,"SAM_model_parameters.csv",dir="report/assessment")


