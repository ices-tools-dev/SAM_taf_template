# function to build change in advice tables -------------------
get_advice_change_tables_SAM <- function(WG="WGNSSK 2026",fit=fit,fc=fc,
                                     data.yr=2025,assess.yr=2026,advice.yr = 2027,
                                     Ry=NULL,By=NULL){
  
  # This function will build results tables for making the change in advice plots
  # Inputs:
  #       WG - character string. A label, usually the WG with year (e.g. "WGNSSK 2026"), to denote where the results come from
  #       fit - sam.fit object. The stock assessment fit from SAM
  #       fc - samforecast object. A single forecast object, for ACOM change in advice you want the scenario that relates to headline advice.
  #       data.yr - numeric. The last year of data
  #       assess.yr - numeric. The year in which the assessment was conducted, e.g. the intermediate year 
  #       advice.yr - numeric. The year for which advice is being given
  #       Ry - numeric. The years for which the sam forecast is resampling Recruitment values from (e.g. 2000:2025)
  #       By - numeric. The years used to get average values for biological parameters (e.g. 2023:2025)
  
  # Outputs:
  #         res - a list containing tables of the forecast results: summary values (SSB, Rec, Fbar, catch), 
  #         mean weights in the stock, selectivity, numbers-at-age and biomass-at-age.
  
  
  # Set up---------#
  ac <- as.character 
  # results list
  res <- vector(mode="list",length=5)
  names(res) <- c("summary","mean weights","selectivity","natage","batage")
  
  
  # get assessment fit from forecast
  fit_fc <- attr(fc, "fit")
  
  # Assessment and forecast settings needed
  ages <- fit$conf$minAge:fit$conf$maxAge
  if(!is.null(By)) By <- data.yr 
  ave.years <- By
  
  # Recruitment in forecast years reported as geometric mean value
  if(!is.null(Ry)) Ry <- data.yr
  R <- rectable(fit)[,1]
  R_geoMean <- exp(mean(log(R[ac(Ry)]))) # better summary stat for tables and plots when length(Ry) is even
  
  
  # helper functions

  doAve <- function(x){
    if(length(dim(x))==2){
      ret <- colMeans(x[rownames(x)%in%ave.years,,drop=FALSE], na.rm=TRUE)
    }
    if(length(dim(x))==3){
      ret <- apply(x[rownames(x)%in%ave.years,,,drop=FALSE],c(2,3),mean, na.rm=TRUE)
    }
    ret
  }
  
  getThisOrAve <- function(x,y, ave){
    if(y %in% rownames(x)){
      if(length(dim(x))==2){  
        ret <- x[which(rownames(x)==y),]
      }else{
        ret <- x[which(rownames(x)==y),,]
      }
      ret[is.na(ret)]<-ave[is.na(ret)]
    }else{
      ret <- ave
    }
    ret
  }
  
  # Summary table of forecast assumptions/results -----------------------#
  # make table
  tab <- data.frame(WG=WG,Variable = sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                    Year = rep(c(data.yr,assess.yr,advice.yr),4), Type=NA, Value= NA, Source = "Forecast")
  
  # Set data/int/advice year
  tab$Type[tab$Year %in% data.yr] <- "Data"
  tab$Type[tab$Year %in% assess.yr] <- "Intermediate year"
  tab$Type[tab$Year %in% advice.yr] <- "Advice year"
  
  # Overwrite source as assessment for data year
  tab$Source[tab$Year %in% data.yr] <- "Assessment"
  
  # Get assessment summary
  astab <- as.data.frame(summary(fit))
  colnames(astab)[1] <- "R" # standardise column names
  colnames(astab)[7] <- "Fbar" # standardise column names
  astab$Year <- rownames(astab)
  cat_tab <- catchtable(fit)
  
  # Get forecast summary 
  fctab <- attr(fc,"tab")
  
  # data year values
  idx <- which(astab$Year %in% data.yr)
  tab$Value[tab$Year %in% data.yr & tab$Variable %in% "Recruitment"] <- astab$R[idx] 
  tab$Value[tab$Year %in% data.yr & tab$Variable %in% "SSB"] <- astab$SSB[idx] 
  tab$Value[tab$Year %in% data.yr & tab$Variable %in% "Fbar"] <- astab$Fbar[idx] 
  tab$Value[tab$Year %in% data.yr & tab$Variable %in% "Total catch"] <- cat_tab[ac(data.yr),"Estimate"] 
  
  # assessment year values
  tab$Value[tab$Year %in% assess.yr & tab$Variable %in% "SSB"] <- fctab[ac(assess.yr),"ssb:median"]
  tab$Value[tab$Year %in% assess.yr & tab$Variable %in% "Fbar"] <- fctab[ac(assess.yr),"fbar:median"]
  tab$Value[tab$Year %in% assess.yr & tab$Variable %in% "Total catch"] <- fctab[ac(assess.yr),"catch:median"]
  
  # advice year values
  tab$Value[tab$Year %in% advice.yr & tab$Variable %in% "SSB"] <- fctab[ac(advice.yr),"ssb:median"]
  tab$Value[tab$Year %in% advice.yr & tab$Variable %in% "Fbar"] <- fctab[ac(advice.yr),"fbar:median"]
  tab$Value[tab$Year %in% advice.yr & tab$Variable %in% "Total catch"] <- fctab[ac(advice.yr),"catch:median"]
  
  # Use geomean for Rec
  tab$Value[tab$Variable %in% "Recruitment" & tab$Source %in% "Forecast"] <- R_geoMean
  
  # Set factors for plotting order
  tab$Type <- factor(tab$Type,levels=c("Data","Intermediate year","Advice year"))
  tab$Variable <- factor(tab$Variable,levels=c("SSB","Fbar","Total catch","Recruitment"))
  
  res[[1]] <- tab
  
  # Forecast stock wts ---------------------------------------------------#
  
  # Use average from ave.years unless values are provided externally 
  ave.sw <- doAve(fit_fc$data$stockMeanWeight)
  sw <- as.data.frame(rbind(rbind(ave.sw,ave.sw),ave.sw))
  row.names(sw) <- c(assess.yr,advice.yr,advice.yr+1)
  
  for(y in c(assess.yr,advice.yr,advice.yr+1)){
    sw[ac(y),]<-getThisOrAve(fit_fc$data$stockMeanWeight, y, ave.sw)
  }
  
  # Make table
  sw$Year <- row.names(sw)
  dat <- reshape2::melt(sw,id.vars=c("Year"))
  names(dat) <- c("Year","Age","stock.wt")
  
  # age 0 - check on no observations - set to NA
  dat[dat$Age==0 & dat$stock.wt==0,"stock.wt"]<-NA
  
  # Add source, WG and type
  dat$Source <- "Forecast"
  dat$WG <- WG
  dat$Type <- NA
 # dat$Type[dat$Year %in% data.yr] <- "Data"
  dat$Type[dat$Year %in% assess.yr] <- "Intermediate year"
  dat$Type[dat$Year %in% advice.yr] <- "Advice year"
  dat$Type[dat$Year %in% (advice.yr+1)] <- "SSB year"
  

  res[[2]] <- dat
  
  
  # Forecast selectivity --------------------------------------------------#
  
  # data year selectivity - get from assessment fit
  fay <- faytable(fit)[ac(data.yr),]
  sel <- fay/max(fay)
  
  # Make table
  tab <- data.frame(WG=WG,Year=data.yr,Type="Data",Source="Assessment",
                    Age=ages,sel=sel)
  
  # forecast - get from forecast sim. 
  fIdx <- (1:length(ages))+length(ages)
  yrs <- lapply(fc,function(x){return(x$year)})
  
  # assessment year
  yIdx <- which(yrs %in% assess.yr)
  fy <- exp(apply(fc[[yIdx]]$sim[,fIdx],2,quantile,0.5))
  sel <- fy/max(fy)
  
  tab <- rbind(tab,data.frame(WG=WG,Year=assess.yr,Type="Intermediate year",Source="Forecast",
                              Age=ages,sel=sel))
  
  # advice year
  yIdx <- which(yrs %in% advice.yr)
  fy <- exp(apply(fc[[yIdx]]$sim[,fIdx],2,quantile,0.5))
  sel <- fy/max(fy)
  
  tab <- rbind(tab,data.frame(WG=WG,Year=advice.yr,Type="Advice year",Source="Forecast",
                              Age=ages,sel=sel))
  
  # Set factors for plotting order
  tab$Type <- factor(tab$Type,levels=c("Data","Intermediate year","Advice year"))
  
  res[[3]] <- tab
  
  # N at age from forecast ------------------------------------------
  
  # stock numbers in data year - get from fit object
  N <- ntable(fit)
  tab <- cbind(data.frame(WG=WG,Year=row.names(N),Type="Data",Source="Assessment"),N)
  if(max(tab$Year)==assess.yr){
    tab <- tab[tab$Year<assess.yr,] # remove assessment year if included
  }
  
  # Get forecast numbers at age
  Nfc <- do.call(rbind, lapply(fc, function(x)exp(colMeans(x$sim[,1:ncol(N)]))))
  rownames(Nfc) <- lapply(fc, function(x)x$year)
  colnames(Nfc) <- colnames(N)
  
  natage_fc <- as.data.frame(Nfc)
  natage_fc$Year <- unlist(lapply(fc,function(x){return(x$year)}))
  colnames(natage_fc) <- c(ages,"Year")
  natage_fc <- natage_fc[natage_fc$Year >=assess.yr,] # restrict to forecast years
  
  # Add WG, Source and Type
  natage_fc$WG <- WG
  natage_fc$Source <- "Forecast"
  natage_fc$Type <- NA
  natage_fc$Type[natage_fc$Year %in% assess.yr] <- "Intermediate year"
  natage_fc$Type[natage_fc$Year %in% advice.yr] <- "Advice year"
  
  # Add to table
  tab <- rbind(tab,natage_fc)
  
  n_tab <- reshape2::melt(tab,id.vars=c("WG","Year","Type","Source"))
  colnames(n_tab) <- c("WG","Year","Type","Source","Age","N")
  
  # replace R with R geomean
  n_tab$N[n_tab$Age == min(ages) & n_tab$Source %in% "Forecast"] <- R_geoMean
  
  # Remove SSB year
  n_tab <- n_tab[n_tab$Year < (advice.yr+1),]
  
  # Set factors for plotting order
  n_tab$Type <- factor(n_tab$Type,levels=c("Data","Intermediate year","Advice year"))
  
  res[[4]] <- n_tab
  
  # biomass at age ------------------------------#
  # get forecast n at age tables
  # Get stock weights from data - sam fit object
  wt <- as.data.frame(fit$data$stockMeanWeight)
  if(max(row.names(wt))>data.yr){
    idx <- which(row.names(wt)>data.yr) # restrict to data years
    wt <- wt[-idx,]
  }
  # combine wts in datas to forecast weights (sw)
  wt <- rbind(wt,sw[,-which(colnames(sw) %in% "Year")]) 
  
  # Add WG, Source, Type to table
  wt$Year <- row.names(wt)
  wt$WG <- WG
  wt$Source <- "Assessment"
  wt$Source[wt$Year > data.yr] <- "Forecast"
  wt$Type <- "Data"
  wt$Type[wt$Year %in% assess.yr] <- "Intermediate year"
  wt$Type[wt$Year %in% advice.yr] <- "Advice year"
  
  wt_tab <- reshape2::melt(wt,id.vars=c("WG","Year","Type","Source"))
  colnames(wt_tab) <- c("WG","Year","Type","Source","Age","stock.wt")
  
  # Remove SSB year
  wt_tab <- wt_tab[wt_tab$Year < (advice.yr+1),]
  
  # Merge with n-at-age table and calculate biomass
  b_tab <- merge(n_tab,wt_tab)
  b_tab$biomass <- b_tab$N*b_tab$stock.wt
  
  # Set factors for plotting order
  b_tab$Type <- factor(b_tab$Type,levels=c("Data","Intermediate year","Advice year","SSB year"))
  
  res[[5]] <- b_tab
  
  # return results ---------------#
  return(res)
  
}
