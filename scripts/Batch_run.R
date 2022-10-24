library(ctmm)


setwd("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals")
source("HR_Function.R")


FILES <- list.files("~/Dropbox (Smithsonian)/MultiSpecies_Data/Mammals",
                    pattern = ".Rda",
                    full.names = TRUE)



#For testing purposes, use a gazelle that runs through quickly
#i <- 51
#j <- 1



#Then walk through each of them sequentially
for(i in 61:length(FILES)){
  
  #Load in the tracking data
  DATA <- get(load(FILES[i]))
  
  #Store the species binomial
  
  #BINOMIAL <- gsub("/Users/noonanm/Dropbox (Smithsonian)/MultiSpecies_Data/Birds/", "", gsub(".Rda", "", FILES[i]), fixed = TRUE)
  #BINOMIAL <- gsub("/Users/michaelnoonan/Dropbox (Smithsonian)/MultiSpecies_Data/Mammals/", "", gsub(".Rda", "", FILES[i]), fixed = TRUE)
  BINOMIAL <- gsub("/home/mike/Dropbox (Smithsonian)/MultiSpecies_Data/Mammals/", "", gsub(".Rda", "", FILES[i]), fixed = TRUE)
  
  
  message("Working on the tracking data on ", BINOMIAL)
  
  
  
  if(BINOMIAL == "Brachylagus_idahoensis"){
    
    DATA$gps.vdop <- 1/(DATA$gps.satellite.count-2)
    
    DATA$height.above.msl <- NULL
    
    #Convert to a telemetry object
    DATA <- as.telemetry(DATA)
    
    UERE <- uere.fit(DATA[1:12])
    
    DATA <- DATA[13:29]
    
    uere(DATA) <- UERE
    
  } else {
  
    
    #Convert to a telemetry object
    
    if(i == 3 || i == 26 || i == 36){
      DATA <- as.telemetry(DATA[,c(3:5, 10)])
    } else {
      
      #Convert to a telemetry object
      DATA <- as.telemetry(DATA)
    }
  }
  

  #Create the paths to the file directory
  dir.create(paste("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/", BINOMIAL, sep = ""))
  dir.create(paste("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/", BINOMIAL, "/Fits", sep = ""))
  Model_path = paste("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/", BINOMIAL, "/Fits", sep = "")
  
  dir.create(paste("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/", BINOMIAL, "/UDs", sep = ""))
  UD_path = paste("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/", BINOMIAL, "/UDs", sep = "")
  
  dir.create(paste("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/", BINOMIAL, "/Figs", sep = ""))
  Fig_Path = paste("~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/", BINOMIAL, "/Figs", sep = "")
  
  
  #Then walk through each individual sequentially
  for(j in 1:length(DATA)){
    
    cat("Working on individual ", j, " of ", length(DATA), "\n")
    
    #Extract the current individual
    
    if(class(DATA) == "list") {cilla <- DATA[[j]]} else {
      
      cilla <- DATA
    }

    
          
    RESULTS <- tryCatch(
        
        {
          
          RESULTS <- AKDE_HR(cilla,
                             Model_path = Model_path,
                             UD_path = UD_path,
                             Fig_Path = Fig_Path)
        
        
        }, error=function(err) {
          message(cat("Home range estimation failed, returning NAs \n"))
          
          RESULTS <- as.data.frame(t(unlist(c(BINOMIAL,
                                              cilla@info[1],
                                             rep(NA, 32)))))
          
          return(RESULTS)
        }
      )
      
      

      
      
    if(i ==1 && j == 1){
      
      write.table(RESULTS,
                  file = "~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/HR_Results_NEW_DATA.csv",
                  row.names=FALSE,
                  col.names=TRUE,
                  sep=",",
                  append=TRUE)
      
    } else {
      
      write.table(RESULTS,
                  file = "~/Dropbox (Smithsonian)/Allometric_Relationships/Scaling_of_Bias/Mammals/Bias_Scaling_Mammals/Results/HR_Results_NEW_DATA.csv",
                  row.names=FALSE,
                  col.names=FALSE,
                  sep=",",
                  append=TRUE)
      
    }
    
    
    
    
  }#Closes the loop that runs over the as.telemetry object
  
}#Closes the loop that runs over the individual datasets



