corr <- function(directory, threshold = 0){
    curdir <- getwd()
    corrList <- c() # a list for collecting the number correlation calculated
    for(n in 1:332){
    	#create filename
		n_char <- as.character(n)
		frontZero <- paste(rep("0", 3-nchar(n_char)), collapse="")
		fileName<-paste(c(frontZero,n_char,".csv"), collapse="")
		fullFilePath = paste(c(curdir,"/",directory,"/",fileName),collapse="")
		
		#read data from the csv file
		pollutionData <- read.csv(fullFilePath)
		
		#print(pollutionData)
		#print(pollutionData)
	    #extract data of different pollution read
		sulfateData <- pollutionData[["sulfate"]]
		nitrateData <- pollutionData[["nitrate"]] 
		completeEntry <- !is.na(sulfateData) & !is.na(nitrateData)
		if(sum(completeEntry)>threshold){
			corVal <- cor(sulfateData[completeEntry], nitrateData[completeEntry])
			corrList <- c(corrList, corVal)
		}
    }
    print(corrList)
}