pollutantmean <- function(directory, pollutant, id=1:332){
    curdir <- getwd()
    meanReadWt <- c() # a list to keep weighted mean pollution read from each station
    numRead <- c() # a list to keep number of pollution data point read from each station
	for(n in id){
		#create filename
		n_char <- as.character(n)
		frontZero <- paste(rep("0", 3-nchar(n_char)), collapse="")
		fileName<-paste(c(frontZero,n_char,".csv"), collapse="")
		fullFilePath = paste(c(curdir,"/",directory,"/",fileName),collapse="")
		
		#read data from the csv file
		pollutionData <- read.csv(fullFilePath)
		
		#extract particular pollution measure of interest
		selectedPollutant <- pollutionData[[pollutant]]
	    measuredPollutant <- selectedPollutant[!is.na(selectedPollutant)]
	    
	    numReadStation <- length(measuredPollutant)
	    if (numReadStation>0){
	 	    meanReadWt <- c(meanReadWt, mean(measuredPollutant)*numReadStation)
     	    numRead <- c(numRead, numReadStation)   	
	    }

		
	}
	print(sum(meanReadWt)/sum(numRead))
}