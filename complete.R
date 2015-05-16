complete <- function(directory, id=1:332){
    curdir <- getwd()
    completeList <- c() # a list for collecting the number of complete measure
    for(n in id){
		#create filename
		n_char <- as.character(n)
		frontZero <- paste(rep("0", 3-nchar(n_char)), collapse="")
		fileName<-paste(c(frontZero,n_char,".csv"), collapse="")
		fullFilePath = paste(c(curdir,"/",directory,"/",fileName),collapse="")
		
		#read data from the csv file
		pollutionData <- read.csv(fullFilePath)
		
		#extract data of different pollution read
		sulfateData <- pollutionData[["sulfate"]]
		nitrateData <- pollutionData[["nitrate"]]    
		completeList <- c(completeList, sum(!is.na(sulfateData) & !is.na(nitrateData)))	
    }	
    data.frame(ID=id, nobs = completeList)
}