# Finding the best hospital in a state
best <- function(state, outcome){
	
	selectedStateFullData <- getStateData(state)
	
    nameRate <- searchOutcome(selectedStateFullData, outcome)
	

	
	#find best hospitals with minimal mortality rate
	minMortality <- min(nameRate$mortalityRate)
	bestHospitals <- nameRate$hospitalName[nameRate$mortalityRate == minMortality]
	
	#sort and print the first Hospital name
	sortedHospitals <- sort(bestHospitals)
	bestHospital <- sortedHospitals[1]
	
	bestHospital
	
}

# Ranking a hospital by outcome in a state
rankhospital <- function(state, outcome, num = "best"){
	
	selectedStateFullData <- getStateData(state) # get state data
	
	#get hospital name and mortality rate; eliminate NA entries
	nameRate <- searchOutcome(selectedStateFullData, outcome) 
	
	#sort hospital by mortality rate and then name
	sortMortalityRate <- nameRate$mortalityRate[order(nameRate$mortalityRate, nameRate$hospitalName)]
	sortHospitalName <- nameRate$hospitalName[order(nameRate$mortalityRate, nameRate$hospitalName)]
	
	hospitalNum <- length(sortHospitalName) # get total number of hospital
		
	if(identical(num, "best")){
		num <- 1
	}
	if(identical(num, "worst")){
		num <- hospitalNum
	}
	
	if(num>hospitalNum){
		return(NA) 
	}
	
	sortHospitalName[num]	
}


rankall <- function(outcome, num="best"){
	## organise data by state
	fullData <- read.csv("hospitalData/outcome-of-care-measures.csv", colClasses="character")
	stateFullData<-split(fullData, fullData$State)
	
	selectedHospital <- c()
	for (state in names(stateFullData)){
		eachStateData <- stateFullData[[state]]
		nameRate <- searchOutcome(eachStateData, outcome)
		
		#sort hospital by mortality rate and then name
	    sortMortalityRate <- nameRate$mortalityRate[order(nameRate$mortalityRate, nameRate$hospitalName)]
	    sortHospitalName <- nameRate$hospitalName[order(nameRate$mortalityRate, nameRate$hospitalName)]
	    
	    hospitalNum <- length(sortHospitalName) # get total number of hospital in the state
		
		
    	if(identical(num, "best")){num <- 1}
	    if(identical(num, "worst")){num <- hospitalNum}
	    if(num>hospitalNum){sortHospitalName <- rep(NA, num) } # handle the case when there is not enough hospital
	
	    selectedHospital <- append(selectedHospital, sortHospitalName[num]	)
	}
	
	stateHospital <- list(stateName = names(stateFullData), hospitalName = selectedHospital)
	

}


getStateHospitalRank <- function(){
	
}

getStateData <- function(state){	
	## get data from a chosen state
	fullData <- read.csv("hospitalData/outcome-of-care-measures.csv", colClasses="character")
	stateFullData<-split(fullData, fullData$State)
	selectedStateFullData <- stateFullData[[state]]
	
	
	#Here check validity of state name 
	if (length(selectedStateFullData) < 1){
		#exit and report error
		stop("invalid state name")
	}
	selectedStateFullData
}

searchOutcome <- function(allData, outcome){
	## Make a search string for mortality rate of interest 
	prefixString <- "^Hospital.*Death.*Mortality.*"
	searchFor <- paste(prefixString, outcome,sep="")
	
	#extract Hospital name and mortality rate column 
	hospitalName <- allData[grepl("Hospital.Name", names(allData))] 
	mortalityRate <- allData[grepl(searchFor, names(allData))]
	
	#Here check validity of disease name 
	if (length(mortalityRate) < 1){
		#exit and report error
		stop("invalid outcome")
	}
	
    #eliminate entries in which data in unavailable; convert mortality to number
	hospitalName <- hospitalName[!(mortalityRate == "Not Available")]
	mortalityRate <- as.numeric(mortalityRate[!(mortalityRate == "Not Available")])
	
	nameRate <- list(hospitalName = hospitalName, mortalityRate = mortalityRate)
}