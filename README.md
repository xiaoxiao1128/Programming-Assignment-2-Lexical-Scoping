# Programming-Assignment-2-Lexical-Scoping
best <- function (state, outcome){
  data <- read.csv("outcome-of-care-measures.csv")
  A <- data$State == state
  ## Test if the variable: state is in the list of data$State. If not, the sum of A will be 0.
  if (!sum(A)) {
    stop ("invalid state")
  }
  
  disease_list <- c("heart attack", "heart failure", "pneumonia")
  ## Test if the variable: outcome is in the list of disease
  if (!outcome %in% disease_list){
    stop ("invalid outcome")
  }
  ## Create the sub-data.frame for the specific state.
  StateData <- subset(data, State == state)
  ## Extract the hospital and rate colume from the data.
  if (outcome == "heart attack") { 
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  }
  else if (outcome == "heart failure"){
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  }
  else {
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  }
  
  ## Assign a common colume name for the StateData whatever the disease is.
  colnames(StateData) <- c("Hospital.Name", "Disease.Rate")
  
  ## Transform the disease.rate colume from Factor to numeric for the purpose of ordering. 
  StateData[, "Disease.Rate"] <- as.numeric(as.character(StateData[, "Disease.Rate"]))
  StateData[, "Hospital.Name"] <- as.character(StateData[, "Hospital.Name"])
 
  ## Order the data.frame by disease rate and hospital names.
  StateData <- StateData[order(StateData$Disease.Rate, StateData$Hospital.Name), ]
  best <- StateData[1, "Hospital.Name"]
  best 
}


rankhospital <- function (state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv")
  A <- data$State == state
  if (!sum(A)) {
    stop ("invalid state")
  }
  
  disease_list <- c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% disease_list){
    stop ("invalid outcome")
  }
  
  StateData <- subset(data, State == state)
  
  if (outcome == "heart attack") { 
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  }
  else if (outcome == "heart failure"){
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  }
  else {
    StateData <- StateData[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  }
  colnames(StateData) <- c("Hospital.Name", "Disease.Rate")

  StateData[, "Disease.Rate"] <- as.numeric(as.character(StateData[, "Disease.Rate"]))
  StateData[, "Hospital.Name"] <- as.character(StateData[, "Hospital.Name"])
  
  StateData <- StateData[order(StateData$Disease.Rate, StateData$Hospital.Name), ] 

  ## Specify the exact value for num input.  
  N <- sum(!is.na(StateData$Disease.Rate))
  if (num == "best"){
    num <- 1
  }
  else if (num == "worst"){
    num <- N
  }
  else{}
  
  Hospital <- StateData[num, "Hospital.Name"]
  Hospital
}


rankall <- function (outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv")

  disease_list <- c("heart attack", "heart failure", "pneumonia")
  
  if (!outcome %in% disease_list){
    stop ("invalid outcome")
  }

  ## Extract the hospital and rate colume from the data.
  if (outcome == "heart attack") { 
    data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
  }
  else if (outcome == "heart failure"){
    data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
  }
  else {
    data <- data[c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  }
  ## Assign a common colume name for the StateData whatever the disease is.
  colnames(data)[3] <- "Disease.Rate"
  
  ## Transform the disease.rate colume from Factor to numeric for the purpose of ordering.So does the hospital colume.  
  data[, "Disease.Rate"] <- as.numeric(as.character(data[, "Disease.Rate"]))
  data[, "Hospital.Name"] <- as.character(data[, "Hospital.Name"])
  
  ## Create a list to store all of the state names in US, and order it alphabetically. 
  Statelist <- as.character(unique(data$State))
  Statelist <- Statelist[order(Statelist)]
  
  Final <- data.frame()
  
  for (i in seq_len(length(Statelist))){
    ## Create the sub-data.frame for the specific state.
    StateData <- subset(data, State == Statelist[i])
    
    ## Order the data.frame by disease rate and hospital names.
    StateData <- StateData[order(StateData$Disease.Rate, StateData$Hospital.Name), ]
    
    ## Specify the exact value for num input. 
    N <- sum(!is.na(StateData$Disease.Rate))
    if (num == "best"){
      num <- 1
    }
    else if (num == "worst"){
      num <- N
    }
    else{}
    
    Hospital <- StateData[num, "Hospital.Name"]
    tmp <- data.frame(Hospital, Statelist[i])  # Create each row for the final data.frame. 
    colnames(tmp) <- c("hospital", "state") 
    Final <- rbind(Final, tmp)
  }
  
  Final
}
