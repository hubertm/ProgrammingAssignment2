best <- function(state, outcome) {
      outcome_care<-read.csv("outcome-of-care-measures.csv")
      if (is.na(match(state,outcome_care$State))){
            stop("invalid state")
      }
      hospitallist=NULL
      if(outcome=="heart attack"){
            disease<-split(outcome_care,outcome_care$State)
            result<-as.numeric(as.vector(disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
            hospitallist<-as.character(disease[[state]]$Hospital.Name[which(result==min(result,na.rm=TRUE))])
      }
      if(outcome=="heart failure"){
            disease<-split(outcome_care,outcome_care$State)
            result<-as.numeric(as.vector(disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
            hospitallist<-as.character(disease[[state]]$Hospital.Name[which(result==min(result,na.rm=TRUE))])
      }
      if(outcome=="pneumonia"){
            disease<-split(outcome_care,outcome_care$State)
            result<-as.numeric(as.vector(disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
            hospitallist<-as.character(disease[[state]]$Hospital.Name[which(result==min(result,na.rm=TRUE))])
      }
      if (is.na(match(outcome,c("heart attack","heart failure","pneumonia")))){
            stop("invalid outcome")
      }

      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      output<-sort(hospitallist)
      return(output[1])

}