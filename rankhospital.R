rankhospital <- function(state, outcome,num = "best") {
      outcome_care<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if (is.na(match(state,outcome_care$State))){
            stop("invalid state")
      }
      if (is.na(match(outcome,c("heart attack","heart failure","pneumonia")))){
            stop("invalid outcome")
      }
      if (num=="best") num<-1
      rankedlist=data.frame()
      outcome_care[,11]<-as.numeric(outcome_care[,11])
      outcome_care[,17]<-as.numeric(outcome_care[,17])
      outcome_care[,23]<-as.numeric(outcome_care[,23])
      disease<-split(outcome_care,outcome_care$State)
      if(outcome=="heart attack"){
            diseaseorder<-order(disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.last = NA)
            rankedlist=data.frame(Hospital.Name=disease[[state]]$Hospital.Name[diseaseorder], Mortality.Heart.Attack=disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[diseaseorder], Rank=1:length(diseaseorder))
            reordered<-order(rankedlist$Mortality.Heart.Attack,rankedlist$Hospital.Name)
            rerankedlist=data.frame(Hospital.Name=rankedlist$Hospital.Name[reordered], Mortality.Heart.Attack=rankedlist$Mortality.Heart.Attack[reordered], Rank=1:length(reordered))
            if (num=="worst") num<-length(diseaseorder)
      }
      if(outcome=="heart failure"){
            diseaseorder<-order(disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.last = NA)
            rankedlist=data.frame(Hospital.Name=disease[[state]]$Hospital.Name[diseaseorder], Mortality.Heart.Failure=disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[diseaseorder], Rank=1:length(diseaseorder))
            reordered<-order(rankedlist$Mortality.Heart.Failure,rankedlist$Hospital.Name)
            rerankedlist=data.frame(Hospital.Name=rankedlist$Hospital.Name[reordered], Mortality.Heart.Failure=rankedlist$Mortality.Heart.Failure[reordered], Rank=1:length(reordered))
            if (num=="worst") num<-length(diseaseorder)
      }
      if(outcome=="pneumonia"){
            diseaseorder<-order(disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.last = NA)
            rankedlist=data.frame(Hospital.Name=disease[[state]]$Hospital.Name[diseaseorder], Mortality.Pneumonia=disease[[state]]$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[diseaseorder], Rank=1:length(diseaseorder))
            reordered<-order(rankedlist$Mortality.Pneumonia,rankedlist$Hospital.Name)
            rerankedlist=data.frame(Hospital.Name=rankedlist$Hospital.Name[reordered], Mortality.Pneumonia=rankedlist$Mortality.Pneumonia[reordered], Rank=1:length(reordered))
            if (num=="worst") num<-length(diseaseorder)
      }
      
      return(as.character(rerankedlist$Hospital.Name[num]))
}