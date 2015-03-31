rankall <- function(outcome, num = "best") {
      outcome_care<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
      outcome_care[,11]<-as.numeric(outcome_care[,11])
      outcome_care[,17]<-as.numeric(outcome_care[,17])
      outcome_care[,23]<-as.numeric(outcome_care[,23])
      outcome_state<-split(outcome_care,outcome_care$State)
      if (is.na(match(outcome,c("heart attack","heart failure","pneumonia")))){
            stop("invalid outcome")
      }
      if (num=="best") num<-1
      if(outcome=="heart attack") out<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
      if(outcome=="heart failure") out<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
      if(outcome=="pneumonia") out<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      counter=1
      storenum<-num
      ergebnis=NULL
      for (state in names(outcome_state)){
            num<-storenum
            firstorder<-order(outcome_state[[state]][[out]],na.last = NA)
            rankedlist<-data.frame(hospital=outcome_state[[state]]$Hospital.Name[firstorder], outcomenumber=outcome_state[[state]][[out]][firstorder])
            reordered<-order(rankedlist$outcomenumber,rankedlist$hospital)
            rerankedlist<-data.frame(hospital=rankedlist$hospital[reordered],outcomenumber=rankedlist$outcomenumber[reordered])
            if (num=="worst") num<-length(firstorder)
            if (num <= length(firstorder) & num > 0) {
                  ergebnis<-c(ergebnis,as.character(rerankedlist$hospital[num]))
            } 
            else {
                  ergebnis<-c(ergebnis,NA)     
            }
            counter<-counter+1
      }
      resultat<-data.frame(hospital=ergebnis,state=names(outcome_state),row.names = names(outcome_state))
}
#rankall("heart failure")
rankall("heart attack", 20)
#rankall("pneumonia", "worst")