best<- function(state,outcome){
  my_data <- read.csv("outcome-of-care-measures.csv",header = T)
  
  my_state_data <- my_data[,c(2,7,11,17,23)]
  col_names<- c("hospital_name","State","heart attack","heart failure","pneumonia")
  
  colnames(my_state_data)<- col_names
  
  if(!(state %in% my_state_data$State))
  {
    stop("invalid state")
  }
  if(!(outcome %in% c("heart attack","heart failure", "pneumonia")))
  {
    stop("invalid outcome")
  }
  
  my_state_data<-my_state_data[is.element(my_state_data$State,state),]
  my_state_data$`heart attack` <- suppressWarnings(as.numeric(as.character(my_state_data$`heart attack`)))
  my_state_data$`heart failure` <- suppressWarnings(as.numeric(as.character(my_state_data$`heart failure`)))
  my_state_data$pneumonia <- suppressWarnings(as.numeric(as.character(my_state_data$pneumonia)))
  my_state_data <- lapply(my_state_data,function(x) if(is.factor(x)) factor(x) else x)
  my_state_data <- data.frame(my_state_data)
  my_state_data$dat_rank<-NA
  if(outcome == "heart attack")
  {
    my_state_data <- my_state_data[,c(1,2,3)]
    my_state_data <- my_state_data[complete.cases(my_state_data),]
    my_order_data <- order(my_state_data[,"heart.attack"], my_state_data[,"hospital_name"],decreasing = FALSE, na.last = TRUE)
  }
  else if (outcome == "heart failure")
  {
    my_state_data <- my_state_data[,c(1,2,4)]
    my_state_data <- my_state_data[complete.cases(my_state_data),]
    my_order_data <- order(my_state_data[,"heart.failure"], my_state_data[,"hospital_name"],decreasing = FALSE, na.last = TRUE)
  }
  else {
    my_state_data <- my_state_data[,c(1,2,5)]
    my_state_data <- my_state_data[complete.cases(my_state_data),]
    my_order_data <- order(my_state_data[,"pneumonia"], my_state_data[,"hospital_name"],decreasing = FALSE, na.last = TRUE)
  }
  
  my_state_data$dat_rank[my_order_data]<-1:nrow(my_state_data)
  suppressWarnings(as.character(unlist(my_state_data[my_state_data$dat_rank ==1,"hospital_name"])))
}