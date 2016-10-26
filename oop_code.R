if(!require(dplyr)) {
    stop("the 'dplyr' package needs to be installed first")
}

# design classes to represent the concept of a “subject”, a “visit”, and a “room”

Subject <- function(id){
    structure(list(ID = id), class = "Subject")
}

LongitudinalData <- function(x){
    
    su <- vector(length = max(x$id))
    
    su[1] <- Subject(1)
    
    # TODO
    
    structure(list(subjects = su, len = length(unique(x$id))), class = "LongitudinalData")
}

print.LongitudinalData <- function(x) {
    if(class(x) != "LongitudinalData") stop();
    cat(paste("Longitudinal dataset with", x$len, "subjects", sep=' '))
}





make_LD <- function(x) {
 
    ret <- LongitudinalData(x)
    
       
    return(ret)
}


