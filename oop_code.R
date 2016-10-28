if(!require(dplyr)) {
    stop("the 'dplyr' package needs to be installed first")
}

if(!require(purrr)) {
    stop("the 'purrr' package needs to be installed first")
}



#######################   Classes definition

Subject <- function(x, id, sum = FALSE){
    structure(list(data = x, ID = id, SUM = sum), class = "Subject")
}

Visit <- function(x, id, v, sum = FALSE){
    structure(list(data = x, ID = id, V = v, SUM = sum), class = "Visit")
}

Room <- function(x, id, v, r, sum = FALSE){
    structure(list(data = x, ID = id, V = v, R = r, SUM = sum), class = "Room")
}

LongitudinalData <- function(x){
    structure(list(data = x), class = "LongitudinalData")
}



#######################   Print functions

# ID: 44 
# Visit: 0 
# Room: bedroom
print.Room <- function(x) {
    if(class(x) != "Room") stop();
    
    if(x$SUM == FALSE) {
        cat(paste("ID:", x$ID, sep=' '), "\n") 
        cat(paste("Visit:", x$V, sep=' '), "\n")
        cat(paste("Room:", x$R, sep=' '), "\n")    
    } else {
        cat(paste("ID:", x$ID, sep=' '), "\n")
        
        ret <- data %>% 
            filter( (id == x$ID) & (visit == x$V) & (room == x$R) )
     
        print(summary(ret$value))
    }
}

# ID: 44 
# Visit: 0 
print.Visit <- function(x) {
    if(class(x) != "Visit") stop();
    
    if(x$SUM == FALSE){
        cat(paste("ID:", x$ID, sep=' '), "\n") 
        cat(paste("Visit:", x$V, sep=' '), "\n")    
    } else {
        cat(paste("ID:", x$ID, sep=' '), "\n")
        cat(paste("Visit:", x$V, sep=' '), "\n")    
        
        ret <- data %>% 
            filter(id == x$ID) %>% 
            select(visit, room, value) %>% 
            group_by(visit, room) %>% 
            summarise(sum = mean(value)) %>% 
            spread(room, sum) %>% 
            filter(visit == x$V) %>%
            ungroup() %>%
            select(-visit) 
        
        print(as.data.frame(ret))
    }
}

# Subject ID: 14
print.Subject <- function(x) {
    if(class(x) != "Subject") stop();
    
    if(x$SUM == FALSE) {
        cat(paste("Subject ID:", x$ID, sep=' '), "\n")    
    } else {
        cat(paste("ID:", x$ID, sep=' '), "\n")    
        
        ret <- data %>% 
            filter(id == x$ID) %>% 
            select(visit, room, value) %>% 
            group_by(visit, room) %>% 
            summarise(sum = mean(value)) %>% 
            spread(room, sum)
        
        print(as.data.frame(ret))
    }
}

# Longitudinal dataset with 10 subjects
print.LongitudinalData <- function(x) {
    if(class(x) != "LongitudinalData") stop();
    cat(paste("Longitudinal dataset with", length(unique(x$data$id)), "subjects", sep=' '), "\n")
}



#######################   Generic functions

subject <- function(x, i) UseMethod("subject")

subject.LongitudinalData <- function(x, i){
    
    if( !(i %in% unique(x$data$id)) ) {
        return(NULL)
    }
    
    Subject(x$data, i)
}



visit <- function(x, v) UseMethod("visit")

visit.Subject <- function(x, v){
    
    if( !(v %in% unique(x$data$visit)) ) {
        return(NULL)
    }
    
    Visit(x$data, x$ID, v)
}



room <- function(x, r) UseMethod("room")

room.Visit <- function(x, r){
    
    if( !(r %in% unique(x$data$room)) ) {
        return(NULL)
    }
    
    Room(x$data, x$ID, x$V, r)
}



summary.Subject <- function(x){
    Subject(x$data, x$ID, sum = TRUE)
}

summary.Visit <- function(x){
    Visit(x$data, x$ID, x$V, sum = TRUE)
}

summary.Room <- function(x){
    Room(x$data, x$ID, x$V, x$R, sum = TRUE)
}




make_LD <- function(x) {
    LongitudinalData(x)
}




















