audps <-
function(evaluation, dates, type = "absolute") {
if(!(is.matrix(evaluation) | is.data.frame(evaluation))){
evaluation<-rbind(evaluation)
}
n<-length(dates)
k<-ncol(evaluation)
if (n!=k) {
cat("Error:\nThe number of dates of evaluation \nmust agree with the number of evaluations\n")
return()
}
d<-(dates[n]-dates[1])
area<-audpc(evaluation, dates)
audps<-area+(evaluation[1]+evaluation[n])/2*d/(n-1)
if (type =="relative" ) audps <-audps/(d*100)
if (type =="absolute" | type =="relative" ) {
return(audps)
}
else cat("Error: type is 'absolute' or 'relative'\n\n")
}
