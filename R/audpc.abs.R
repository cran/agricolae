"audpc.abs" <-
function(evaluation, dates) {
n<-length(dates)
k<-ncol(evaluation)
if (n!=k) {
cat("Error:\nThe number of dates of evaluation \nmust agree with the number of evaluations\n")
return()
}
audpc<-0
for (i in 1:(n-1)) {
audpc<- audpc + (evaluation[,i]+evaluation[,i+1])*(dates[i+1]-dates[i])/2
}
return(audpc)
}

