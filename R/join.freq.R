"join.freq" <-
function(breaks,join){
join<-join[-1]
breaks<-breaks[-join]
return(breaks)
}

