"resampling.model" <-
function(k,data,model) {
modelo<-model
parte<-strsplit(model,"~")[[1]]
name.y<-strsplit(parte[1]," ")[[1]]
nm<-names(data)
nc<-length(nm)
for(i in 1:nc){
if (name.y==nm[i]) pos.dep=i
}
model<-as.formula(model)
ecuacion<-lm(model,data=data)
xx<-data.frame(anova(ecuacion),NA)
fc<-xx[,4]
names(xx)<-c("Df","Sum Sq","Mean Sq","F value","Pr(>F)","Resampling")
m<-nrow(data)
gk<-nrow(xx)-1
f<-rep(0,gk)
cuenta <- rep(0,gk)
# Start Resampling
for(i in 1:k){
muestra<-sample(data[,pos.dep],m)
data[,pos.dep]<-muestra
resample<-lm(model,data=data)
for (j in 1:gk){
f[j]<-anova(resample)[j,4]
if(f[j] >= fc[j])cuenta[j]<-cuenta[j]+1
}
}
# finish resampling

for( j in 1:gk){
xx[j,6]<-cuenta[j]/k
}
cat("\nResampling of the experiments\n")
cat(rep("-",14),"\n")
cat("Proposed model:",modelo,"\n")
cat("---\n")
cat("Resampling of the analysis of variancia for the proposed model\n")
cat("Determination of the P-Value by Resampling\n")
cat("Samples:",k,"\n\n")
xx<-as.matrix(xx)
print(xx,na.print="")
cat("---\n\n")
return(list(solution=xx,acum=cuenta,samples=k))
}

