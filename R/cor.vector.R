"cor.vector" <-
function(x,y,method = c("pearson","kendall", "spearman"),alternative = "two.sided") 
{
method <- match.arg(method)
name.xy<-paste(deparse(substitute(x)), "and", deparse(substitute(y)))
xx<-cbind(x,y)
yy<-na.omit(xx)
nn<-length(yy[,1])
x<-yy[,1]
y<-yy[,2]
corr<-correl(x,y,method=method,alternative=alternative)
stat<-corr$stat
coef<-corr$rho
pvalue<-corr$pvalue
if(method=="pearson") {
gl<-nn-2
cat("\nPearson's product-moment correlation\n\n")
cat("data:",name.xy,"\n")
cat("t =",stat,", df =",gl,", p-value =",pvalue,"\n")
cat("alternative hypothesis: true rho is ")
if(alternative == "two.sided" ) cat("not equal to 0") 
if(alternative == "less" ) cat("less than 0") 
if(alternative == "greater") cat("greater than 0")
cat("sample estimates:\ncor\n",coef,"\n")
#list(t=t,df=gl,p.value=pvalue,rho=coef)
}
if(method=="spearman"){
cat("\nSpearman's rank correlation rho\n\n")
cat("data:",name.xy,"\n")
cat("p-value =",pvalue,"\n")
cat("alternative hypothesis: true rho is ")
if(alternative == "two.sided" ) cat("not equal to 0") 
if(alternative == "less" ) cat("less than 0") 
if(alternative == "greater") cat("greater than 0")
cat("\nsample estimates:\nrho\n",coef,"\n")
#list(S=t,p.value=pvalue,rho=coef)
}
if(method=="kendall"){
cat("\nKendall's rank correlation tau\n\n")
cat("data:",name.xy,"\n")
cat("z-norm = ",stat,"p-value =",pvalue,"\n")
cat("alternative hypothesis: true rho is ")
if(alternative == "two.sided" ) cat("not equal to 0") 
if(alternative == "less" ) cat("less than 0") 
if(alternative == "greater") cat("greater than 0")
cat("\nsample estimates:\ntau\n",coef,"\n")
#list(z.norm=stat,p.value=pvalue,tau=coef)
}
}

