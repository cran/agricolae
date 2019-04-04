### R code from vignette source 'tutorial.Rnw'

###################################################
### code chunk number 1: agricolae
###################################################
options(keep.source = TRUE, width = 60)
qp <- packageDescription("agricolae")


###################################################
### code chunk number 2: tutorial.Rnw:29-30
###################################################
rm(list=ls())


###################################################
### code chunk number 3: tutorial.Rnw:51-52
###################################################
library(agricolae)


###################################################
### code chunk number 4: tutorial.Rnw:58-60 (eval = FALSE)
###################################################
## help(package="agricolae")
## help(waller.test)


###################################################
### code chunk number 5: tutorial.Rnw:77-81
###################################################
detach(package:agricolae) # detach package agricole
library(agricolae) # Load the package to the memory
designs<-apropos("design")
print(designs[substr(designs,1,6)=="design"], row.names=FALSE)


###################################################
### code chunk number 6: tutorial.Rnw:90-91
###################################################
library(agricolae) # Load the package to the memory: 


###################################################
### code chunk number 7: tutorial.Rnw:105-108
###################################################
A<-as.data.frame(data(package="agricolae")$results[,3:4])
A[,2]<-paste(substr(A[,2],1,35),"..",sep=".")
head(A)


###################################################
### code chunk number 8: tutorial.Rnw:119-122
###################################################
weight<-c( 68, 53, 69.5, 55, 71, 63, 76.5, 65.5, 69, 75, 76, 57, 70.5, 71.5, 56, 81.5,
           69, 59, 67.5, 61, 68, 59.5, 56.5, 73, 61, 72.5, 71.5, 59.5, 74.5, 63)
print(summary(weight)) 


###################################################
### code chunk number 9: f1
###################################################
par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.6)
h1<- graph.freq(weight,col=colors()[84],frequency=1,las=2,density=20,ylim=c(0,12),ylab="Frequency") 
x<-h1$breaks
h2<- plot(h1, frequency =2, axes= FALSE,ylim=c(0,0.4),xlab="weight",ylab="Relative (%)")
polygon.freq(h2, col=colors()[84], lwd=2, frequency =2)
axis(1,x,cex=0.6,las=2)
y<-seq(0,0.4,0.1)
axis(2, y,y*100,cex=0.6,las=1) 


###################################################
### code chunk number 10: tutorial.Rnw:144-145
###################################################
stat.freq(h1)


###################################################
### code chunk number 11: tutorial.Rnw:164-165
###################################################
print(summary(h1),row.names=FALSE)


###################################################
### code chunk number 12: tutorial.Rnw:170-174
###################################################
sturges.freq(weight)
intervals.freq(h1)
join.freq(h1,1:3) -> h3
print(summary(h3))


###################################################
### code chunk number 13: f2
###################################################
par(mfrow=c(1,2),mar=c(4,4,0,1),cex=0.8)
plot(h3, frequency=2,col=colors()[84],ylim=c(0,0.6),axes=FALSE,xlab="weight",ylab="%",border=0)
y<-seq(0,0.6,0.2)
axis(2,y,y*100,las=2)
axis(1,h3$breaks)
normal.freq(h3,frequency=2,col=colors()[90])
ogive.freq(h3,col=colors()[84],xlab="weight")


###################################################
### code chunk number 14: f3
###################################################
par(mfrow=c(1,2),mar=c(4,3,2,1),cex=0.6)
h4<-hist(weight,xlab="Classes (h4)")
table.freq(h4)
# this is possible
# hh<-graph.freq(h4,plot=FALSE)
# summary(hh)
# new class
classes <- c(0, 10, 20, 30, 40, 50)
freq <- c(3, 8, 15, 18, 6)
h5 <- graph.freq(classes,counts=freq, xlab="Classes (h5)",main="Histogram grouped data")


###################################################
### code chunk number 15: tutorial.Rnw:220-221
###################################################
print(summary(h5),row.names=FALSE)


###################################################
### code chunk number 16: tutorial.Rnw:265-271
###################################################
str(design.crd)
trt <- c("A", "B", "C")
repeticion <- c(4, 3, 4)
outdesign <- design.crd(trt,r=repeticion,seed=777,serie=0)
book1 <- outdesign$book
head(book1)


###################################################
### code chunk number 17: tutorial.Rnw:280-288
###################################################
str(design.rcbd)
trt <- c("A", "B", "C","D","E")
repeticion <- 4
outdesign <- design.rcbd(trt,r=repeticion, seed=-513, serie=2)
# book2 <- outdesign$book
book2<- zigzag(outdesign) # zigzag numeration
print(outdesign$sketch)
print(matrix(book2[,1],byrow = TRUE, ncol = 5))


###################################################
### code chunk number 18: tutorial.Rnw:294-298
###################################################
str(design.lsd)
trt <- c("A", "B", "C", "D")
outdesign <- design.lsd(trt, seed=543, serie=2)
print(outdesign$sketch)


###################################################
### code chunk number 19: tutorial.Rnw:302-304
###################################################
book <- zigzag(outdesign)
print(matrix(book[,1],byrow = TRUE, ncol = 4))


###################################################
### code chunk number 20: tutorial.Rnw:312-317
###################################################
str(design.graeco)
trt1 <- c("A", "B", "C", "D")
trt2 <- 1:4
outdesign <- design.graeco(trt1,trt2, seed=543, serie=2)
print(outdesign$sketch)


###################################################
### code chunk number 21: tutorial.Rnw:322-324
###################################################
book <- zigzag(outdesign)
print(matrix(book[,1],byrow = TRUE, ncol = 4))


###################################################
### code chunk number 22: tutorial.Rnw:331-339
###################################################
str(design.youden)
varieties<-c("perricholi","yungay","maria bonita","tomasa")
r<-3
outdesign <-design.youden(varieties,r,serie=2,seed=23)
print(outdesign$sketch)
book <- outdesign$book
print(book) # field book.
print(matrix(as.numeric(book[,1]),byrow = TRUE, ncol = r))


###################################################
### code chunk number 23: tutorial.Rnw:344-346
###################################################
book <- zigzag(outdesign)
print(matrix(as.numeric(book[,1]),byrow = TRUE, ncol = r))


###################################################
### code chunk number 24: tutorial.Rnw:354-361
###################################################
str(design.bib)
trt <- c("A", "B", "C", "D", "E" )
k <- 4
outdesign <- design.bib(trt,k, seed=543, serie=2)
book5 <- outdesign$book
outdesign$statistics
outdesign$parameters


###################################################
### code chunk number 25: tutorial.Rnw:366-367
###################################################
outdesign$sketch


###################################################
### code chunk number 26: tutorial.Rnw:373-375
###################################################
book <- zigzag(outdesign)
matrix(book[,1],byrow = TRUE, ncol = 4)


###################################################
### code chunk number 27: tutorial.Rnw:382-388
###################################################
str(design.cyclic)
trt <- c("A", "B", "C", "D", "E", "F" )
outdesign <- design.cyclic(trt,k=3, r=6, seed=543, serie=2)
book6 <- outdesign$book
outdesign$sketch[[1]]
outdesign$sketch[[2]]


###################################################
### code chunk number 28: tutorial.Rnw:394-398
###################################################
book <- zigzag(outdesign)
array(book$plots,c(3,6,2))->X
t(X[,,1])
t(X[,,2])


###################################################
### code chunk number 29: tutorial.Rnw:404-405
###################################################
str(design.lattice)


###################################################
### code chunk number 30: tutorial.Rnw:411-418
###################################################
trt<-letters[1:9]
outdesign <-design.lattice(trt, r = 3, serie = 2, seed = 33, 
    kinds =  "Super-Duper")
book7 <- outdesign$book
outdesign$parameters
outdesign$sketch
head(book7)


###################################################
### code chunk number 31: tutorial.Rnw:423-428
###################################################
book <- zigzag(outdesign)
array(book$plots,c(3,3,3)) -> X
t(X[,,1])
t(X[,,2])
t(X[,,3])


###################################################
### code chunk number 32: tutorial.Rnw:434-444
###################################################
str(design.alpha)
trt <- letters[1:15]
outdesign <- design.alpha(trt,k=3,r=2,seed=543)
book8 <- outdesign$book
outdesign$statistics
outdesign$sketch
# codification of the plots
A<-array(book8[,1], c(3,5,2))
t(A[,,1])
t(A[,,2])


###################################################
### code chunk number 33: tutorial.Rnw:449-453
###################################################
book <- zigzag(outdesign)
A<-array(book[,1], c(3,5,2))
t(A[,,1])
t(A[,,2])


###################################################
### code chunk number 34: tutorial.Rnw:459-466
###################################################
str(design.dau)
rm(list=ls())
trt1 <- c("A", "B", "C", "D")
trt2 <- c("t","u","v","w","x","y","z")
outdesign <- design.dau(trt1, trt2, r=5, seed=543, serie=2)
book9 <- outdesign$book
with(book9,by(trt, block,as.character))


###################################################
### code chunk number 35: tutorial.Rnw:471-474
###################################################
book <- zigzag(outdesign)
with(book,by(plots, block, as.character))
head(book)


###################################################
### code chunk number 36: tutorial.Rnw:482-483
###################################################
str(design.split)


###################################################
### code chunk number 37: tutorial.Rnw:488-497
###################################################
trt1<-c("A","B","C","D")
trt2<-c("a","b","c")
outdesign <-design.split(trt1,trt2,r=3,serie=2,seed=543)
book10 <- outdesign$book
head(book10)
p<-book10$trt1[seq(1,36,3)]
q<-NULL
for(i in 1:12) 
q <- c(q,paste(book10$trt2[3*(i-1)+1],book10$trt2[3*(i-1)+2], book10$trt2[3*(i-1)+3]))


###################################################
### code chunk number 38: tutorial.Rnw:502-503
###################################################
print(t(matrix(p,c(4,3))))


###################################################
### code chunk number 39: tutorial.Rnw:508-509
###################################################
print(t(matrix(q,c(4,3))))


###################################################
### code chunk number 40: tutorial.Rnw:514-516
###################################################
book <- zigzag(outdesign)
head(book,5)


###################################################
### code chunk number 41: tutorial.Rnw:522-523
###################################################
str(design.strip)


###################################################
### code chunk number 42: tutorial.Rnw:528-540
###################################################
trt1<-c("A","B","C","D")
trt2<-c("a","b","c")
outdesign <-design.strip(trt1,trt2,r=3,serie=2,seed=543)
book11 <- outdesign$book
head(book11)
t3<-paste(book11$trt1, book11$trt2)
B1<-t(matrix(t3[1:12],c(4,3)))
B2<-t(matrix(t3[13:24],c(3,4)))
B3<-t(matrix(t3[25:36],c(3,4)))
print(B1)
print(B2)
print(B3)


###################################################
### code chunk number 43: tutorial.Rnw:545-551
###################################################
book <- zigzag(outdesign)
head(book)
array(book$plots,c(3,4,3))->X
t(X[,,1])
t(X[,,2])
t(X[,,3])


###################################################
### code chunk number 44: tutorial.Rnw:557-558
###################################################
str(design.ab)


###################################################
### code chunk number 45: tutorial.Rnw:561-562
###################################################
trt <- c (4,2,3) # three factors with  4,2 and 3 levels.


###################################################
### code chunk number 46: tutorial.Rnw:566-570
###################################################
trt<-c(3,2) # factorial 3x2
outdesign <-design.ab(trt, r=3, serie=2)
book12 <- outdesign$book
head(book12) # print of the field book


###################################################
### code chunk number 47: tutorial.Rnw:575-577
###################################################
book <- zigzag(outdesign)
head(book)


###################################################
### code chunk number 48: tutorial.Rnw:582-587
###################################################
trt<-c(2,2,2)
crd<-design.ab(trt, r=5, serie=2,design="crd")
names(crd)
crd$parameters
head(crd$book)


###################################################
### code chunk number 49: tutorial.Rnw:598-602
###################################################
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
cv.model(model)
with(sweetpotato,mean(yield))


###################################################
### code chunk number 50: tutorial.Rnw:607-609
###################################################
df<-df.residual(model)
MSerror<-deviance(model)/df


###################################################
### code chunk number 51: tutorial.Rnw:616-618
###################################################
# comparison <- LSD.test(yield,virus,df,MSerror)
LSD.test(model, "virus",console=TRUE)


###################################################
### code chunk number 52: tutorial.Rnw:623-625
###################################################
# comparison <- LSD.test(yield, virus,df, MSerror, group=FALSE)
outLSD <-LSD.test(model, "virus", group=FALSE,console=TRUE)


###################################################
### code chunk number 53: tutorial.Rnw:632-634
###################################################
options(digits=2)
print(outLSD)


###################################################
### code chunk number 54: tutorial.Rnw:640-645
###################################################
LSD.test(model, "virus", group=FALSE, p.adj= "bon",console=TRUE)
out<-LSD.test(model, "virus", group=TRUE, p.adj= "holm")
print(out$group)
out<-LSD.test(model, "virus", group=FALSE, p.adj= "holm")
print(out$comparison)


###################################################
### code chunk number 55: tutorial.Rnw:656-657
###################################################
duncan.test(model, "virus",console=TRUE)


###################################################
### code chunk number 56: tutorial.Rnw:664-666
###################################################
# SNK.test(model, "virus", alpha=0.05,console=TRUE)
SNK.test(model, "virus", group=FALSE,console=TRUE)


###################################################
### code chunk number 57: tutorial.Rnw:673-675
###################################################
# REGW.test(model, "virus", alpha=0.05,console=TRUE)
REGW.test(model, "virus", group=FALSE,console=TRUE)


###################################################
### code chunk number 58: tutorial.Rnw:682-684
###################################################
outHSD<- HSD.test(model, "virus",console=TRUE)
outHSD


###################################################
### code chunk number 59: tutorial.Rnw:691-697
###################################################
A<-sweetpotato[-c(4,5,7),]
modelUnbalanced <- aov(yield ~ virus, data=A)
outUn <-HSD.test(modelUnbalanced, "virus",group=FALSE, unbalanced = TRUE)
print(outUn$comparison)
outUn <-HSD.test(modelUnbalanced, "virus",group=TRUE, unbalanced = TRUE)
print(outUn$groups)


###################################################
### code chunk number 60: tutorial.Rnw:704-708
###################################################
outUn <-HSD.test(modelUnbalanced, "virus",group=FALSE)
print(outUn$comparison[,1:2])
outUn <-HSD.test(modelUnbalanced, "virus",group=TRUE)
print(outUn$groups)


###################################################
### code chunk number 61: tutorial.Rnw:715-718
###################################################
# variance analysis:
anova(model)
with(sweetpotato,waller.test(yield,virus,df,MSerror,Fc= 17.345, group=FALSE,console=TRUE))


###################################################
### code chunk number 62: tutorial.Rnw:722-723
###################################################
outWaller <- waller.test(model, "virus", group=FALSE,console=FALSE)


###################################################
### code chunk number 63: tutorial.Rnw:728-730
###################################################
names(outWaller)
print(outWaller$comparison)


###################################################
### code chunk number 64: tutorial.Rnw:735-736
###################################################
outWaller$statistics


###################################################
### code chunk number 65: tutorial.Rnw:743-746
###################################################
# analysis of variance: 
scheffe.test(model,"virus", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")


###################################################
### code chunk number 66: tutorial.Rnw:752-753
###################################################
outScheffe <- scheffe.test(model,"virus", group=FALSE, console=TRUE)


###################################################
### code chunk number 67: tutorial.Rnw:760-762
###################################################
# modelABC <-aov (y ~ A * B * C, data)
# compare <-LSD.test (modelABC, c ("A", "B", "C"),console=TRUE)


###################################################
### code chunk number 68: tutorial.Rnw:769-781
###################################################
yield <-scan (text =
 "6 7 9 13 16 20 8 8 9
  7 8 8 12 17 18 10 9 12
  9 9 9 14 18 21 11 12 11
  8 10 10 15 16 22 9 9 9 "
 )
block <-gl (4, 9)
clone <-rep (gl (3, 3, labels = c ("c1", "c2", "c3")), 4)
nitrogen <-rep (gl (3, 1, labels = c ("n1", "n2", "n3")), 12)
A <-data.frame (block, clone, nitrogen, yield)
head (A)
outAOV <-aov (yield ~ block + clone * nitrogen, data = A)


###################################################
### code chunk number 69: tutorial.Rnw:784-787
###################################################
anova (outAOV)
outFactorial <-LSD.test (outAOV, c("clone", "nitrogen"), 
main = "Yield ~ block + nitrogen + clone + clone:nitrogen",console=TRUE)


###################################################
### code chunk number 70: tutorial.Rnw:790-795 (eval = FALSE)
###################################################
## par(mar=c(3,3,2,0))
## pic1<-bar.err(outFactorial$means,variation="range",ylim=c(5,25), bar=FALSE,col=0,las=1)
## points(pic1$index,pic1$means,pch=18,cex=1.5,col="blue")
## axis(1,pic1$index,labels=FALSE)
## title(main="average and range\nclon:nitrogen")


###################################################
### code chunk number 71: tutorial.Rnw:802-811
###################################################
# Example linear estimation and design of experiments. (Joshi)
# Institute of Social Sciences Agra, India
# 6 varieties of wheat in 10 blocks of 3 plots each.
block<-gl(10,3)
variety<-c(1,2,3,1,2,4,1,3,5,1,4,6,1,5,6,2,3,6,2,4,5,2,5,6,3,4,5,3, 4,6)
Y<-c(69,54,50,77,65,38,72,45,54,63,60,39,70,65,54,65,68,67,57,60,62, 
59,65,63,75,62,61,59,55,56)
head(cbind(block,variety,Y))
BIB.test(block, variety, Y,console=TRUE)


###################################################
### code chunk number 72: tutorial.Rnw:816-819
###################################################
out <-BIB.test(block, trt=variety, Y, test="tukey", group=FALSE, console=TRUE)
names(out)
rm(block,variety)


###################################################
### code chunk number 73: tutorial.Rnw:832-837
###################################################
# alpha design 
Genotype<-paste("geno",1:30,sep="")
r<-2
k<-3
plan<-design.alpha(Genotype,k,r,seed=5)


###################################################
### code chunk number 74: tutorial.Rnw:843-845
###################################################
yield <-c(5,2,7,6,4,9,7,6,7,9,6,2,1,1,3,2,4,6,7,9,8,7,6,4,3,2,2,1,1, 
          2,1,1,2,4,5,6,7,8,6,5,4,3,1,1,2,5,4,2,7,6,6,5,6,4,5,7,6,5,5,4)


###################################################
### code chunk number 75: tutorial.Rnw:849-852
###################################################
data<-data.frame(plan$book,yield)
# The analysis: 
modelPBIB <- with(data,PBIB.test(block, Genotype, replication, yield, k=3, group=TRUE, console=TRUE))


###################################################
### code chunk number 76: tutorial.Rnw:887-893
###################################################
trt<-c(1,8,5,5,2,9,2,7,3,6,4,9,4,6,9,8,7,6,1,5,8,3,2,7,3,7,2,1,3,4,6,4,9,5,8,1)
yield<-c(48.76,10.83,12.54,11.07,22,47.43,27.67,30,13.78,37,42.37,39,14.46,30.69,42.01,
22,42.8,28.28,50,24,24,15.42,30,23.8,19.68,31,23,41,12.9,49.95,25,45.57,30,20,18,43.81)
sqr<-rep(gl(4,3),3)
block<-rep(1:12,3)
modelLattice<-PBIB.test(block,trt,sqr,yield,k=3,console=TRUE, method="VC")


###################################################
### code chunk number 77: tutorial.Rnw:908-913
###################################################
block<-c(rep("I",7),rep("II",6),rep("III",7))
trt<-c("A","B","C","D","g","k","l","A","B","C","D","e","i","A","B", "C",
"D","f","h","j")
yield<-c(83,77,78,78,70,75,74,79,81,81,91,79,78,92,79,87,81,89,96, 82)
head(data.frame(block, trt, yield))


###################################################
### code chunk number 78: tutorial.Rnw:918-919
###################################################
by(trt,block,as.character)


###################################################
### code chunk number 79: tutorial.Rnw:924-925
###################################################
by(yield,block,as.character)


###################################################
### code chunk number 80: tutorial.Rnw:930-933
###################################################
modelDAU<- DAU.test(block,trt,yield,method="lsd",console=TRUE)
options(digits = 2)
modelDAU$means


###################################################
### code chunk number 81: tutorial.Rnw:936-938
###################################################
modelDAU<- DAU.test(block,trt,yield,method="lsd",group=FALSE,console=FALSE)
head(modelDAU$comparison,8)


###################################################
### code chunk number 82: tutorial.Rnw:960-962
###################################################
data(corn)
str(corn)


###################################################
### code chunk number 83: tutorial.Rnw:969-970
###################################################
str(kruskal)


###################################################
### code chunk number 84: tutorial.Rnw:975-976
###################################################
outKruskal<-with(corn,kruskal(observation,method,group=TRUE, main="corn", console=TRUE))


###################################################
### code chunk number 85: tutorial.Rnw:985-989
###################################################
out<-with(corn,kruskal(observation,method,group=TRUE, main="corn", p.adj="holm"))
print(out$group)
out<-with(corn,kruskal(observation,method,group=FALSE, main="corn", p.adj="holm"))
print(out$comparison)


###################################################
### code chunk number 86: tutorial.Rnw:995-996
###################################################
str(friedman)


###################################################
### code chunk number 87: tutorial.Rnw:1001-1004
###################################################
data(grass)
out<-with(grass,friedman(judge,trt, evaluation,alpha=0.05, group=FALSE,
main="Data of the book of Conover",console=TRUE))


###################################################
### code chunk number 88: tutorial.Rnw:1010-1011
###################################################
str(waerden.test)


###################################################
### code chunk number 89: tutorial.Rnw:1016-1018
###################################################
data(sweetpotato)
outWaerden<-with(sweetpotato,waerden.test(yield,virus,alpha=0.01,group=TRUE,console=TRUE))


###################################################
### code chunk number 90: tutorial.Rnw:1022-1025
###################################################

names(outWaerden)



###################################################
### code chunk number 91: tutorial.Rnw:1028-1029
###################################################
out<-with(sweetpotato,waerden.test(yield,virus,group=FALSE,console=TRUE))


###################################################
### code chunk number 92: tutorial.Rnw:1037-1038
###################################################
str(Median.test)


###################################################
### code chunk number 93: tutorial.Rnw:1041-1042
###################################################
str(Median.test)


###################################################
### code chunk number 94: tutorial.Rnw:1047-1052
###################################################
data(sweetpotato)
outMedian<-with(sweetpotato,Median.test(yield,virus,console=TRUE))
names(outMedian)
outMedian$statistics
outMedian$medians


###################################################
### code chunk number 95: f14
###################################################
par(mfrow=c(2,2),mar=c(3,3,1,1),cex=0.8)
# Graphics
bar.group(outMedian$groups,ylim=c(0,50))
bar.group(outMedian$groups,xlim=c(0,50),horiz = TRUE)
plot(outMedian)
plot(outMedian,variation="IQR",horiz = TRUE)


###################################################
### code chunk number 96: tutorial.Rnw:1075-1076
###################################################
str(durbin.test)


###################################################
### code chunk number 97: tutorial.Rnw:1081-1091
###################################################
days <-gl(7,3)
chemical<-c("A","B","D","A","C","E","C","D","G","A","F","G", "B","C","F", 
"B","E","G","D","E","F")
toxic<-c(0.465,0.343,0.396,0.602,0.873,0.634,0.875,0.325,0.330, 0.423,0.987,0.426,
0.652,1.142,0.989,0.536,0.409,0.309, 0.609,0.417,0.931)
head(data.frame(days,chemical,toxic))
out<-durbin.test(days,chemical,toxic,group=FALSE,console=TRUE,
main="Logarithm of the toxic dose")
names(out)
out$statistics


###################################################
### code chunk number 98: tutorial.Rnw:1104-1108
###################################################
# model <-aov (yield ~ fertilizer, data = field) 
# out <-LSD.test (model, "fertilizer", group = TRUE) 
# bar.group (out $ group)
str(bar.group)


###################################################
### code chunk number 99: tutorial.Rnw:1116-1120
###################################################
# model <-aov (yield ~ fertilizer, data = field) 
# out <-LSD.test (model, "fertilizer", group = TRUE) 
# bar.err(out$means)
str(bar.err)


###################################################
### code chunk number 100: f4
###################################################
par(mfrow=c(2,2),mar=c(3,3,2,1),cex=0.7)
c1<-colors()[480]; c2=colors()[65] 
bar.err(outHSD$means, variation="range",ylim=c(0,50),col=c1,las=1)
bar.err(outHSD$means, variation="IQR",horiz=TRUE, xlim=c(0,50),col=c2,las=1)
plot(outHSD, variation="range",las=1)
plot(outHSD, horiz=TRUE, variation="SD",las=1)


###################################################
### code chunk number 101: tutorial.Rnw:1147-1160 (eval = FALSE)
###################################################
## par(mfrow=c(2,2),cex=0.7,mar=c(3.5,1.5,3,1))
## C1<-bar.err(modelPBIB$means[1:7, ], ylim=c(0,9), col=0, main="C1", 
## variation="range",border=3,las=2)
## C2<-bar.err(modelPBIB$means[8:15,], ylim=c(0,9), col=0, main="C2", 
## variation="range", border =4,las=2)
## # Others graphic
## C3<-bar.err(modelPBIB$means[16:22,], ylim=c(0,9), col=0, main="C3", 
## variation="range",border =2,las=2)
## C4<-bar.err(modelPBIB$means[23:30,], ylim=c(0,9), col=0, main="C4", 
## variation="range", border =6,las=2)
## # Lattice graphics
## par(mar=c(2.5,2.5,1,0),cex=0.6)
## bar.group(modelLattice$group,ylim=c(0,55),density=10,las=1)


###################################################
### code chunk number 102: f13
###################################################
# model : yield ~ virus
# Important group=TRUE
par(mfrow=c(1,2),mar=c(3,3,1,1),cex=0.8)
x<-duncan.test(model, "virus", group=TRUE)
plot(x,las=1)
plot(x,variation="IQR",horiz=TRUE,las=1)


###################################################
### code chunk number 103: f5
###################################################
# function (x, main = NULL, color1 = "red", color2 = "blue", 
#    color3 = "black", cex.axis = 0.8, las = 1, pch = 20, 
#    bty = "l", cex = 0.8, lwd = 1, xlab = "", ylab = "", 
#   ...)
# model : yield ~ virus
# Important group=FALSE
x<-HSD.test(model, "virus", group=FALSE)
diffograph(x,cex.axis=0.9,xlab="Yield",ylab="Yield",cex=0.9)


###################################################
### code chunk number 104: tutorial.Rnw:1212-1216
###################################################
options(digit=2)
f <- system.file("external/dataStb.csv", package="agricolae")
dataStb<-read.csv(f)
stability.par(dataStb, rep=4, MSerror=1.8, alpha=0.1, main="Genotype",console=TRUE)


###################################################
### code chunk number 105: tutorial.Rnw:1223-1226
###################################################
output <- stability.par(dataStb, rep=4, MSerror=2)
names(output)
print(output$stability)


###################################################
### code chunk number 106: tutorial.Rnw:1231-1235
###################################################
data5<-dataStb[,1:5]
altitude<-c(1200, 1300, 800, 1600, 2400)
stability <- stability.par(data5,rep=4,MSerror=2, cova=TRUE, name.cov= "altitude",
file.cov=altitude)


###################################################
### code chunk number 107: tutorial.Rnw:1242-1246
###################################################
data <- data.frame(name=row.names(dataStb), dataStb)
output<-stability.nonpar(data, "YIELD", ranking=TRUE)
names(output)
output$statistics


###################################################
### code chunk number 108: tutorial.Rnw:1263-1264
###################################################
  str(AMMI)


###################################################
### code chunk number 109: tutorial.Rnw:1267-1268
###################################################
  str(plot.AMMI)


###################################################
### code chunk number 110: tutorial.Rnw:1271-1279
###################################################
data(plrv)
model<-with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
names(model)
model$ANOVA
model$analysis
pc <- model$analysis[, 1]
pc12<-sum(pc[1:2])
pc123<-sum(pc[1:3])


###################################################
### code chunk number 111: f6
###################################################
par(cex=0.4,mar=c(4,4,1,2))
plot(model,type=1,las=1,xlim=c(-5,6))


###################################################
### code chunk number 112: f15
###################################################
par(mar=c(4,4,1,2),cex=0.5)
plot(model,type=3,number=TRUE,las=1,xlim=c(-5,6),bty="l")


###################################################
### code chunk number 113: tutorial.Rnw:1315-1322
###################################################
data(plrv)
model<- with(plrv,AMMI(Locality, Genotype, Rep, Yield, console=FALSE))
index<-index.AMMI(model)
# Crops with improved stability according AMMI.
print(index[order(index[,3]),])
# Crops with better response and improved stability according AMMI.
print(index[order(index[,4]),])


###################################################
### code chunk number 114: f7
###################################################
par(cex=0.6,mar=c(3,3,2,1))
data(pamCIP)
rownames(pamCIP)<-substr(rownames(pamCIP),1,6)
output<-consensus(pamCIP,distance="binary", method="complete", nboot=5)


###################################################
### code chunk number 115: f8
###################################################
par(cex=0.6,mar=c(3,3,1.5,1))
out1<- hcut(output,h=0.4,group=8,type="t",edgePar = list(lty=1:2, col=colors()[c(42,84)]),
main="group 8" ,col.text="blue",cex.text=1,las=1)


###################################################
### code chunk number 116: tutorial.Rnw:1358-1359
###################################################
names(output)


###################################################
### code chunk number 117: tutorial.Rnw:1365-1368
###################################################
dend <- as.dendrogram(output$dendrogram)
data <- output$table.dend
head(output$table.dend)


###################################################
### code chunk number 118: tutorial.Rnw:1370-1373 (eval = FALSE)
###################################################
## par(mar=c(3,3,1,1),cex=0.6)
## plot(dend,type="r",edgePar = list(lty=1:2, col=colors()[c(42,84)]) ,las=1)
## text(data[,3],data[,4],data[,5],col="blue",cex=1)


###################################################
### code chunk number 119: tutorial.Rnw:1380-1384
###################################################
data(soil)
# set.seed(9473)
simulated <- montecarlo(soil$pH,1000)
h<-graph.freq(simulated,nclass=7,plot=FALSE)


###################################################
### code chunk number 120: f9
###################################################
par(mar=c(2,0,2,1),cex=0.6)
plot(density(soil$pH),axes=FALSE,main="pH density of the soil\ncon Ralstonia",xlab="",lwd=4)
lines(density(simulated), col="blue", lty=4,lwd=4)
axis(1,0:12)
legend("topright",c("Original","Simulated"),lty=c(1,4),col=c("black", "blue"), lwd=4)


###################################################
### code chunk number 121: tutorial.Rnw:1399-1400
###################################################
round(table.freq(h),2)


###################################################
### code chunk number 122: tutorial.Rnw:1403-1404
###################################################
summary(soil$pH)


###################################################
### code chunk number 123: tutorial.Rnw:1407-1408
###################################################
summary(simulated)


###################################################
### code chunk number 124: tutorial.Rnw:1414-1421
###################################################
data(potato)
potato[,1]<-as.factor(potato[,1])
potato[,2]<-as.factor(potato[,2])
model<-"cutting~variety + date + variety:date"
analysis<-resampling.model(model, potato, k=100)
Xsol<-as.matrix(round(analysis$solution,2))
print(Xsol,na.print = "")


###################################################
### code chunk number 125: tutorial.Rnw:1430-1431
###################################################
simModel <- simulation.model(model, potato, k=100,console=TRUE)


###################################################
### code chunk number 126: tutorial.Rnw:1433-1434
###################################################
ab<-simModel$simulation[3,3]


###################################################
### code chunk number 127: tutorial.Rnw:1441-1447
###################################################
corr.x<- matrix(c(1,0.5,0.5,1),c(2,2))
corr.y<- rbind(0.6,0.7)
names<-c("X1","X2")
dimnames(corr.x)<-list(names,names)
dimnames(corr.y)<-list(names,"Y")
output<-path.analysis(corr.x,corr.y)


###################################################
### code chunk number 128: tutorial.Rnw:1450-1451
###################################################
output


###################################################
### code chunk number 129: tutorial.Rnw:1486-1494
###################################################
rm(list=ls())
options(digits = 2)
data(heterosis)
str(heterosis)
site2<-subset(heterosis,heterosis[,1]==2)
site2<-subset(site2[,c(2,5,6,8)],site2[,4]!="Control")
output1<-with(site2,lineXtester(Replication, Female, Male, v2))
options(digits = 7)


###################################################
### code chunk number 130: f10
###################################################
par(mar=c(3,3,4,1),cex=0.7)
data(rice)
table<-index.smith(rice, col="blue",
 main="Interaction between the CV and the plot size",type="l",xlab="Size")


###################################################
### code chunk number 131: tutorial.Rnw:1513-1515
###################################################
uniformity <- data.frame(table$uniformity)
head(uniformity)


###################################################
### code chunk number 132: tutorial.Rnw:1524-1527
###################################################
data(paracsho)
species <- paracsho[79:87,4:6]
species


###################################################
### code chunk number 133: tutorial.Rnw:1532-1533
###################################################
output <- index.bio(species[,3],method="Shannon",level=95,nboot=200)


###################################################
### code chunk number 134: tutorial.Rnw:1541-1544
###################################################
data(soil)
correlation(soil[,2:4],method="pearson")
with(soil,correlation(pH,soil[,3:4],method="pearson"))


###################################################
### code chunk number 135: tutorial.Rnw:1555-1557
###################################################
data(RioChillon)
with(RioChillon$babies,tapply.stat(yield,farmer,function(x) max(x)-min(x)))


###################################################
### code chunk number 136: tutorial.Rnw:1576-1579
###################################################
data(sweetpotato)
model <- model<-aov(yield ~ virus, data=sweetpotato)
cv.model(model)


###################################################
### code chunk number 137: tutorial.Rnw:1587-1588
###################################################
x<-c(3,4,5,2,3,4,5,6,4,NA,7)


###################################################
### code chunk number 138: tutorial.Rnw:1593-1594
###################################################
skewness(x)


###################################################
### code chunk number 139: tutorial.Rnw:1599-1600
###################################################
kurtosis(x)


###################################################
### code chunk number 140: tutorial.Rnw:1607-1616
###################################################
q<-5
f<-15
K<-seq(10,1000,100)
n<-length(K)
y<-rep(0,3*n)
dim(y)<-c(n,3)
for(i in 1:n) y[i,1]<-waller(K[i],q,f,Fc=2)
for(i in 1:n) y[i,2]<-waller(K[i],q,f,Fc=4)
for(i in 1:n) y[i,3]<-waller(K[i],q,f,Fc=8)


###################################################
### code chunk number 141: tutorial.Rnw:1622-1629 (eval = FALSE)
###################################################
## par(mar=c(3,3,4,1),cex=0.7)
## plot(K,y[,1],type="l",col="blue",ylab="waller",bty="l")
## lines(K,y[,2],type="l",col="brown",lty=2,lwd=2)
## lines(K,y[,3],type="l",col="green",lty=4,lwd=2)
## legend("topleft",c("2","4","8"),col=c("blue","brown","green"),lty=c(1,8,20),
## lwd=2,title="Fc")
## title(main="Waller in function of K")


###################################################
### code chunk number 142: tutorial.Rnw:1634-1650
###################################################
K<-100
Fc<-1.2
q<-c(seq(6,20,1),30,40,100)
f<-c(seq(4,20,2),24,30)
n<-length(q)
m<-length(f)
W.D <-rep(0,n*m)
dim(W.D)<-c(n,m)
for (i in 1:n) {
for (j in 1:m) {
W.D[i,j]<-waller(K, q[i], f[j], Fc)
}}
W.D<-round(W.D,2)
dimnames(W.D)<-list(q,f)
cat("table: Waller Duncan k=100, F=1.2")
print(W.D)


###################################################
### code chunk number 143: tutorial.Rnw:1657-1662
###################################################
days<-c(7,14,21,28,35,42)
evaluation<-data.frame(E1=10,E2=40,E3=50,E4=70,E5=80,E6=90)
print(evaluation)
absolute1 <-audpc(evaluation,days)
relative1 <-round(audpc(evaluation,days,"relative"),2)


###################################################
### code chunk number 144: tutorial.Rnw:1669-1671
###################################################
absolute2 <-audps(evaluation,days)
relative2 <-round(audps(evaluation,days,"relative"),2)


###################################################
### code chunk number 145: f11
###################################################
par(mfrow=c(1,2),mar=c(3,3,1,1),cex=0.7)
plot(days, evaluation,type="h",ylim=c(0,100),axes=FALSE,col= colors()[42],xlab="Days", ylab="Evaluation")
lines(days,evaluation,col= colors()[42])
axis(1,days)
axis(2,seq(0,100,20),las=2)
rect(7,0,42,100)
text(15,80,substitute(paste("Audpc Abs.=",A1),list(A1=absolute1)))
text(15,70,substitute(paste("Audpc Rel.=",A2),list(A2=relative1)))
x<-seq(3.5,45.5,7)
evaluation<-c(evaluation,evaluation[6])
plot(x, evaluation,type="s",ylim=c(0,100),axes=FALSE,col= colors()[42],xlab="Days", ylab="Evaluation")
points(x,evaluation,type="h",col= colors()[42])
points(days,evaluation[-6],pch=16,col=4)
text(days,5,days,col=4,cex=1)
axis(1,x,pos=0)
axis(2,seq(0,100,20),las=2)
rect(3.5,0,45.5,100)
text(13,80,substitute(paste("Audps Abs.=",A1),list(A1=absolute2)))
text(13,70,substitute(paste("Audps Rel.=",A2),list(A2=relative2)))


###################################################
### code chunk number 146: tutorial.Rnw:1711-1717
###################################################
data(potato)
potato[,1]<-as.factor(potato[,1])
model<-lm(cutting ~ date + variety,potato)
df<-df.residual(model)
MSerror<-deviance(model)/df
analysis<-with(potato,nonadditivity(cutting, date, variety, df, MSerror))


###################################################
### code chunk number 147: tutorial.Rnw:1726-1753
###################################################
options(digits=2)
f <- system.file("external/weather.csv", package="agricolae")
weather <- read.csv(f,header=FALSE)
f <- system.file("external/severity.csv", package="agricolae")
severity <- read.csv(f)
weather[,1]<-as.Date(weather[,1],format = "%m/%d/%Y")
# Parameters dates
dates<-c("2000-03-25","2000-04-09","2000-04-12","2000-04-16","2000-04-22")
dates<-as.Date(dates)
EmergDate <- as.Date("2000/01/19")
EndEpidDate <- as.Date("2000-04-22")
dates<-as.Date(dates)
NoReadingsH<- 1
RHthreshold <- 90
WS<-weatherSeverity(weather,severity,dates,EmergDate,EndEpidDate,
NoReadingsH,RHthreshold)
# Parameters to Lateblight function
InocDate<-"2000-03-18"
LGR <- 0.00410
IniSpor <- 0
SR <- 292000000
IE <- 1.0
LP <- 2.82
InMicCol <- 9
Cultivar <- "NICOLA"
ApplSys <- "NOFUNGICIDE"
main<-"Cultivar: NICOLA"


###################################################
### code chunk number 148: f12
###################################################
par(mar=c(3,3,4,1),cex=0.7)
#--------------------------
model<-lateblight(WS, Cultivar,ApplSys, InocDate, LGR,IniSpor,SR,IE, 
LP,MatTime='LATESEASON',InMicCol,main=main,type="l",xlim=c(65,95),lwd=1.5,
xlab="Time (days after emergence)", ylab="Severity (Percentage)")


###################################################
### code chunk number 149: tutorial.Rnw:1772-1775
###################################################
head(model$Gfile)
str(model$Ofile)
head(model$Ofile[,1:7])


###################################################
### code chunk number 150: tutorial.Rnw:1779-1785
###################################################
x<- model$Ofile$nday
y<- model$Ofile$SimSeverity
w<- model$Gfile$nday
z<- model$Gfile$MeanSeverity
Min<-model$Gfile$MinObs
Max<-model$Gfile$MaxObs


###################################################
### code chunk number 151: tutorial.Rnw:1788-1795 (eval = FALSE)
###################################################
## par(mar=c(3,2.5,1,1),cex=0.7)
## plot(x,y,type="l",xlim=c(65,95),lwd=1.5,xlab="Time (days after emergence)",
## ylab="Severity (Percentage)")
## points(w,z,col="red",cex=1,pch=19); npoints <- length(w)
## for ( i in 1:npoints)segments(w[i],Min[i],w[i],Max[i],lwd=1.5,col="red")
## legend("topleft",c("Disease progress curves","Weather-Severity"),
## title="Description",lty=1,pch=c(3,19),col=c("black","red"))


