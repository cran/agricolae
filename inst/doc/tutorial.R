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
### code chunk number 3: tutorial.Rnw:50-51
###################################################
library(agricolae)


###################################################
### code chunk number 4: tutorial.Rnw:56-58 (eval = FALSE)
###################################################
## help(package="agricolae")
## help(waller.test)


###################################################
### code chunk number 5: tutorial.Rnw:71-75
###################################################
detach(package:agricolae) # detach package agricole
library(agricolae) # Load the package to the memory
designs<-apropos("design")
print(designs[substr(designs,1,6)=="design"], row.names=FALSE)


###################################################
### code chunk number 6: tutorial.Rnw:82-83
###################################################
library(agricolae) # Load the package to the memory: 


###################################################
### code chunk number 7: tutorial.Rnw:94-97
###################################################
A<-as.data.frame(data(package="agricolae")$results[,3:4])
A[,2]<-paste(substr(A[,2],1,35),"..",sep=".")
head(A)


###################################################
### code chunk number 8: tutorial.Rnw:106-109
###################################################
weight<-c( 68, 53, 69.5, 55, 71, 63, 76.5, 65.5, 69, 75, 76, 57, 70.5, 71.5, 56, 81.5,
           69, 59, 67.5, 61, 68, 59.5, 56.5, 73, 61, 72.5, 71.5, 59.5, 74.5, 63)
print(summary(weight)) 


###################################################
### code chunk number 9: f1
###################################################
par(mfrow=c(1,2),mar=c(4,3,0,1),cex=0.6)
h1<- graph.freq(weight,col="yellow",frequency=1,las=2,xlab="h1") 
h2<- graph.freq (weight, frequency =2, axes= FALSE,las=2,xlab="h2") 
polygon.freq(h2, col="blue", lwd=2, frequency =2)
TIC<- h2$breaks[2]- h2$breaks[1]
axis(1,c(h2$mids[1]-TIC, h2$mids, h2$mids[6]+TIC ),cex=0.6)
axis(2, cex=0.6,las=1) 


###################################################
### code chunk number 10: tutorial.Rnw:129-130
###################################################
stat.freq(h1)


###################################################
### code chunk number 11: tutorial.Rnw:143-144
###################################################
print(summary(h1))


###################################################
### code chunk number 12: tutorial.Rnw:149-153
###################################################
sturges.freq(weight)
intervals.freq(h1)
join.freq(h1,1:3) -> h3
print(summary(h3))


###################################################
### code chunk number 13: f2
###################################################
par(mfrow=c(1,2),mar=c(4,3,0,1),cex=0.6)
plot(h3, frequency=2,col="magenta",ylim=c(0,0.6))
normal.freq(h3,frequency=2,col="green")
ogive.freq(h3,col="blue")


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
### code chunk number 15: tutorial.Rnw:195-196
###################################################
print(summary(h5))


###################################################
### code chunk number 16: tutorial.Rnw:239-245
###################################################
str(design.crd)
trt <- c("A", "B", "C")
repeticion <- c(4, 3, 4)
outdesign <- design.crd(trt,r=repeticion,seed=777,serie=0)
book1 <- outdesign$book
head(book1)


###################################################
### code chunk number 17: tutorial.Rnw:252-261
###################################################
str(design.rcbd)

trt <- c("A", "B", "C","D","E")
repeticion <- 4
outdesign <- design.rcbd(trt,r=repeticion, seed=-513, serie=2)
# book2 <- outdesign$book
book2<- zigzag(outdesign) # zigzag numeration
print(t(matrix(book2[,3],c(5,4))))
print(t(matrix(book2[,1],c(5,4))),digits=0)


###################################################
### code chunk number 18: tutorial.Rnw:265-270
###################################################
str(design.lsd)
trt <- c("A", "B", "C", "D")
outdesign <- design.lsd(trt, seed=543, serie=2)
book3 <- outdesign$book
print(t(matrix(book3[,4],c(4,4))))


###################################################
### code chunk number 19: tutorial.Rnw:273-275
###################################################
book <- zigzag(outdesign)
print(t(matrix(book[,1],c(4,4))),digit=0)


###################################################
### code chunk number 20: tutorial.Rnw:280-286
###################################################
str(design.graeco)
trt1 <- c("A", "B", "C", "D")
trt2 <- 1:4
outdesign <- design.graeco(trt1,trt2, seed=543, serie=2)
book4 <- outdesign$book
print(t(matrix(paste(book4[,4], book4[,5]),c(4,4))))


###################################################
### code chunk number 21: tutorial.Rnw:289-291
###################################################
book <- zigzag(outdesign)
print(t(matrix(book[,1],c(4,4))),digit=0)


###################################################
### code chunk number 22: tutorial.Rnw:296-307
###################################################
str(design.youden)
varieties<-c("perricholi","yungay","maria bonita","tomasa")
outdesign <-design.youden(varieties,r=3,serie=2,seed=23)
youden <- outdesign$book
print(youden) # field book.
plots <-as.numeric(youden[,1])
trt <-as.character(youden[,4])
dim(plots)<-c(3,4)
dim(trt) <-c(3,4)
print(t(plots))
print(t(trt))


###################################################
### code chunk number 23: tutorial.Rnw:310-312
###################################################
book <- zigzag(outdesign)
print(t(matrix(book[,1],c(3,4))),digit=0)


###################################################
### code chunk number 24: tutorial.Rnw:318-325
###################################################
str(design.bib)
trt <- c("A", "B", "C", "D", "E" )
k <- 4
outdesign <- design.bib(trt,k, seed=543, serie=2)
book5 <- outdesign$book
outdesign$statistics
outdesign$parameters


###################################################
### code chunk number 25: tutorial.Rnw:328-329
###################################################
t(matrix(book5[,3],c(4,5)))


###################################################
### code chunk number 26: tutorial.Rnw:334-336
###################################################
book <- zigzag(outdesign)
t(matrix(book[,1],c(4,5)))


###################################################
### code chunk number 27: tutorial.Rnw:343-349
###################################################
str(design.cyclic)
trt <- c("A", "B", "C", "D", "E", "F" )
outdesign <- design.cyclic(trt,k=3, r=6, seed=543, serie=2)
book6 <- outdesign$book
outdesign$sketch[[1]]
outdesign$sketch[[2]]


###################################################
### code chunk number 28: tutorial.Rnw:354-358
###################################################
book <- zigzag(outdesign)
array(book$plots,c(3,6,2))->X
t(X[,,1])
t(X[,,2])


###################################################
### code chunk number 29: tutorial.Rnw:363-364
###################################################
str(design.lattice)


###################################################
### code chunk number 30: tutorial.Rnw:368-375
###################################################
trt<-letters[1:9]
outdesign <-design.lattice(trt, r = 3, serie = 2, seed = 33, 
    kinds =  "Super-Duper")
book7 <- outdesign$book
outdesign$parameters
outdesign$sketch
head(book7)


###################################################
### code chunk number 31: tutorial.Rnw:378-383
###################################################
book <- zigzag(outdesign)
array(book$plots,c(3,3,3)) -> X
t(X[,,1])
t(X[,,2])
t(X[,,3])


###################################################
### code chunk number 32: tutorial.Rnw:388-398
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
### code chunk number 33: tutorial.Rnw:401-405
###################################################
book <- zigzag(outdesign)
A<-array(book[,1], c(3,5,2))
t(A[,,1])
t(A[,,2])


###################################################
### code chunk number 34: tutorial.Rnw:411-420
###################################################
str(design.dau)
rm(list=ls())
trt1 <- c("A", "B", "C", "D")
trt2 <- c("t","u","v","w","x","y","z")
outdesign <- design.dau(trt1, trt2, r=5, seed=543, serie=2)
book9 <- outdesign$book
attach(book9)
by(trt, block,as.character)
detach(book9)


###################################################
### code chunk number 35: tutorial.Rnw:423-428
###################################################
book <- zigzag(outdesign)
attach(book)
by(plots, block, as.character)
detach(book)
head(book)


###################################################
### code chunk number 36: tutorial.Rnw:434-435
###################################################
str(design.split)


###################################################
### code chunk number 37: tutorial.Rnw:439-448
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
### code chunk number 38: tutorial.Rnw:451-452
###################################################
print(t(matrix(p,c(4,3))))


###################################################
### code chunk number 39: tutorial.Rnw:455-456
###################################################
print(t(matrix(q,c(4,3))))


###################################################
### code chunk number 40: tutorial.Rnw:459-461
###################################################
book <- zigzag(outdesign)
head(book,5)


###################################################
### code chunk number 41: tutorial.Rnw:465-466
###################################################
str(design.strip)


###################################################
### code chunk number 42: tutorial.Rnw:469-481
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
### code chunk number 43: tutorial.Rnw:484-490
###################################################
book <- zigzag(outdesign)
head(book)
array(book$plots,c(3,4,3))->X
t(X[,,1])
t(X[,,2])
t(X[,,3])


###################################################
### code chunk number 44: tutorial.Rnw:494-495
###################################################
str(design.ab)


###################################################
### code chunk number 45: tutorial.Rnw:498-499
###################################################
trt <- c (4,2,3) # three factors with  4,2 and 3 levels.


###################################################
### code chunk number 46: tutorial.Rnw:503-507
###################################################
trt<-c(3,2) # factorial 3x2
outdesign <-design.ab(trt, r=3, serie=2)
book12 <- outdesign$book
head(book12) # print of the field book


###################################################
### code chunk number 47: tutorial.Rnw:511-513
###################################################
book <- zigzag(outdesign)
head(book)


###################################################
### code chunk number 48: tutorial.Rnw:517-522
###################################################
trt<-c(2,2,2)
crd<-design.ab(trt, r=5, serie=2,design="crd")
names(crd)
crd$parameters
head(crd$book)


###################################################
### code chunk number 49: tutorial.Rnw:533-539
###################################################
data(sweetpotato)
model<-aov(yield~virus, data=sweetpotato)
cv.model(model)
attach(sweetpotato)
mean(yield)
detach(sweetpotato)


###################################################
### code chunk number 50: tutorial.Rnw:542-544
###################################################
df<-df.residual(model)
MSerror<-deviance(model)/df


###################################################
### code chunk number 51: tutorial.Rnw:551-553
###################################################
# comparison <- LSD.test(yield,virus,df,MSerror)
LSD.test(model, "virus",console=TRUE)


###################################################
### code chunk number 52: tutorial.Rnw:558-560
###################################################
# comparison <- LSD.test(yield, virus,df, MSerror, group=F)
outLSD <-LSD.test(model, "virus", group=F,console=TRUE)


###################################################
### code chunk number 53: tutorial.Rnw:567-568
###################################################
print(outLSD)


###################################################
### code chunk number 54: tutorial.Rnw:573-574
###################################################
LSD.test(model, "virus", group=F, p.adj= "bon",console=TRUE)


###################################################
### code chunk number 55: tutorial.Rnw:583-584
###################################################
duncan.test(model, "virus",console=TRUE)


###################################################
### code chunk number 56: tutorial.Rnw:590-592
###################################################
# SNK.test(model, "virus", alpha=0.05,console=TRUE)
SNK.test(model, "virus", group=FALSE,console=TRUE)


###################################################
### code chunk number 57: tutorial.Rnw:599-601
###################################################
outHSD<- HSD.test(model, "virus",console=TRUE)
outHSD


###################################################
### code chunk number 58: tutorial.Rnw:608-613
###################################################
# variance analysis:
anova(model)
attach(sweetpotato)
waller.test(yield,virus,df,MSerror,Fc= 17.345, group=F,console=TRUE)
detach(sweetpotato)


###################################################
### code chunk number 59: tutorial.Rnw:617-618
###################################################
outWaller <- waller.test(model, "virus", group=FALSE,console=FALSE)


###################################################
### code chunk number 60: tutorial.Rnw:622-624
###################################################
names(outWaller)
print(outWaller$comparison)


###################################################
### code chunk number 61: tutorial.Rnw:627-628
###################################################
outWaller$statistics


###################################################
### code chunk number 62: tutorial.Rnw:634-637
###################################################
# analysis of variance: 
scheffe.test(model,"virus", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")


###################################################
### code chunk number 63: tutorial.Rnw:641-642
###################################################
outScheffe <- scheffe.test(model,"virus", group=FALSE, console=TRUE)


###################################################
### code chunk number 64: tutorial.Rnw:647-649
###################################################
# modelABC <-aov (y ~ A * B * C, data)
# compare <-LSD.test (modelABC, c ("A", "B", "C"),console=TRUE)


###################################################
### code chunk number 65: tutorial.Rnw:654-669
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
anova (outAOV)
outFactorial <-LSD.test (outAOV, c("clone", "nitrogen"), 
main = "Yield ~ block + nitrogen + clone + clone:nitrogen",console=TRUE)


###################################################
### code chunk number 66: tutorial.Rnw:672-677 (eval = FALSE)
###################################################
## par(mar=c(3,3,2,0))
## pic1<-bar.err(outFactorial$means,variation="range",ylim=c(5,25), bar=FALSE,col=0,las=1)
## points(pic1$index,pic1$means,pch=18,cex=1.5,col="blue")
## axis(1,pic1$index,labels=FALSE)
## title(main="average and range\nclon:nitrogen")


###################################################
### code chunk number 67: tutorial.Rnw:684-692
###################################################
#Example linear estimation and design of experiments. (Joshi)
# Profesor de Estadistica, Institute of Social Sciences Agra, India
# 6 variedades de trigo en 10 bloques de 3 parcelas cada una.
block<-gl(10,3)
variety<-c(1,2,3,1,2,4,1,3,5,1,4,6,1,5,6,2,3,6,2,4,5,2,5,6,3,4,5,3, 4,6)
y<-c(69,54,50,77,65,38,72,45,54,63,60,39,70,65,54,65,68,67,57,60,62, 
59,65,63,75,62,61,59,55,56)
BIB.test(block, variety, y,console=TRUE)


###################################################
### code chunk number 68: tutorial.Rnw:695-698
###################################################
out <-BIB.test(block, trt=variety, y, test="tukey", group=FALSE, console=TRUE)
names(out)
rm(block,variety)


###################################################
### code chunk number 69: tutorial.Rnw:711-716
###################################################
# alpha design 
Genotype<-paste("geno",1:30,sep="")
r<-2
k<-3
plan<-design.alpha(Genotype,k,r,seed=5)


###################################################
### code chunk number 70: tutorial.Rnw:722-724
###################################################
yield <-c(5,2,7,6,4,9,7,6,7,9,6,2,1,1,3,2,4,6,7,9,8,7,6,4,3,2,2,1,1, 
          2,1,1,2,4,5,6,7,8,6,5,4,3,1,1,2,5,4,2,7,6,6,5,6,4,5,7,6,5,5,4)


###################################################
### code chunk number 71: tutorial.Rnw:727-734
###################################################
data<-data.frame(plan$book,yield)
rm(yield,Genotype)
# The analysis: 
attach(data)
modelPBIB <- PBIB.test(block, Genotype, replication, yield, k=3, group=TRUE,
console=TRUE)
detach(data)


###################################################
### code chunk number 72: tutorial.Rnw:767-772
###################################################
rm(trt)
A<-read.table("lattice3X3.txt", header=T)
attach(A)
modelLattice<-PBIB.test(block,trt,sqr,yield,k=3,console=TRUE)
detach(A)


###################################################
### code chunk number 73: tutorial.Rnw:787-792
###################################################
block<-c(rep("I",7),rep("II",6),rep("III",7))
trt<-c("A","B","C","D","g","k","l","A","B","C","D","e","i","A","B", "C",
"D","f","h","j")
yield<-c(83,77,78,78,70,75,74,79,81,81,91,79,78,92,79,87,81,89,96, 82)
head(data.frame(block, trt, yield))


###################################################
### code chunk number 74: tutorial.Rnw:795-796
###################################################
by(trt,block,as.character)


###################################################
### code chunk number 75: tutorial.Rnw:799-800
###################################################
by(yield,block,as.character)


###################################################
### code chunk number 76: tutorial.Rnw:803-805
###################################################
modelDAU<- DAU.test(block,trt,yield,method="lsd",console=TRUE)
modelDAU$means


###################################################
### code chunk number 77: tutorial.Rnw:807-809
###################################################
modelDAU<- DAU.test(block,trt,yield,method="lsd",group=F,console=FALSE)
head(modelDAU$comparison,8)


###################################################
### code chunk number 78: tutorial.Rnw:826-828
###################################################
data(corn)
str(corn)


###################################################
### code chunk number 79: tutorial.Rnw:833-834
###################################################
str(kruskal)


###################################################
### code chunk number 80: tutorial.Rnw:837-840
###################################################
attach(corn)
outKruskal<-kruskal(observation,method,group=TRUE, main="corn", console=TRUE)
detach(corn)


###################################################
### code chunk number 81: tutorial.Rnw:845-846
###################################################
str(friedman)


###################################################
### code chunk number 82: tutorial.Rnw:849-855
###################################################
rm(trt)
data(grass)
attach(grass)
out<-friedman(judge,trt, evaluation,alpha=0.05, group=FALSE,
main="Data of the book of Conover",console=TRUE)
detach(grass)


###################################################
### code chunk number 83: tutorial.Rnw:859-860
###################################################
str(waerden.test)


###################################################
### code chunk number 84: tutorial.Rnw:863-867
###################################################
rm(yield)
data(sweetpotato)
attach(sweetpotato)
outWaerden<-waerden.test(yield,virus,alpha=0.01,group=TRUE,console=TRUE)


###################################################
### code chunk number 85: tutorial.Rnw:870-871
###################################################
names(outWaerden)


###################################################
### code chunk number 86: tutorial.Rnw:874-876
###################################################
out<-waerden.test(yield,virus,group=F,console=TRUE)
detach(sweetpotato)


###################################################
### code chunk number 87: tutorial.Rnw:880-881
###################################################
str(Median.test)


###################################################
### code chunk number 88: tutorial.Rnw:884-891
###################################################
data(sweetpotato)
attach(sweetpotato)
outMedian<-Median.test(yield,virus,console=TRUE)
detach(sweetpotato)
names(outMedian)
outMedian$statistics
outMedian$Medians


###################################################
### code chunk number 89: tutorial.Rnw:896-897
###################################################
str(durbin.test)


###################################################
### code chunk number 90: tutorial.Rnw:900-909
###################################################
days <-gl(7,3)
chemical<-c("A","B","D","A","C","E","C","D","G","A","F","G", "B","C","F", 
"B","E","G","D","E","F")
toxic<-c(0.465,0.343,0.396,0.602,0.873,0.634,0.875,0.325,0.330, 0.423,0.987,0.426,
0.652,1.142,0.989,0.536,0.409,0.309, 0.609,0.417,0.931)
out<-durbin.test(days,chemical,toxic,group=F,console=TRUE,
main="Logarithm of the toxic dose")
names(out)
out$statistics


###################################################
### code chunk number 91: tutorial.Rnw:921-925
###################################################
# model <-aov (yield ~ fertilizer, data = field) 
# out <-LSD.test (model, "fertilizer", group = TRUE) 
# bar.group (out $ group)
str(bar.group)


###################################################
### code chunk number 92: tutorial.Rnw:934-938
###################################################
# model <-aov (yield ~ fertilizer, data = field) 
# out <-LSD.test (model, "fertilizer", group = TRUE) 
# bar.err(out$means)
str(bar.err)


###################################################
### code chunk number 93: f4
###################################################
par(mfrow=c(1,2),mar=c(3,3,2,0),cex=0.7)
c1<-colors()[480]; c2=colors()[65]; c3=colors()[15]; c4=colors()[140] 
G1<-bar.group(outHSD$groups, ylim=c(0,45), main="Tukey\nG1",col=c1,las=1)
# G2<-bar.group(outHSD$groups, horiz=T, xlim=c(0,45), main="Tukey\nG2",col=c2,las=1)
# G3<-bar.err(outWaller$means, variation="range",ylim=c(0,45), col=c3,main="Range\nG3",las=1)
G4<-bar.err(outWaller$means, horiz=T, xlim=c(0,45), col=c4, variation="SE",
main="Standard error \nG4",las=1)


###################################################
### code chunk number 94: tutorial.Rnw:960-973 (eval = FALSE)
###################################################
## par(mfrow=c(2,2),cex=0.7,mar=c(3.5,1.5,3,0))
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
### code chunk number 95: tutorial.Rnw:984-990
###################################################
options(digit=2)
v1 <- c(10.2,8.8,8.8,9.3,9.6,7.2,8.4,9.6,7.9,10,9.3,8.0,10.1,9.4,10.8,6.3,7.4)
v2 <- c(7,7.8,7.0,6.9,7,8.3,7.4,6.5,6.8,7.9,7.3,6.8,8.1,7.1,7.1,6.4,4.1)
v3 <- c(5.3,4.4,5.3,4.4,5.5,4.6,6.2,6.0,6.5,5.3,5.7,4.4,4.2,5.6,5.8,3.9,3.8)
v4 <- c(7.8,5.9,7.3,5.9,7.8,6.3,7.9,7.5,7.6,5.4,5.6,7.8,6.5,8.1,7.5,5.0,5.4)
v5 <- c(9,9.2,8.8,10.6,8.3,9.3,9.6,8.8,7.9, 9.1,7.7,9.5,9.4,9.4,10.3,8.8,8.7)


###################################################
### code chunk number 96: tutorial.Rnw:993-995
###################################################
study <- data.frame(v1, v2, v3, v4, v5)
rownames(study) <- LETTERS[1:17]


###################################################
### code chunk number 97: tutorial.Rnw:1000-1003
###################################################
output <- stability.par(study, rep=4, MSerror=2)
names(output)
print(output$stability)


###################################################
### code chunk number 98: tutorial.Rnw:1008-1011
###################################################
altitude<-c(1200, 1300, 800, 1600, 2400)
stability <- stability.par(study,rep=4,MSerror=2, cova=TRUE, name.cov= "altitude",
file.cov=altitude)


###################################################
### code chunk number 99: tutorial.Rnw:1018-1022
###################################################
data <- data.frame(name=row.names(study), study)
output<-stability.nonpar(data, "YIELD", ranking=TRUE)
names(output)
output$statistics


###################################################
### code chunk number 100: tutorial.Rnw:1039-1040
###################################################
str(AMMI)


###################################################
### code chunk number 101: tutorial.Rnw:1043-1044
###################################################
str(plot.AMMI)


###################################################
### code chunk number 102: tutorial.Rnw:1047-1055
###################################################
rdto <- c(study[,1], study[,2], study[,3], study[,4], study[,5])
environment <- gl(5,17)
genotype <- rep(rownames(study),5)
model<-AMMI(ENV=environment, GEN=genotype, REP=4, Y=rdto, MSE=2, console=TRUE)
pc <- model$analysis[, 1]
pc12<-sum(pc[1:2])
pc123<-sum(pc[1:3])
rm(rdto,environment,genotype)


###################################################
### code chunk number 103: f5
###################################################
par(cex=0.8,mar=c(4,4,1,0))
plot(model,type=1,las=1)


###################################################
### code chunk number 104: tutorial.Rnw:1081-1090
###################################################
data(plrv)
attach(plrv)
model<- AMMI(Locality, Genotype, Rep, Yield, console=FALSE)
detach(plrv)
index<-index.AMMI(model)
# Crops with improved stability according AMMI.
print(index[order(index[,3]),])
# Crops with better response and improved stability according AMMI.
print(index[order(index[,4]),])


###################################################
### code chunk number 105: f6
###################################################
par(cex=0.6,mar=c(3,3,2,0))
data(pamCIP)
rownames(pamCIP)<-substr(rownames(pamCIP),1,6)
output<-consensus(pamCIP,distance="binary", method="complete", nboot=5)


###################################################
### code chunk number 106: f7
###################################################
par(cex=0.6,mar=c(3,3,1.5,0))
out1<- hcut(output,h=0.4,group=8,type="t",edgePar = list(lty=1:2, col=colors()[c(42,84)]),
main="group 8" ,col.text="blue",cex.text=1,las=1)


###################################################
### code chunk number 107: tutorial.Rnw:1126-1127
###################################################
names(output)


###################################################
### code chunk number 108: tutorial.Rnw:1133-1136
###################################################
dend <- as.dendrogram(output$dendrogram)
data <- output$table.dend
head(output$table.dend)


###################################################
### code chunk number 109: tutorial.Rnw:1138-1141 (eval = FALSE)
###################################################
## par(mar=c(3,3,1,1),cex=0.6)
## plot(dend,type="r",edgePar = list(lty=1:2, col=colors()[c(42,84)]) ,las=1)
## text(data[,3],data[,4],data[,5],col="blue",cex=1)


###################################################
### code chunk number 110: tutorial.Rnw:1148-1152
###################################################
data(soil)
# set.seed(9473)
simulated <- montecarlo(soil$pH,1000)
h<-graph.freq(simulated,nclass=7,plot=FALSE)


###################################################
### code chunk number 111: f8
###################################################
par(mar=c(2,0,2,1),cex=0.6)
plot(density(soil$pH),axes=F,main="pH density of the soil\ncon Ralstonia",xlab="",lwd=4)
lines(density(simulated), col="blue", lty=4,lwd=4)
axis(1,0:12)
legend("topright",c("Original","Simulated"),lty=c(1,4),col=c("black", "blue"), lwd=4)


###################################################
### code chunk number 112: tutorial.Rnw:1167-1168
###################################################
round(table.freq(h),2)


###################################################
### code chunk number 113: tutorial.Rnw:1171-1172
###################################################
summary(soil$pH)


###################################################
### code chunk number 114: tutorial.Rnw:1175-1176
###################################################
summary(simulated)


###################################################
### code chunk number 115: tutorial.Rnw:1182-1189
###################################################
data(potato)
potato[,1]<-as.factor(potato[,1])
potato[,2]<-as.factor(potato[,2])
model<-"cutting~variety + date + variety:date"
analysis<-resampling.model(model, potato, k=100)
Xsol<-as.matrix(round(analysis$solution,2))
print(Xsol,na.print = "")


###################################################
### code chunk number 116: tutorial.Rnw:1198-1199
###################################################
simModel <- simulation.model(model, potato, k=100,console=TRUE)


###################################################
### code chunk number 117: tutorial.Rnw:1201-1202
###################################################
ab<-simModel$simulation[3,3]


###################################################
### code chunk number 118: tutorial.Rnw:1209-1215
###################################################
corr.x<- matrix(c(1,0.5,0.5,1),c(2,2))
corr.y<- rbind(0.6,0.7)
names<-c("X1","X2")
dimnames(corr.x)<-list(names,names)
dimnames(corr.y)<-list(names,"Y")
output<-path.analysis(corr.x,corr.y)


###################################################
### code chunk number 119: tutorial.Rnw:1217-1218
###################################################
output


###################################################
### code chunk number 120: tutorial.Rnw:1252-1259
###################################################
rm(list=ls())
data(heterosis)
site2<-subset(heterosis,heterosis[,1]==2)
site2<-subset(site2[,c(2,5,6,8)],site2[,4]!="Control")
attach(site2)
output1<-lineXtester(Replication, Female, Male, v2)
detach(site2)


###################################################
### code chunk number 121: f9
###################################################
par(mar=c(3,3,4,0),cex=0.7)
data(rice)
table<-index.smith(rice,pch=19, col="blue",
 main="Interaction between the CV and the plot size",type="l",xlab="Size")


###################################################
### code chunk number 122: tutorial.Rnw:1277-1279
###################################################
uniformity <- data.frame(table$uniformity)
head(uniformity)


###################################################
### code chunk number 123: tutorial.Rnw:1287-1290
###################################################
data(paracsho)
species <- paracsho[79:87,4:6]
species


###################################################
### code chunk number 124: tutorial.Rnw:1293-1294
###################################################
output <- index.bio(species[,3],method="Shannon",level=95,nboot=200)


###################################################
### code chunk number 125: tutorial.Rnw:1300-1306
###################################################
data(soil)
correlation(soil[,2:4],method="pearson")
attach(soil)
correlation(pH,soil[,3:4],method="pearson")
correlation(pH,CaCO3,method="pearson")
detach(soil)


###################################################
### code chunk number 126: tutorial.Rnw:1317-1321
###################################################
data(RioChillon)
attach(RioChillon$babies)
tapply.stat(yield,farmer,function(x) max(x)-min(x))
detach(RioChillon$babies)


###################################################
### code chunk number 127: tutorial.Rnw:1336-1339
###################################################
data(sweetpotato)
model <- model<-aov(yield ~ virus, data=sweetpotato)
cv.model(model)


###################################################
### code chunk number 128: tutorial.Rnw:1346-1347
###################################################
x<-c(3,4,5,2,3,4,5,6,4,NA,7)


###################################################
### code chunk number 129: tutorial.Rnw:1350-1351
###################################################
skewness(x)


###################################################
### code chunk number 130: tutorial.Rnw:1354-1355
###################################################
kurtosis(x)


###################################################
### code chunk number 131: tutorial.Rnw:1361-1370
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
### code chunk number 132: tutorial.Rnw:1374-1381 (eval = FALSE)
###################################################
## par(mar=c(3,3,4,0),cex=0.7)
## plot(K,y[,1],type="l",col="blue",ylab="waller",bty="l")
## lines(K,y[,2],type="l",col="brown",lty=2,lwd=2)
## lines(K,y[,3],type="l",col="green",lty=4,lwd=2)
## legend("topleft",c("2","4","8"),col=c("blue","brown","green"),lty=c(1,8,20),
## lwd=2,title="Fc")
## title(main="Waller in function of K")


###################################################
### code chunk number 133: tutorial.Rnw:1385-1401
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
### code chunk number 134: tutorial.Rnw:1407-1412
###################################################
days<-c(7,14,21,28,35,42)
evaluation<-data.frame(E1=10,E2=40,E3=50,E4=70,E5=80,E6=90)
print(evaluation)
absolute1 <-audpc(evaluation,days)
relative1 <-round(audpc(evaluation,days,"relative"),2)


###################################################
### code chunk number 135: tutorial.Rnw:1418-1420
###################################################
absolute2 <-audps(evaluation,days)
relative2 <-round(audps(evaluation,days,"relative"),2)


###################################################
### code chunk number 136: f10
###################################################
par(mfrow=c(1,2),mar=c(3,3,1,0),cex=0.7)
plot(days, evaluation,type="h",ylim=c(0,100),axes=F,col= colors()[42],xlab="Days", ylab="Evaluation")
lines(days,evaluation,col= colors()[42])
axis(1,days)
axis(2,seq(0,100,20),las=2)
rect(7,0,42,100)
text(15,80,substitute(paste("Audpc Abs.=",A1),list(A1=absolute1)))
text(15,70,substitute(paste("Audpc Rel.=",A2),list(A2=relative1)))
x<-seq(3.5,45.5,7)
evaluation<-c(evaluation,evaluation[6])
plot(x, evaluation,type="s",ylim=c(0,100),axes=F,col= colors()[42],xlab="Days", ylab="Evaluation")
points(x,evaluation,type="h",col= colors()[42])
points(days,evaluation[-6],pch=16,col=4)
text(days,5,days,col=4,cex=1)
axis(1,x,pos=0)
axis(2,seq(0,100,20),las=2)
rect(3.5,0,45.5,100)
text(13,80,substitute(paste("Audps Abs.=",A1),list(A1=absolute2)))
text(13,70,substitute(paste("Audps Rel.=",A2),list(A2=relative2)))


###################################################
### code chunk number 137: tutorial.Rnw:1457-1465
###################################################
data(potato)
potato[,1]<-as.factor(potato[,1])
model<-lm(cutting ~ date + variety,potato)
df<-df.residual(model)
MSerror<-deviance(model)/df
attach(potato)
analysis<-nonadditivity(cutting, date, variety, df, MSerror)
detach(potato)


###################################################
### code chunk number 138: tutorial.Rnw:1474-1500
###################################################
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
### code chunk number 139: f11
###################################################
par(mar=c(3,3,4,0),cex=0.7)
#--------------------------
model<-lateblight(WS, Cultivar,ApplSys, InocDate, LGR,IniSpor,SR,IE, 
LP,MatTime='LATESEASON',InMicCol,main=main,type="l",xlim=c(65,95),lwd=1.5,
xlab="Time (days after emergence)", ylab="Severity (Percentage)")


###################################################
### code chunk number 140: tutorial.Rnw:1515-1518
###################################################
head(model$Gfile)
str(model$Ofile)
head(model$Ofile[,1:7])


###################################################
### code chunk number 141: tutorial.Rnw:1522-1528
###################################################
x<- model$Ofile$nday
y<- model$Ofile$SimSeverity
w<- model$Gfile$nday
z<- model$Gfile$MeanSeverity
Min<-model$Gfile$MinObs
Max<-model$Gfile$MaxObs


###################################################
### code chunk number 142: tutorial.Rnw:1530-1537 (eval = FALSE)
###################################################
## par(mar=c(3,2.5,1,0),cex=0.7)
## plot(x,y,type="l",xlim=c(65,95),lwd=1.5,xlab="Time (days after emergence)",
## ylab="Severity (Percentage)")
## points(w,z,col="red",cex=1,pch=19); npoints <- length(w)
## for ( i in 1:npoints)segments(w[i],Min[i],w[i],Max[i],lwd=1.5,col="red")
## legend("topleft",c("Disease progress curves","Weather-Severity"),
## title="Description",lty=1,pch=c(3,19),col=c("black","red"))


