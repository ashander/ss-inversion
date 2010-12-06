#  Version 0.1 -- Collin Edwards
#  It should make a couple of pretty graphs - the one on the left is the total population sizes, the one on the right is the relative proportions of e and d
#

source('source-sink.R')

numYears=1000

###################

#Parameters:
size = list(E=1000,D=200)
lambda = list(Ee=1.5, Ed=.7, De=.9, Dd=4.93)

#For initializing: give intial ratio of e to d, plus the number of individuals in ND and NE
initdToTot=.1          #initial d to total ratio
initTotPop=size$E+.1*size$D    #total size of combined populations

#############################
#############################
#Actual code:
#creating vectors
Ntotd=numeric(numYears)
Ntote=Ntotd
Ntot=Ntotd
NDd=Ntotd
NDe=Ntotd
NEd=Ntotd
NEe=Ntotd
#initializing vectors
Ntotd[1]=initdToTot*(initTotPop)
Ntote[1]=(1-initdToTot)*(initTotPop)
Ntot=initTotPop
INIT = Disp(Ntotd[1], Ntote[1], K=size)
NDd[1]=INIT[1]
NDe[1]=INIT[2]
NEd[1]=INIT[3]
NEe[1]=INIT[4]

#############
#first year is 


for(t in 2:numYears){
	TMP= Repro(NDd[t-1], NDe[t-1], NEd[t-1], NEe[t-1], L=lambda)
	Ntotd[t]=TMP[1]
	Ntote[t]=TMP[2]
	Ntot[t]=Ntotd[t]+Ntote[t] #total number of individuals of any type
	OUT = Disp(TMP[1],TMP[2],K=size)
	NDd[t]=OUT[1]
	NDe[t]=OUT[2]
	NEd[t]=OUT[3]
	NEe[t]=OUT[4]
}

par(mfrow=c(1,2))
plot(c(1,numYears),c(0,Ntot[numYears]),col="transparent",main="Population sizes",xlab="time",ylab="Population size")
legend(.7*numYears,.9*Ntot[numYears], c("Total Pop","e","d"), cex=0.6, col=c("black","green","brown"), lty=1);
lines(1:numYears,Ntot,col="black")
lines(1:numYears,Ntotd,col="brown")
lines(1:numYears,Ntote,col="green")
plot(c(1,numYears),c(0,1),main="Ratios of d and e",xlab="time",ylab="Proportion",col="transparent")
legend(.55*numYears,.9, c("Proportion that is e","proportion that is d"), cex=0.6, col=c("green","brown"), lty=1);
lines(1:numYears,Ntotd/Ntot,col="brown")
lines(1:numYears,Ntote/Ntot,col="green")

