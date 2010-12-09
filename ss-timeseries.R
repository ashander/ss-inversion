#  Version 0.1 -- Collin Edwards
#  It should make a couple of pretty graphs - the one on the left is the total population sizes, the one on the right is the relative proportions of e and d
#

source('source-sink.R')

numYears=500

###################

#Parameters:
size = list(E=1000,D=200)
lamEe = 1.5
lambda = list(Ee=lamEe, Ed=.7, De=.9, Dd=1.1*lamEe)

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

#number of eggs per each
NEeggs = Ntotd
NDeggs = Ntotd


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

plot(c(1,numYears),c(0,7.5),col="transparent", xlab="time",ylab=expression(eta[E/D]))
lams =c(.5, 3.2, 3.4)
for(i in 1:3){
	lambda$Dd=lams[i]*lamEe
for(t in 2:numYears){
	TMP= Repro(NDd[t-1], NDe[t-1], NEd[t-1], NEe[t-1], L=lambda)
	Ntotd[t]=TMP[1]
	Ntote[t]=TMP[2]
	
	#dispersers produced?
	TMP2 = Prod(NDd[t-1], NDe[t-1], NEd[t-1], NEe[t-1], L=lambda)
	NDeggs[t] =TMP2[1]
	NEeggs[t] =TMP2[2]		
	Ntot[t]=Ntotd[t]+Ntote[t] #total number of individuals of any type

	
	Ntot[t]=Ntotd[t]+Ntote[t] #total number of individuals of any type
	OUT = Disp(TMP[1],TMP[2],K=size)
	NDd[t]=OUT[1]
	NDe[t]=OUT[2]
	NEd[t]=OUT[3]
	NEe[t]=OUT[4]
}
	etaDE = NDeggs/NEeggs*1/(size$D/size$E)
	etaDE = etaDE[2:length(etaDE)]
	lines(2:numYears,etaDE, lty=i, lwd=2)
}
legend(10,6, c(expression(lambda[Ee]/lambda[Dd]== 0.5),expression(lambda[Ee]/lambda[Dd]== 3.2), expression(lambda[Ee]/lambda[Dd]== 3.4)) , lty=1:3, lwd=2, cex=.8, bty='n')

abline(h=1, lty=1,col='gray')
abline(h=0,lty=1)
