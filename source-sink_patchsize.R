numYears=5

###################

#Parameters:
KE=1000 #in numbers of individuals
lambdaEe=1.5
lambdaEd=.7
lambdaDe=.9
lambdaDd=4.93

#For initializing: give intial ratio of e to d, plus the number of individuals in ND and NE
initdToTot=.1          #initial d to total ratio



########################
#########################
#Actual code:
count=1
bins=1000
deltad=numeric(bins)
vectorKD=seq(.01*KE,10*KE,length.out=bins)
ratioK=vectorKD/KE
for(count in 1:bins){
KD=vectorKD[count]
cat(KD)
initTotPop=KE+KD    #total size of combined populations

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
if(initdToTot>.5){ #d is more than half the pop, D is the source, E is the sink
		#cat("d is more common, D is the source")
		NDd[1]=(Ntotd[1]/Ntot[1])*KD
		NDe[1]=(Ntote[1]/Ntot[1])*KD
		NEd[1]=(Ntotd[1]/Ntot[1])*min(KE,(Ntotd[1]+Ntote[1]-KD))
		NEe[1]=(Ntote[1]/Ntot[1])*min(KE,(Ntotd[1]+Ntote[1]-KD))
	}else{ #e is more than half the pop, E is the source
		#cat("e is more common, E is the source")
		NEd[1]=(Ntotd[1]/Ntot[1])*KE
		NEe[1]=(Ntote[1]/Ntot[1])*KE
		NDd[1]=(Ntotd[1]/Ntot[1])*min(KD,(Ntotd[1]+Ntote[1]-KE))
		NDe[1]=(Ntote[1]/Ntot[1])*min(KD,(Ntotd[1]+Ntote[1]-KE))
	}
#############
#first year is 


for(t in 2:numYears){
	Ntotd[t]=NDd[t-1]*lambdaDd+NEd[t-1]*lambdaEd #finding the total number of d individuals produced this year
	#cat(Ntotd[t])
	Ntote[t]=NEe[t-1]*lambdaEe+NDe[t-1]*lambdaDe #ditto for e individuals
	#cat(Ntote[t])
	Ntot[t]=Ntotd[t]+Ntote[t] #total number of individuals of any type
	#cat(Ntotd[t])
	#cat(" ")
	#cat(Ntot[t])
	#cat("   ")
	if((Ntotd[t]/Ntot[t])>.5){ #d is more than half the pop, D is the source, E is the sink
		#cat("d is more common, D is the source")
		NDd[t]=(Ntotd[t]/Ntot[t])*KD
		NDe[t]=(Ntote[t]/Ntot[t])*KD
		NEd[t]=(Ntotd[t]/Ntot[t])*min(KE,(Ntotd[t]+Ntote[t]-KD))
		NEe[t]=(Ntote[t]/Ntot[t])*min(KE,(Ntotd[t]+Ntote[t]-KD))
	}else{ #e is more than half the pop, E is the source
		#cat("e is more common, E is the source")
		NEd[t]=(Ntotd[t]/Ntot[t])*KE
		NEe[t]=(Ntote[t]/Ntot[t])*KE
		NDd[t]=(Ntotd[t]/Ntot[t])*min(KD,(Ntotd[t]+Ntote[t]-KE))
		NDe[t]=(Ntote[t]/Ntot[t])*min(KD,(Ntotd[t]+Ntote[t]-KE))
	}
}
deltad[count]=(Ntotd[numYears]-Ntotd[numYears-1])/Ntot[numYears]
}
plot(ratioK,type="line",deltad,main="change in proportion of d",xlab="ratio of KD:KE",ylab="Deltad")
