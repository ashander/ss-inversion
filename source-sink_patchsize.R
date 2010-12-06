#  Version 0.1 -- Collin Edwards

#Okay, this one looks at changes in the population size of d.  
#Graph is of deltad in terms of the ratio of the size of the patch for D to the size for the patch for E.
#
#Plug in some numbers at the top, run the thing, look at the pretty graph.  When the curve is above 0, there will be a source-sink inversion.
#If someone remembers how to put a horizonal line on a graph, please put that at the end of the code (with the horizontal line at y=0).

#If you want to make the graph more focused on a smaller region, change the line
#vectorKD=seq(.01*KE,10*KE,length.out=bins)
#so that the region is smaller, ie:
#vectorKD=seq(.01*KE,4*KE,length.out=bins)
#
#for reference, 
#seq(a,b,length.out=c)
#produces a sequence of numbers from a to b, with a total of c numbers in the sequence.



source('source-sink.R')

numYears=5

###################

#Parameters:
size = list(E=1000,D=200)
lambda = list(Ee=1.5, Ed=.7, De=.9, Dd=4.93)

#For initializing: give intial ratio of e to d, plus the number of individuals in ND and NE
initdToTot=.1          #initial d to total ratio

########################
#########################
#Actual code:
count=1
bins=1000
deltad=numeric(bins)
vectorKD=seq(.01*size$E,100*size$E,length.out=bins)
ratioK=vectorKD/size$E
for(count in 1:bins){
	size$D=vectorKD[count]	
	#cat(KD)
	initTotPop=size$E+size$D    #total size of combined populations

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
		cat(TMP,'\n')
		Ntot[t]=Ntotd[t]+Ntote[t] #total number of individuals of any type
		OUT = Disp(TMP[1],TMP[2],K=size)
		NDd[t]=OUT[1]
		NDe[t]=OUT[2]
		NEd[t]=OUT[3]
		NEe[t]=OUT[4]
	}
	deltad[count]=(Ntotd[numYears]-Ntotd[numYears-1])/Ntot[numYears]
}
plot(ratioK,type='l',deltad,main="change in proportion of d",xlab="ratio of KD:KE",ylab="Deltad")
abline(h=0)
