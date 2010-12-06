#  Version 0.1 -- Collin Edwards
#  Version 0.1 -- Jaime Ashander -- Refactoring for code reuse




###################



Repro<-function(NDdIN, NDeIN, NEdIN, NEeIN, L=lambda){
	# given nubmer of each genotype in each environment, find totals of each geno produced across all
	NtotdOUT=NDdIN*lambda$Dd+NEdIN*lambda$Ed #finding the total number of d individuals produced this year
	NtoteOUT=NEeIN*lambda$Ee+NDeIN*lambda$De #ditto for e individuals
	return(c(NtotdOUT, NtoteOUT))
	}

Disp<-function(NtotdIN, NtoteIN, K=size){
	# given total number of offspring of each genotype, define dispersal
	NtotIN	= NtotdIN + NtoteIN
	if((NtotdIN/NtotIN)>.5){ #d is more than half the pop, D is the source, E is the sink
		#cat("d is more common, D is the source")
		NDdOUT=(NtotdIN/NtotIN)*K$D
		NDeOUT=(NtoteIN/NtotIN)*K$D
		NEdOUT=(NtotdIN/NtotIN)*min(K$E,(NtotdIN+NtoteIN-K$D))
		NEeOUT=(NtoteIN/NtotIN)*min(K$E,(NtotdIN+NtoteIN-K$D))
	}else{ #e is more than half the pop, E is the source
		#cat("e is more common, E is the source")
		NEdOUT=(NtotdIN/NtotIN)*K$E
		NEeOUT=(NtoteIN/NtotIN)*K$E
		NDdOUT=(NtotdIN/NtotIN)*min(K$D,(NtotdIN+NtoteIN-K$E))
		NDeOUT=(NtoteIN/NtotIN)*min(K$D,(NtotdIN+NtoteIN-K$E))
	}
	return(c(NDdOUT, NDeOUT, NEdOUT, NEeOUT))	
}