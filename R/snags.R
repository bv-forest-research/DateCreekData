

#snag harvest
#lni <- grep(harv_text,rf2)
#lni_grs <- split(lni,cumsum(c(1, diff(lni) != 1)))
#lni_snag <- lni_grs$`3` #first harvest chunk
#StartCuts <- lni_snag[1]
#EndCuts <- c()
#for(ii in 1:(length(lni_snag)-1)){
#  if(lni_snag[ii+1]==(lni_snag[ii]+1)){ #use while
#    EndCuts <- lni_snag[ii+1]
#  }else{
#    EndCuts <- lni_snag[ii]
#  }
#}
#lnm <- c(StartCuts,EndCuts)
#x <- removeRow(lnm, rf2)
#bring in the gaps
#rf2 <- append(x,HarvLines,lnm[1]-1)
