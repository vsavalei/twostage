#PIM functions Jan 2025
#Rose et al PIM (Pseudo-Indicator Model)#

#functions:
# PIM.uni(C) -- sets up syntax pertaining to each composite
# PIM.multi(C) -- sets of syntax for covariances of components with each other and of composites with components
# PIM(C,compmodel) -- runs both functions and creates the PIM syntax, adding the model for composites (compmodel)

PIM.uni<-function (C) {
  PIM.uni<-NULL
  cnames <-rownames(C)
  for (j in 1:length(cnames)){
    cnamesj <- colnames(C)[C[j, ] == 1]
    if(length(cnamesj)==1){next} #skip composites with one component
    compj <- rownames(C)[j]
    allbut1 <- paste(sprintf("(-1)*%s", cnamesj[-1]), collapse = " + ")
    PIMj <- sprintf("%s =~ 1*%s\n  %s ~ %s\n   %s ~~ 0*%s \n %s ~ 1  \n %s ~ 0*1",
                    compj, cnamesj[1], cnamesj[1], allbut1, cnamesj[1], cnamesj[1], compj, cnamesj[1] )
    PIM.uni<-paste(PIM.uni,PIMj,sep='\n')
  }
  return(PIM.uni)
}


PIM.multi<-function (C) {
  allbut1<-NULL #a vector
  allbut1.string<-NULL #sum of all components listed in allbut1
  cnames <-rownames(C)
  for (j in 1:length(cnames)){ #creates a vector of all components (but 1st in each)
    cnamesj <- colnames(C)[C[j, ] == 1]

    if(length(cnamesj)==1){next} #skip composites with one component
    allbut1j.strong <- paste(sprintf("%s", cnamesj[-1]), collapse = " + ")
    allbut1j <- cnamesj[-1]
    allbut1<-c(allbut1,allbut1j)
  }

  combs <- combn(allbut1, 2, simplify = FALSE) #combs of 2 at a time
  texts <- sapply(combs, function(x) paste(x, collapse = " ~~ "))
  PIM.multi1 <- paste(texts, collapse = "\n")

  #create a text object with covs among allbut1 elements and each composite:
  right_side <- paste(allbut1, collapse = "+")
  int1 <- paste(cnames, right_side, sep = " ~~ ")
  PIM.multi2 <- paste(int1, collapse = " \n ")

  PIM.multi <- paste(PIM.multi1,PIM.multi2, sep = " \n")
  return(PIM.multi)

}

PIM<-function (C,compmodel) {
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMt <- paste(PIMu,PIMm, compmodel, sep = "\n")
  return(PIMt)
}


