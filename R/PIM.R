#PIM functions Jan 2025
#Rose et al PIM (Pseudo-Indicator Model)#

#functions:
# PIM.multi(C) -- sets of syntax for covariances of components with each other and of composites with components
# PIM(C,compmodel) -- runs both functions and creates the PIM syntax, adding the model for composites (compmodel)

#' PIM.uni() function creates the first part of the PIM model syntax
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are components
#'          A 1 indicates that a component belongs to that composite
#'
#' @returns A string with the first part of the PIM model syntax
#' @export
#'
#' @examples
#' #C1 is the sum of Y1, Y2, and Y3
#' #C2 is the sum of Y4, Y5, and Y6
#' #C3 is Y7
#' C<-matrix(0,nrow=3,ncol=7)
#' C[1,1:3]<-1
#' C[2,4:6]<-1
#' C[3,7]<-1
#' rownames(C)<-c("C1","C2","C3")
#' colnames(C)<-c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
#' PIM.uni(C)
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


#' PIM.multi() function creates the second part of the PIM model syntax
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are components
#'
#' @returns A string with the second part of the PIM model syntax
#' @export
#'
#' @examples
#' #C1 is the sum of Y1, Y2, and Y3
#' #C2 is the sum of Y4, Y5, and Y6
#' #C3 is Y7
#' C<-matrix(0,nrow=3,ncol=7)
#' C[1,1:3]<-1
#' C[2,4:6]<-1
#' C[3,7]<-1
#' rownames(C)<-c("C1","C2","C3")
#' colnames(C)<-c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
#' PIM.multi(C)
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

  combs <- utils::combn(allbut1, 2, simplify = FALSE) #combs of 2 at a time
  texts <- sapply(combs, function(x) paste(x, collapse = " ~~ "))
  PIM.multi1 <- paste(texts, collapse = "\n")

  #create a text object with covs among allbut1 elements and each composite:
  right_side <- paste(allbut1, collapse = "+")
  int1 <- paste(cnames, right_side, sep = " ~~ ")
  PIM.multi2 <- paste(int1, collapse = " \n ")

  PIM.multi <- paste(PIM.multi1,PIM.multi2, sep = " \n")
  return(PIM.multi)

}


#' PIM() function creates the full lavaan PIM model syntax using three parts:
#' one created by PIM.uni(), one created by PIM.multi(), and the lavaan model for composites
#' the names of the composite variables should match row names of the C matrix
#' @param C A matrix of 0s and 1s, where rows are composites and columns are components
#' @param compmodel A string with the lavaan model for composites
#'
#' @returns A string with the full PIM model syntax
#' @export
#'
#' @examples
#' #C1 is the sum of Y1, Y2, and Y3
#' #C2 is the sum of Y4, Y5, and Y6
#' #C3 is Y7
#' C<-matrix(0,nrow=3,ncol=7)
#' C[1,1:3]<-1
#' C[2,4:6]<-1
#' C[3,7]<-1
#' rownames(C)<-c("C1","C2","C3")
#' colnames(C)<-c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
#' compmodel<-"C1 ~ C2 + C3"
#' PIM(C,compmodel)
#'
#' @references
#' Rose, N., Wagner, W., Mayer, A., & Nagengast, B. (2019). Model-based manifest and latent composite scores in structural equation models. Collabra: Psychology, 5(1), Article 9. https://doi.org/10.1525/collabra.143
#'
PIM<-function (C,compmodel) {
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMt <- paste(PIMu,PIMm, compmodel, sep = "\n")
  return(PIMt)
}


