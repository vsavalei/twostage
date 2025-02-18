#Functions to create a lavaan model syntax for PIM (Pseudo-Indicator Model)

#grabs names of fictitious variables (not in dataset) from the model
composites<-function (C){
  component_names <- colnames(C) #observed variables' names
  model_names <-rownames(C) #composite model variable names
  composites <-setdiff(model_names, component_names) #only var names that aren't in data

  common_elements <- intersect(model_names, component_names)
  if (length(common_elements) > 0) {
    message2 <- paste("Note: The following variables are treated as observed variables:",
                      paste(common_elements, collapse = ", "),
                      "\nThe generated PIM syntax may need to be manually modified to allow",
                      "\ntheir correlation with other exogeneous variables that are composites,",
                      "\nwhich are set up as latent variables in PIM. See README.md for more detail.")
    message(message2)
  }
  return(composites)
 }


# creates the first part of the PIM model syntax
PIM.uni<-function (C) {
  PIM.uni<-NULL
  cnames <- composites(C) #only var names that aren't in data
  for (j in 1:length(cnames)){
    cnamesj <- colnames(C)[C[j, ] == 1]
    compj <- rownames(C)[j]
    if(length(cnamesj)==1){
      message1<-paste0("Note: The composite named ",compj, " has only one component: ",
                      cnamesj,". \nKeep this in mind when interpreting the results!",
                      " See README.md for alternatives.")
      message(message1)
      #special syntax for composites with one component
      PIMj <- sprintf("%s =~ 1*%s\n  %s ~~ 0*%s \n %s ~ 1  \n %s ~ 0*1",
                      compj, cnamesj[1], cnamesj[1], cnamesj[1], compj, cnamesj[1] )

      } else {
        allbut1 <- paste(sprintf("(-1)*%s", cnamesj[-1]), collapse = " + ")
        PIMj <- sprintf("%s =~ 1*%s\n  %s ~ %s\n   %s ~~ 0*%s \n %s ~ 1  \n %s ~ 0*1",
                    compj, cnamesj[1], cnamesj[1], allbut1, cnamesj[1], cnamesj[1], compj, cnamesj[1] )
      }
    PIM.uni<-paste(PIM.uni,PIMj,sep='\n')
  }
  return(PIM.uni)
}


#creates the second part of the PIM model syntax
PIM.multi<-function (C) {
  allbut1<-NULL #a vector
  allbut1.string<-NULL #sum of all components listed in allbut1
  cnames <-rownames(C)
  for (j in 1:length(cnames)){ #creates a vector of all components (but 1st in each)
    cnamesj <- colnames(C)[C[j, ] == 1]
    #if(length(cnamesj)==1){next} #used to skip composites with one component
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

#' Creates the lavaan syntax for a PIM (Pseudo-Indicator Model)
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
#' PIM_syntax(C,compmodel)
#'
#' @references
#' Rose, N., Wagner, W., Mayer, A., & Nagengast, B. (2019). Model-based manifest and latent composite scores in structural equation models. Collabra: Psychology, 5(1), Article 9. https://doi.org/10.1525/collabra.143
#'
PIM_syntax<-function (C,compmodel) {
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  out <- paste(PIMu,PIMm, compmodel, sep = "\n")
  return(out)
}


