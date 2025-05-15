#Functions to create a lavaan model syntax for PIM (Pseudo-Indicator Model)
#TODO: Fix the code construction so that message2 is not necessary
#TODO: ADD VARIANCES OR MARKER APPROACH FOR EXOGENEOUS LATENT VARS


#grab names of "latent" variables or components (i.e., not in dataset) from the model
composites<-function (C){
  component_names <- colnames(C) #observed variables' names
  model_names <-rownames(C) #composite model variable names
  composites <-setdiff(model_names, component_names) #only var names that aren't in data
  common_elements <- intersect(model_names, component_names) #model var names that are also in the data

  #Commented out; I am not sure we need this message anymore given automatic
  #correlations among all exogenous vars regardless of type
  # if (length(common_elements) > 0) {
  #   message2 <- paste("Note: The following variables are treated as observed variables:",
  #                     paste(common_elements, collapse = ", "),
  #                     "\nThe generated PIM syntax may need to be manually modified to allow",
  #                     "\ntheir correlation with other exogeneous latent variables that represent composites")
  #   message(message2)
  # }
  return(composites)
 }


# creates the first part of the PIM model syntax
# so this still created a latent variable with name BEH, not good
# need to select rows of C that are not excluded by composites function
# the syntax below will accommodate single component composites as well
# (i.e., if the user for some reason wants to rename an observed variable)
# TODO: Eventually, allow user to specify which indicator is the pseudo-indicator
#       for each composite
# TODO: Permit average and not just sum scores

PIM.uni<-function (C) {
  PIM.uni<-NULL
  cnames <- composites(C) #model var names that aren't in the data
  C1 <- C[rownames(C) %in% cnames, ] #submatrix with rows that will be set up as "latent" composites
  for (j in 1:length(cnames)){
    cnamesj <- colnames(C1)[C1[j, ] == 1]
    compj <- rownames(C1)[j]
    if(length(cnamesj) !=1){
    allbut1 <- paste(sprintf("(-1)*%s", cnamesj[-1]), collapse = " + ")
    PIMj <- sprintf("%s =~ 1*%s\n  %s ~ %s\n   %s ~~ 0*%s \n %s ~ 1  \n %s ~ 0*1",
            compj, cnamesj[1], cnamesj[1], allbut1, cnamesj[1], cnamesj[1],
            compj, cnamesj[1])
    }  else {
    PIMj <- sprintf("%s =~ 1*%s\n  %s ~~ 0*%s \n %s ~ 1  \n %s ~ 0*1",
                      compj, cnamesj[1], cnamesj[1], cnamesj[1],
                      compj, cnamesj[1])

      }
    compj_var <- paste(compj,"~~",compj) #composite variance (for "lavaan")

    PIM.uni<-paste(c(PIM.uni,PIMj,compj_var), collapse="\n")
    }

  return(PIM.uni)
}

#additional syntax needed to fit the PIM model using lavaan rather than sem command
#explicit (residual) variances and intercepts
PIM.uni.lav<-function (C) {
  PIM.uni.lav<-NULL
  varnames <- rownames(C) #model var names, broader than cnames
  #here, we do this for all rows of C, including obv variables that will be treated as such
  #C1 <- C[rownames(C) %in% cnames, ] #submatrix with rows that will be set up as "latent" composites
  for (j in 1:length(varnames)){
    varnamesj <- colnames(C)[C[j, ] == 1]
    compj <- rownames(C)[j]

    if (length(varnamesj) == 1) {varnamesjj <- varnamesj} else {
      varnamesjj <- varnamesj[-1]
    }

    # freely estimated variance for each indicator but the first
    varsj <- paste(paste(varnamesjj, "~~", varnamesjj), collapse = "\n")
    # freely estimated intercept for each indicator but the first
    intsj <- paste(paste(varnamesjj, "~", 1), collapse = " \n ")

    PIM.uni.lav<-paste(c(PIM.uni.lav,varsj,intsj),collapse="\n")
  }
  return(PIM.uni.lav)
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
  PIM.multi2 <- paste(int1, collapse = "\n")

  PIM.multi <- paste(c(PIM.multi1,PIM.multi2), collapse = "\n")
  return(PIM.multi)

}

## explicitly correlates exogenous variables in a model for composites
## also useful for the baseline model for composites
## We need to add corelations among factors. Problem: here, we do not know which ones
## are factors and which ones are composites.
## In the PIM.uni.lavaan, we already free variances of composites, so this will
## double up the syntax if appied indiscriminantly. In the PIM.uni.lavaan, however, we
## do not have info about the model and names of true latent vars. Maybe combine these
## two functions.
## Problem with message4: if user ends orthogonal covs to compmodel, it will still
## get printed. However, that message is no longer accurate, because user specification
## will override this duplicate specification. (verified). We need a better unified way to set up the entire PIM.
comp_exog_covs_syntax<-function (compmodel) {
  partable<-lavaanify(compmodel)

  # Find endogenous vars (values in lhs with ~ in op)
  end_names1 <- unique(partable$lhs[partable$op == "~"]) # do not need to worry about 1
  # Find more endogenous vars (values in rhs with =~ in op), indicators of factors
  end_names2 <- unique(partable$rhs[partable$op == "=~"])

  # Endogenous appear in lhs(to avoid 1?) but not those in end_names1 or end_names2
  # What about those that via fixed.x=TRUE would never appear in lhs, but only rhs?
  # Do we just ignore fixed.x=TRUE because of missing data?
  # "" excludes empty rhs, which corresponds to mean structure parameters
  # why isn't 1 stored in rhs? i don't know.
  exog_names <- unique(partable$rhs[!(partable$rhs %in% c(end_names1, end_names2,""))])

  # Generate combinations of all pairs
  pairs <- utils::combn(exog_names, 2, simplify = FALSE)
  # Format pairs into the desired string
  fpairs <- sapply(pairs, function(x) paste(x[1], "~~", x[2]))

  # Collapse into a single string separated by newlines
  exog_covs <- paste(fpairs, collapse = "\n")
  message4 <- paste0("Note: The following exogeneous variables in the composites model will be  correlated by default",
              "\n(unless overridden in the composite model syntax): ",paste(exog_names, collapse = ", "),". \nIf you do not want this, modify the composite model syntax manually or set exog_cov=FALSE.")
  message(message4)
  return(exog_covs)
}



#' Creates lavaan syntax for a Pseudo-Indicator Model
#'
#' Creates lavaan syntax for a Pseudo-Indicator Model (PIM) of Rose et al.
#' (2019).
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are
#'   components
#' @param compmodel A string with the lavaan model for composites
#' @param exog_cov Should exogenous variables in the composite model be correlated?
#' @returns A string with the full PIM model syntax
#' @export
#'
#' @details The names of the composite variables in `compmodel` should match the
#'  row names of the `C` matrix. This function does not check for this, as it
#'  just creates a text object and does not evaluate if `lavaan` can run it.
#'
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
#' @references Rose, N., Wagner, W., Mayer, A., & Nagengast, B. (2019).
#' Model-based manifest and latent composite scores in structural equation
#' models. Collabra: Psychology, 5(1), Article 9.
#' https://doi.org/10.1525/collabra.143
#'

PIM_syntax<-function (C,compmodel,exog_cov=TRUE) {
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMulav <- PIM.uni.lav(C)

  if(exog_cov){
    comp_exog_covs_syntax <- comp_exog_covs_syntax(compmodel)

  } else{comp_exog_covs_syntax <- NULL}


  out <- paste(c(PIMu, PIMm,PIMulav,compmodel,comp_exog_covs_syntax), collapse = "\n")
  return(out)
}



#' Baseline Model Syntax for the Composites Model
#'
#' @param compmodel Composite model syntax for which to create a baseline model
#' @param exog_cov Should exogenous variables in the compmodel be correlated in the baseline model? If FALSE, all variables in the baseline model will be orthogonal.
#'
#' @returns Baseline model lavaan syntax
#' @export
#'
#' @examples
#' compmodel<-"C1 ~ C2 + C3"
#' basemodel <- compmodel_base(compmodel) #default is to covary C2 and C3
#' basemodel1 <- compmodel_base(compmodel,exog_cov=FALSE) #all three composites are orthogonal
#'
compmodel_base<-function (compmodel,exog_cov=TRUE) {
  comp_names <- lavNames(compmodel)
  # Create the text object
  part1 <- paste(sapply(comp_names, function(x) paste(x, "~~", x)),
                 collapse = " \n ") #all vars
  part2 <- paste(sapply(comp_names, function(x) paste(x, "~", 1)),
                 collapse = " \n ") #all means

  if(exog_cov){
    part3 <- comp_exog_covs_syntax(compmodel)

    } else{part3 <- NULL}
  compmodel_base <- paste(c(part1, part2,part3), collapse = "\n")
  return(compmodel_base)
}

#NOTE: I think this function should take the regular composites model as input
#Not the modified (already baseline) model, similar to the sat version
#note: exog_cov argument is not used because whether exog covs are included or not
#will be asked when compmodel_base syntax is created
#
#' Baseline Model Syntax for the PIM
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are
#'   components
#' @param compmodel_base A string with lavaan baseline (null) model for composites
#'
#' @returns A string with the full PIM baseline model lavaan syntax
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
#' basemodel <- compmodel_base(compmodel)
#' PIM_model_base <- PIM_syntax_base(C=C,compmodel=basemodel)
#'
PIM_syntax_base<-function (C,compmodel_base) {
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMulav <- PIM.uni.lav(C)

  out <- paste(c(PIMu, PIMm,PIMulav,compmodel_base), collapse = "\n")
  return(out)
}



#' H1 (Saturated) Model Syntax for the Composites Model
#'
#' @param compmodel Composite model syntax for which to create a baseline model

#' @returns H1 (Saturated) model lavaan syntax
#' @export
#'
#' @examples
#' compmodel<-"C1 ~ C2 + C3"
#' satmodel <- compmodel_sat(compmodel)
#'
compmodel_sat<-function (compmodel) {
  comp_names <- lavNames(compmodel)
  # Create the text object
  part1 <- paste(sapply(comp_names, function(x) paste(x, "~~", x)),
                 collapse = " \n ") #all vars
  part2 <- paste(sapply(comp_names, function(x) paste(x, "~", 1)),
                 collapse = " \n ") #all means
  pairs <- utils::combn(comp_names, 2, FUN = function(x) paste(x, collapse = " ~~ "))
  part3 <- paste(pairs, collapse = " \n ") #all covs

  compmodel_sat <- paste(c(part1, part2,part3), collapse = "\n")
  return(compmodel_sat)
}


#fit saturated model for composites
#rewrite this whole function file so that all PIM functions
#are only called once, and all three models are created?
#' Title
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are
#'   components
#' @param compmodel Composite model lavaan syntax
#'
#' @returns A string with the full PIM baseline model lavaan syntax
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
#' PIM_model_sat <- PIM_syntax_sat(C=C,compmodel=compmodel)
#'
PIM_syntax_sat<-function (C,compmodel) {
  compmodel_sat <- compmodel_sat(compmodel)
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMulav <- PIM.uni.lav(C)

  out <- paste(c(PIMu, PIMm,PIMulav,compmodel_sat), collapse = "\n")
  return(out)
}




