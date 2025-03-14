
# TSML functions #################

#' @import lavaan

#namesohd (helper function): names rows/columns of the asy cov matrix from stage1
namesohd<- function (cnames) {
  name_grid_cov <- t(outer(cnames,cnames,function(x,y) paste0(x,"~~",y)))
  names_cov <- name_grid_cov[lower.tri(name_grid_cov, diag = TRUE)]
  names_mean <- paste0(cnames,"~",1)
  namesohd <- c(names_cov,names_mean)
  return(namesohd)
}

#write.sat (helper function): creates the saturated model syntax in lavaan
write.sat <- function(p, varnames) {
  if (length(varnames) != p) {
    stop("The length of 'varnames' must be equal to 'p'.")
  }

  sat.mod <- ""
  for (i in 1:p) {
    linestart <- paste(varnames[i], " ~~ ", varnames[i], sep = "")
    if (p - i > 0) {
      linemid <- ""
      for (j in (i + 1):p) {
        linemid <- paste(linemid, " + ", varnames[j], sep = "")
      }
    } else {
      linemid <- ""
    }
    sat.mod <- paste(sat.mod, linestart, linemid, " \n ", sep = "")
  }
  return(sat.mod)
}


#' stage0
#'
#' This is the prep stage of TSML (can also be used for PIM). This function uses
#' p column names of data and k composite variable names from the lavaan
#' model to create a k x p matrix C, with nonzero elements representing the assignment
#' of components to composites. Which elements are nonzero is determined by presenting
#' a menu of composite names to the user for each component. What the weights are is
#' determined by presenting the user with the menu to create composites as sum scores
#' or average scores.
#'
#' @param data A data file with components (items) to be assigned to composites.
#' It should not contain other variables (or else feed in only selected columns as data).
#' @param model A lavaan model for the composites. It is only used to extract composite names
#' via lavNames().
#'
#' @return A matrix C with rownames set to composite names, and columnnames set to
#' component (item) names. A non-zero value in each row corresponds to the assignment of that
#' component to that composite, with the non-zero value as the weight. Current options for weights
#' are 1 for unit-weighted and 1/pj for average-weighted, where pj is the number of components in that
#' composite.
#'
#' @export
#'
#' @examples
#'
#'\dontrun{ #The menu cannot be used non-interactively
#' # TPB Model for Composites (Full mediation)
#' tpbmod<-'
#' INTALL ~ ATTALL + PBCALL + NORMALL
#' BEH ~ INTALL'
#'
#' stage0(tpbdata, tpbmod)
#'
#'
#'# With appropriate selections, this should result in the message:
#' #Your composites are made up of the following components:
#' #INTALL :  INT1 INT2 INT3
#' # BEH :  BEH
#' #ATTALL :  AT1CPU AT2CPU AT3CPU AT4CPU AT5CPU AT6CPU AT7CPP AT8CPP AT9CPP AT10CPP AT11CPP
#' #PBCALL :  PBC1 PBC2 PBC3
#' #NORMALL :  NORS1 NORS2 NORS3
#' #If this is not correct, start over!
#'}
#'
stage0<-function (data, model) {
  cnames<-lavNames(model)
  C <- matrix(0,nrow=length(cnames),ncol=length(colnames(data)))
  colnames(C)<-colnames(data) #component names
  rownames(C)<-cnames #composite names
  prompt_type <- paste("Are your composites sums or averages of components?")
  type <- utils::menu(c("Sums","Averages"), title = prompt_type)

  for (i in 1:length(colnames(data))){
    prompt_message <- paste("Please select the composite for variable", colnames(data)[i], ":")
    ind_i <- utils::menu(cnames, title = prompt_message)
    C[ind_i,i]<-1
  }

  cat("Your composites are made up of the following components: \n")
  for (j in 1:length(cnames)){
    cnamesj <- colnames(C)[C[j, ] == 1]
    cat(cnames[j], ": ", cnamesj, "\n")
  }
  cat("If this is not correct, start over! \n")

  if(type=="2"){  #rescale unit weights
    C<-C/rowSums(C)
    }

  return(C)
}


#' stage1
#'
#' Stage1 of TSML: fits a saturated model to the items with missing data
#'
#' @param data A datafile with components (items).
#' It should not contain any other variables.
#'
#' @param runcommand Additional arguments to pass to lavaan
#'
#' @return  A list with 3 objects: mhb (estimated means),
#' shb (estimated covariance matrix), ohb (estimated asymptotic covariance matrix
#' of mhb and shb elements)
#'
#' @export
#'
#' @examples
#'
#'out_s1<-stage1(misdata_mcar20)
#'
#'#as tpbdata has no missing data, running stage1 with expected information
#'#will result in TSML matching regular ML
#'out_s1<-stage1(tpbdata, runcommand="information='expected'")
#'
#'
stage1 <- function (data,runcommand=NULL) {
  p <- ncol(data)
  N <- nrow(data)
  pst <- p*(p+1)/2

  #fit the saturated model in lavaan
  S1.mod <- write.sat(p,varnames=colnames(data))

  S1 <- try(eval(parse(text = paste("sem(S1.mod, data = data, missing = 'ml', ", runcommand, ")"))),silent=TRUE)
  #fixed the line below, need to test
  if(!inherits(S1, "try-error")) {
    if(inspect(S1, "converged") == TRUE && is.null(vcov(S1)) == FALSE){
      shb <- fitted.values(S1)$cov    			#sigma-hat-beta
      mhb <- fitted.values(S1)$mean         #mu-hat-beta
      ohb <- vcov(S1)  #asy cov matrix
      S1.output <- list(shb,mhb,ohb)
    } else {S1.output <- NULL } #end if converged loop
  } else {S1.output <- NULL } #end try-error loop
  return(S1.output)
}

#' stage1a
#'
#' Stage1a of TSML: Converts the means, covariance matrix, and asymptotic covariance matrix
#' of components to the corresponding quantities for the composites
#'
#' @param S1.output The output of stage1, a list with three objects (means, covariance matrix,
#' and asymptotic covariance matrix of components)
#' @param C The matrix of component-composite assignments that is the output of stage0
#' (or user-supplied)
#'
#' @return  A list with 3 objects: mhd (estimated means),
#' shd (estimated covariance matrix), ohd (estimated asymptotic covariance matrix
#' of mhb and shb elements) for the composites
#'
#' @export
#'
#' @examples
#'
#' #an example using the first 18 variables in the simulated dataset misdata_mcar20
#' #reduce model size
#'
#'library(lavaan)
#' misdata1<-misdata_mcar20[,1:18]
#'
#' # composite sub-model
#' mod1 <- '
#'  F1 =~ C1 + C2 + C3
#'  F2 =~ C4 + C5 + C6
#'  F2 ~ F1
#'  F2 ~~ F2
#'  F1 ~~ F1'

#' #manual computation for C (stage0) to avoid user input
#' cnames<-lavNames(mod1) #this is a lavaan function
#' C <- matrix(0,nrow=length(cnames),ncol=length(colnames(misdata1)))
#' colnames(C)<-colnames(misdata1)
#' rownames(C)<-cnames
#' C[1,1:3]<-1
#' C[2,4:6]<-1
#' C[3,7:9]<-1
#' C[4,10:12]<-1
#' C[5,13:15]<-1
#' C[6,16:18]<-1
#'
#'out_s1<-stage1(misdata1)
#'out_s1a<-stage1a(out_s1,C)
#'
stage1a <- function (S1.output, C) {
  if (is.null(S1.output)) {S1a.output <- NULL} else {
    shb <- S1.output[[1]]
    mhb <- S1.output[[2]]
    ohb <- S1.output[[3]]

    cnames<-rownames(C)
    k <- nrow(C) #number of composites
    p <-ncol(C) #number of components
    pst <- p*(p+1)/2
    kst <- k*(k+1)/2

    #Cbig, covs in original order, the means
    Cb <- matrix(0, (kst + k), (pst + p))
    Cb[(kst+1):(kst+k),(pst+1):(pst+p)] <- C
    Cbpart1<-lav_matrix_duplication_ginv_pre(C%x%C) #new line 1
    Cb[1:kst,1:pst] <-  lav_matrix_duplication_post(Cbpart1) #new line 2

    #saturated model parameters for the model based on the k composites:
    mhd <- C %*% mhb  			#mu-hat-delta
    rownames(mhd)<- cnames
    shd <- C %*% shb %*% t(C) 	#sigma-hat-delta
    rownames(shd)<-colnames(shd)<- cnames
    #dh <- c(lav_matrix_vech(shd),mhd) 		  #delta-hat
    ohd <- Cb %*% ohb %*% t(Cb) #ohm-hat-delta
    rownames(ohd)<-colnames(ohd) <- namesohd(cnames)
    #order in ohd: vectorized non-red Sigma, then mu
  } #end of big non-null else
  S1a.output <- list(shd, mhd,ohd)
  return(S1a.output)
}

#' stage2
#'
#' Performs stage2 of TSML. Fits the model to the estimated vector of means and the estimated
#' covariance matrix for the composites using complete data routines, adjusts naive
#' standard errors to obtain robust TS standard errors (for normal data), computes
#' the residual-based test statistic (for normal data)
#'
#' @param S1a.output Output from stage1a, a list with three objects (shd, mhd, ohd)
#' @param N Sample size
#' @param model The lavaan model for the composites
#' @param runcommand2 Additional arguments to pass to lavaan
#'
#' @returns For now, a list with four components:
#'
#' TS_Run_naive (the lavaan object for the naive (complete data) model fit for the composites).
#' Parameter estimates from this run are TSML estimates.
#'
#' TS_SEs (the TSML standard errors, computed by adjusting the naive standard errors
#' for the two-stage nature of the estimation, assuming normality)
#'
#' T_res (the residual based test statistic) and T_res_pvalue (the p-value for the residual based test statistic)#' T_res_pvalue: the p-value for the residual based test statistic
#'
#'This output will be re-arranged into something better eventually. Correct fit indices
#'will also be added.
#'
#' @export
#'
#' @examples
#'
#' #an example using the first 18 variables in the simulated dataset misdata_mcar20
#' #reduce model size
#' library(lavaan)
#' misdata1<-misdata_mcar20[,1:18]
#'
#' # composite sub-model
#' mod1 <- '
#'  F1 =~ C1 + C2 + C3
#'  F2 =~ C4 + C5 + C6
#'  F2 ~ F1
#'  F2 ~~ F2
#'  F1 ~~ F1'

#' #manual computation for C (stage0) to avoid user input
#' cnames<-lavNames(mod1) #this is a lavaan function
#' C <- matrix(0,nrow=length(cnames),ncol=length(colnames(misdata1)))
#' colnames(C)<-colnames(misdata1)
#' rownames(C)<-cnames
#' C[1,1:3]<-1
#' C[2,4:6]<-1
#' C[3,7:9]<-1
#' C[4,10:12]<-1
#' C[5,13:15]<-1
#' C[6,16:18]<-1
#'
#'out_s1 <- stage1(misdata1)
#'out_s1a <- stage1a(out_s1,C)
#'out_s2 <- stage2(out_s1a, N = nrow(misdata1), model = mod1,runcommand2="mimic='EQS'")
#'
#'
stage2 <- function (S1a.output, N, model,runcommand2=NULL) {
  shd <- S1a.output[[1]]
  mhd <- S1a.output[[2]]
  ohd <- S1a.output[[3]]

  S2 <- try(eval(parse(text = paste("sem(model, sample.cov = shd, sample.mean = mhd,
                sample.nobs = N, meanstructure=TRUE, ", runcommand2, ")"))),silent=TRUE)
  #find a sane and consistent way to do the line below
  if(inherits(try(vcov(S2)),"try-error")==FALSE){

    ddh <- lavInspect(S2, "delta") #model derivatives
    bread <- lavInspect(S2, "vcov")*N
    Hh <-   lavInspect(S2, "h1.information.expected")
    meat <- Hh%*%ddh

    #reordering
    new_order <- match(rownames(meat), rownames(ohd))
    ohd.reordered <- ohd[new_order,new_order]

    Ohtt <- bread %*% t(meat)%*%ohd.reordered%*%meat%*%bread  #ohm-hat-theta-tilde
    TS_SE <- sqrt(diag(Ohtt))

    et <- c(residuals(S2)$mean,lav_matrix_vech(residuals(S2)$cov)) #so swap
    ahd <- solve(ohd.reordered)/N
    Tres <- (N-1)*t(et) %*% (ahd - (ahd%*%ddh) %*% solve(t(ddh)%*%ahd%*%ddh) %*% (t(ddh)%*%ahd)) %*% et
    pval <- 1 - stats::pchisq(Tres, df = inspect(S2, "fit")["df"])

    result <- list(S2, TS_SE, Tres, pval)
    names(result) <- c("TS_Run_naive", "TS_SEs", "T_res", "T_res_pvalue")

    } #end if SEs were computed
  else { #if lavaan can't compute SEs, we won't try either)
    result <- list(S2, NA, NA, NA)
  }

  return(result)

}

#' twostage
#'
#' Fits the model to composite based on the raw data on components using item-level two-stage
#' maximum likelihood (TSML) of Savalei &amp Rhemtulla (2017). Adjusts naive
#' standard errors to obtain robust TS standard errors (for normal data), computes
#' the residual-based test statistic (for normal data).
#'
#  This function runs stage0, stage1, stage1a, stage2 in sequence.

#' @param data Data file for the components (items)
#' @param model The lavaan model for the composites
#' @param C The matrix of component-composite assignments and weights (if NULL, user input will be requested to construct it)
#' @param runcommand Additional arguments to pass to lavaan for stage1
#' @param runcommand2 Additional arguments to pass to lavaan for stage2
#'
#' @return For now, a list with four components:
#'
#' TS_Run_naive (the lavaan object for the naive (complete data) model fit for the composites).
#' Parameter estimates from this run are TSML estimates.
#'
#' TS_SEs (the TSML standard errors, computed by adjusting the naive standard errors
#' for the two-stage nature of the estimation, assuming normality)
#'
#' T_res (the residual based test statistic) and T_res_pvalue (the p-value for the residual based test statistic)
#'
#'This output will be re-arranged into something better eventually. Correct fit indices
#'will also be added.
#'
#' @export
#'
#' @examples
#'
#' #Example 1: An example using the first 18 variables in the simulated
#' #dataset misdata_mcar20 with 20% missing data on about half the variables
#' #C1 - C6 are parcels formed using three variables each (in order)
#'
#' library(lavaan)
#' misdata1<-misdata_mcar20[,1:18]
#'
#' # composite sub-model
#' mod1 <- '
#'  F1 =~ C1 + C2 + C3
#'  F2 =~ C4 + C5 + C6
#'  F2 ~ F1
#'  F2 ~~ F2
#'  F1 ~~ F1'

#' #manual computation for C to avoid user input
#' cnames<-lavNames(mod1) #this is a lavaan function
#' C <- matrix(0,nrow=length(cnames),ncol=length(colnames(misdata1)))
#' colnames(C)<-colnames(misdata1)
#' rownames(C)<-cnames
#' C[1,1:3]<-1
#' C[2,4:6]<-1
#' C[3,7:9]<-1
#' C[4,10:12]<-1
#' C[5,13:15]<-1
#' C[6,16:18]<-1
#'
#' out_ts <- twostage(data = misdata1, model = mod1,C = C)
#'
#'
#' #Example2: TSML on a complete dataset tpbdata, with lavaan options set to match complete data ML
#' #TPB Model for Composites (Full mediation)
#' tpbmod<-'
#' INTALL ~ ATTALL + PBCALL + NORMALL
#' BEH ~ INTALL'
#'
#' #manual definition of C (instead of stage0 to avoid user input)
#' cnames<-lavNames(tpbmod)
#' C <- matrix(0,nrow=length(cnames),ncol=length(colnames(tpbdata)))
#' colnames(C)<-colnames(tpbdata)
#' rownames(C)<-cnames
#' C[1,c("INT1","INT2","INT3")]<-1
#' C[2,c("BEH")]<-1
#' C[3,grep("AT", names(tpbdata))]<-1
#' C[4,c("PBC1","PBC2","PBC3")]<-1
#' C[5,c("NORS1","NORS2","NORS3")]<-1
#'
#' out_ts <- twostage(data = tpbdata, model = tpbmod,C = C,
#' runcommand = "information='expected'", runcommand2 = "meanstructure=TRUE,
#' fixed.x=FALSE,sample.cov.rescale=FALSE")
#' #The naive and TS standard errors should be identical
#'
#'
#' @references
#'Savalei, V., & Rhemtulla, M. (2017). Normal theory two-stage ML estimator when data are missing at the item level. Journal of Educational and Behavioral Statistics, 42(1), 1-27. https://doi.org/10.3102/1076998617694880

twostage <- function (data,model,C = NULL, runcommand = NULL, runcommand2 = NULL) {
  N <- nrow(data)
  #stage 0, if needed: relating components to composite via user input
  if (is.null(C)) {
    # Define C using stage0 when C is NULL
    C <- stage0(data = data, model = model)
  }
  #stage 1: saturated model on components
  s1 <- stage1(data = data,runcommand=runcommand)
  #stage1a: saturated solution transformation to composites
  s1a <-stage1a(S1.output=s1,C)
  #stage2: model fit to composites
  s2 <- stage2(S1a.output=s1a, N = N,model = model,runcommand2=runcommand2)

  #assign a class to the results
  class(s2) <- "twostage"

  return(s2)
}


#' Summary method for twostage function
#'
#' @param object An object of class 'twostage'.
#' @param ... Additional arguments passed to the summary method (none so far)
#'
#' @export
summary.twostage <- function(object, ...) {

  #validate object structure
  if (!all(c("TS_Run_naive", "TS_SEs", "T_res", "T_res_pvalue") %in% names(object))) {
    stop("This does not appear to be an object generated by twostage()!")
  }

  #create a table of estimates and ses
  df<-fitmeasures(object$TS_Run_naive)["df"]
  est<-cbind(parameterestimates(object$TS_Run_naive,remove.nonfree=T)[,1:5])
  TS_table<-cbind(est,object$TS_SEs)
  names(TS_table)[names(TS_table) == "object$TS_SEs"] <- "TSML se"
  #create a sentence with test statistic results to print
  test.output<-paste("The residual-based TSML chi-square is",
                     round(object$T_res,3),"against",df, "degrees of freedom, with a p-value of",
                     round(object$T_res_pvalue,3))

  cat("Summary of Two-Stage Analysis \n")
  cat("----------------------------\n")
  cat("Two-stage parameter estimates, naive standard errors, and two-stage standard errors: \n")
  print(TS_table, quote=FALSE,row.names=FALSE)
  cat("----------------------------\n")
  cat(test.output, "\n")
}

