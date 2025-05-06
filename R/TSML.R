
# TSML functions #################

#' @import lavaan
#' @importFrom methods as
#' @importFrom stats pnorm
#' @importFrom methods show
#'
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
#' @return  A list with 4 objects: N (sample size), mhb (estimated means),
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
      N <- S1@Data@nobs[[1]]
      S1.output <- list(shb,mhb,ohb,N)
    } else {S1.output <- NULL } #end if converged loop
  } else {S1.output <- NULL } #end try-error loop
  return(S1.output)
}

#' stage1a
#'
#' Stage1a of TSML: Converts the means, covariance matrix, and asymptotic covariance matrix
#' of components to the corresponding quantities for the composites
#'
#' @param S1.output The output of stage1, a list with 4 objects (N, means, covariance matrix,
#' and asymptotic covariance matrix of components)
#' @param C The matrix of component-composite assignments that is the output of stage0
#' (or user-supplied)
#'
#' @return  A list with 4 objects: mhd (estimated means),
#' shd (estimated covariance matrix), ohd (estimated asymptotic covariance matrix
#' of mhb and shb elements) for the composites, and N (sample size)
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
    N <- S1.output[[4]] #not used here, passed on to stage2

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
  S1a.output <- list(shd, mhd,ohd, N)
  return(S1a.output)
}

#' stage2
#'
#' Performs stage2 of TSML. Fits the model to the estimated vector of means and the estimated
#' covariance matrix for the composites using complete data routines, adjusts naive
#' standard errors to obtain robust TS standard errors (for normal data), computes
#' the residual-based test statistic (for normal data)
#'
#' @param S1a.output Output from stage1a, a list with four objects (shd, mhd, ohd, N)
#' @param model The lavaan model for the composites
#' @param runcommand2 Additional arguments to pass to lavaan
#'
#' @return An object of class `twostage`, inheriting class `lavaan`
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
#'out_s2 <- stage2(out_s1a, model = mod1, runcommand2="mimic='EQS'")
#'
#'
stage2 <- function (S1a.output, model,runcommand2=NULL) {
  shd <- S1a.output[[1]]
  mhd <- S1a.output[[2]]
  ohd <- S1a.output[[3]]
  N <- S1a.output[[4]]

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

    #change back to avoid relying on internal lavaan function:
    #S2@ParTable$se_ts <- sqrt(diag(Ohtt))
    #se <- object@ParTable$se_ts[object@ParTable$free!=0] #remove nonfree

    #set TS SEs to initially equal naive SEs, to match nonfree values and length
    S2@ParTable$se_ts <-S2@ParTable$se

    #update se_ts for free parameters
    S2@ParTable$se_ts[S2@ParTable$free!=0] <- sqrt(diag(Ohtt)) #how to check order?

    ## This prior solution relied on an internal lavaan function to ensure correct
    ## order, but RMD check doesn't like this

    #S2@ParTable$se_ts <- lavaan:::lav_model_vcov_se(lavmodel = S2@Model,
    #            lavpartable = S2@ParTable, VCOV = Ohtt)

    #Note: lavInspect(S2,"vcov") will still give the "wrong" (naive) answer
    #Ohtt <- str(S2@vcov$vcov) #also "naive"
    #but I think we want to preserve the naive run
    #we could try to save the vcov in another slot (careful not to break lavaan)

    #Normal-theory residual-based test statistic
    et <- c(residuals(S2)$mean,lav_matrix_vech(residuals(S2)$cov)) #so swap
    ahd <- solve(ohd.reordered)/N
    #executive decision April 15, 2025: Replaced N-1 with N!
    Tres <- (N)*t(et) %*% (ahd - (ahd%*%ddh) %*% solve(t(ddh)%*%ahd%*%ddh) %*% (t(ddh)%*%ahd)) %*% et
    pval <- 1 - stats::pchisq(Tres, df = inspect(S2, "fit")["df"])

    #mauling lavaan -- is this safe?
    #writing into S2@test was not (broke lavaan summary)
    #blavaan directly overwrites @Fit@test slots
    S2@Fit@test$twostage$test <- Tres
    S2@Fit@test$twostage$df <- S2@Fit@test$standard$df
    S2@Fit@test$twostage$pval <- pval

    } #end if

  #assign twostage class to the results
  S2 <- as(S2, "twostage") #cannot direct assign class for S4 objects
  return(S2)

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
#' @return An object of class `twostage`, inheriting class `lavaan`
#'
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
  s2 <- stage2(S1a.output=s1a, model = model,runcommand2=runcommand2)

  return(s2)
}


#rudimentary version
#i want to toggle naive.se to be on for summary, but off for compare functions
parameterEstimates_ts <- function(object,naive.se=TRUE) {

            if (!inherits(object, "twostage")) {
              stop("This does not appear to be an object generated by twostage()!")
            }

            #by default, estimates only (no ses or cis) when class isn't only lavaan
            #therefore, se argument below is needed to get ses
            est<-parameterEstimates(object,se=naive.se, ci=FALSE,remove.nonfree=T)
            names(est)[names(est) == "se"] <- "se_naive"
            names(est)[names(est) == "z"] <- "z_naive"
            names(est)[names(est) == "pvalue"] <- "pvalue_naive"
            se <- object@ParTable$se_ts[object@ParTable$free!=0] #remove nonfree
            z <-  est[,"est"]/se
            pvalue <- 2 * (1 - pnorm(abs(z))) #from lavaan

            out<-cbind(est,se,z,pvalue)
            return(out)
}

