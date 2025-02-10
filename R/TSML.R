# Summary of TSML functions #################

#helper functions:
  #namesohd: function to name rows/columns of the asy cov matrix from stage1
  #write.sat: creates the saturated model syntax in lavaan

#TSML functions:
  #stage0: assigns items to composites based on user input
  #stage1: performs Stage 1 (saturated model fit to all items)
  #stage1a: performs Stage 1a -- converts sat model estimates to values for composites
  #stage2: fits the model to mean and cov matrix from stage1a to composites, computes
  #TS standard errors and residual based test statistic
  #twostage: runs all of the TSML functions at once


namesohd<- function (cnames) {
  name_grid_cov <- t(outer(cnames,cnames,function(x,y) paste0(x,"~~",y)))
  names_cov <- name_grid_cov[lower.tri(name_grid_cov, diag = TRUE)]
  names_mean <- paste0(cnames,"~",1)
  namesohd <- c(names_cov,names_mean)
  return(namesohd)
}


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




#Stage0 (prep stage) of TSML:
#assigns items to composites based on user input
#input is the data and the model for composites
#output is a matrix C for stage1a
stage0<-function (data, model) {
  cnames<-lavNames(model)
  C <- matrix(0,nrow=length(cnames),ncol=length(colnames(data)))
  colnames(C)<-colnames(data)
  rownames(C)<-cnames

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

#Stage 1 of TSML: fits a saturated model to items with missing data
#Produces a list of 3 objects: mhb, shb, ohb, which are means, covariances,
#and asy cov matrix for the items
#runcommand can be used to pass additional arguments to lavaan
#' @export
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

#Stage 1a of TSML: converts Stage1 matrices to Stage2 dimensions
# (from components to composites)
#Input: output of stage1 and matrix C (output of stage0 or manually constructed)
#Produces a list of 4 objects: mhd, shd, dh, ohd
# mhd -- #mu-hat-delta (composite EM mean vector, under saturated model) from Stage 1a, k x 1
# shd -- #sigma-hat-delta (composite EM cov matrix, under saturated model) from Stage 1a, k x k
# dh --  #delta-hat, a vector combining vech(shd) and mhd from Stage 1a (p. 5, line 7 of article), length k*(k+1)/2+k
# ohd -- #asy cov matrix of dh, square with dimensions k*(k+1)/2+k (p. 5, last line in Stage 1a section)
#' @export
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

#stage2: Fits the model to composites, computes TS standard errors and res-based T
#' @export
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


#runs all of the TSML function in sequence
#' Title
#'
#' @param data Your data file with components (items)
#' @param model Your model for the composites
#'
#' @returns A list with several objects
#'
#' @import lavaan
#' @export
#'
#' @examples
#' #To be added later
#'
#' @references
#'Savalei, V., & Rhemtulla, M. (2017). Normal theory two-stage ML estimator when data are missing at the item level. Journal of Educational and Behavioral Statistics, 42(1), 1-27. https://doi.org/10.3102/1076998617694880

twostage <- function (data,model) {
  N <- nrow(data)
  #stage 0: relating components to composite (user input will be requested)
  C <- stage0(data=data, model=model)
  #stage 1: saturated model on components
  s1 <- stage1(data = data)
  #stage1a: saturated solution transformation to composites
  s1a <-stage1a(S1.output=s1,C)
  #stage2: model fit to composites
  s2 <- stage2(S1a.output=s1a, N = N,model = model)

  #output for end user (turn into summary later)
  df<-fitmeasures(s2$TS_Run_naive)["df"]
  est<-cbind(parameterestimates(s2$TS_Run_naive,remove.nonfree=T)[,1:5])
  TS_table<-cbind(est,s2$TS_SEs)
  print("Two-stage parameter estimates, naive standard errors, and two-stage standard errors:")
  print(TS_table)
  test.output<-paste("The residual based chi-square is",
                      round(s2$T_res,3),"against",df, "degrees of freedom, with a p-value of",
                     round(s2$T_res_pvalue,3))

  print(test.output)

  return(s2)
}

