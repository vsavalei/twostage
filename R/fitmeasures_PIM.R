
## fit measures for PIM

# simplification of lav_fit_srmr_mplus, available in lav_fit_srmr.R
# modified to have dependence on two lavaan objects and the compmodel
# the harmelss dependence on G partially left, for (hopefully) a future generalization

#' Title
#'
#' @param lavobject  Estimated PIM (H0)
#' @param lavobject_sat Estimated saturated PIM (H1)
#' @param compmodel Composites Model
#'
#' @returns A list of SRMR values, without means and with means
#' @export
#'
#' @examples
#' #make a nonboring example, here H0 is saturated
#' library(lavaan)
#' misdata_1to7<-misdata_mcar20[,1:7] #first 7 vars, for illustration
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
#' PIM_model <- PIM_syntax(C=C,compmodel=compmodel)
#' PIM_model_sat <- PIM_syntax_sat(C=C,compmodel=compmodel)
#' fit_pim <- lavaan(PIM_model, data=misdata_1to7)
#' fit_pim_sat <- lavaan(PIM_model_sat, data=misdata_1to7)
#' srmr <- srmr_mplus_pim(fit_pim,fit_pim_sat,compmodel)

srmr_mplus_pim <- function(lavobject,lavobject_sat,compmodel) {

  comp_names <- lavNames(compmodel) #variables to use in SRMR

  # ngroups
  G <- lavobject@Data@ngroups

  # container per group -- each a vector of length G initialized to 0
  srmr_mplus.group <- numeric(G)
  srmr_mplus_nomean.group <- numeric(G)

  for (g in 1:G) {

    # H1 (sat), EM estimates
    #S <- lavobject@SampleStats@missing.h1[[g]]$sigma # but I need names...
    #M <- lavobject@SampleStats@missing.h1[[g]]$mu
    #comp data
    #S <- lavobject@SampleStats@cov[[g]] #but I need names!
    #M <- lavobject@SampleStats@mean[[g]]

    # Is lavNames(lavobject) guaranteed to retrieve them in the right order?
    # Because I need to a subset of them, this is really important.
    # Relying on lavInspect for now (lost generalization to MG from here on)

    #saturated H1 model, based on lavobject_sat
    S <- lavInspect(lavobject_sat,"cov.all")[comp_names, comp_names, drop = FALSE]
    M_lv <- lavInspect(lavobject_sat,"mean.lv")
    M_ov <- lavInspect(lavobject_sat,"mean.ov")
    M_all <-c(M_lv,M_ov)
    M <- M_all[comp_names, drop = FALSE]

    nvar <- ncol(S)

    # H0, estimated model, based on lavobject
    #implied <- lavobject@implied    #but I need variable names!
    #lavmodel <- lavobject@Model
    #Sigma.hat <- implied$cov[[g]]
    #Mu.hat <- implied$mean[[g]]

    Sigma.hat <- lavInspect(lavobject,"cov.all")[comp_names, comp_names, drop = FALSE]
    Mu_lv <- lavInspect(lavobject,"mean.lv")
    Mu_ov <- lavInspect(lavobject,"mean.ov")
    Mu_all <-c(Mu_lv,Mu_ov)
    Mu.hat <- Mu_all[comp_names, drop = FALSE]

    # Bollen approach: simply using cov2cor ('correlation residuals')
    S.cor <- stats::cov2cor(S)
    Sigma.cor <- stats::cov2cor(Sigma.hat)
    R.cor <- (S.cor - Sigma.cor)

    # meanstructure
    if (lavobject@Model@meanstructure) {
      # standardized residual mean vector
      R.cor.mean <- M / sqrt(diag(S)) - Mu.hat / sqrt(diag(Sigma.hat))

      e <- nvar * (nvar + 1) / 2 + nvar
      srmr_mplus.group[g] <-
        sqrt((sum(R.cor[lower.tri(R.cor, diag = FALSE)]^2) +
                sum(R.cor.mean^2) +
                sum(((diag(S) - diag(Sigma.hat)) / diag(S))^2)) / e)

      e <- nvar * (nvar + 1) / 2
      srmr_mplus_nomean.group[g] <-
        sqrt((sum(R.cor[lower.tri(R.cor, diag = FALSE)]^2) +
                sum(((diag(S) - diag(Sigma.hat)) / diag(S))^2)) / e)
    } else {
      e <- nvar * (nvar + 1) / 2
      srmr_mplus_nomean.group[g] <- srmr_mplus.group[g] <-
        sqrt((sum(R.cor[lower.tri(R.cor, diag = FALSE)]^2) +
                sum(((diag(S) - diag(Sigma.hat)) / diag(S))^2)) / e)
    }
  } # G

  out<-list(srmr_mplus_nomean.group,srmr_mplus.group)
  #what happens if no mean structure? not our problem, but does the value stay at 0
  names(out) <-c("srmr_mplus_nomean","srmr_mplus")
  return(out)
}
