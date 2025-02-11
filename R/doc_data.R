# Datasets Documentation

#' misdata_mcar20
#'
#'A simulated dataset with MCAR missingness
#'
#' @format
#' A matrix with 200 rows and 27 columns, named Y1-Y27.
#'
#' The following sets of variables have jointly missing data on 20% of their values,
#' with rows randomly selected for each pattern:
#' \{Y1, Y5, Y9\}, \{Y10, Y11\}, \{Y14, Y15, Y16, Y18\}, \{Y20, Y21\}, \{Y22, Y24\},
#' \{Y25, Y26\}.
#'
#' The correct model for the data is a second-order factor model, with 9
#' first-order factors (with three indicators each), and 3 second-order factors (with
#' 3 indicators each). The exact population model that generated this dataset is described
#' in Savalei and Rhemtulla (2017).
#'
#' @references
#' Savalei, V., and Rhemtulla, M. (2017). Normal theory two-stage ML estimator when
#' data are missing at the item level. Journal of Educational and Behavioral Statistics,
#' 42(4), 567-589. https://doi.org/10.3102/1076998617695687
#'
"misdata_mcar20"




#' tpbdata
#'
#'
#' A real dataset collected to test the theory of planned behavior. Data were collected from a
#' sample of N = 108 college students who had the goal of reducing or maintaining their
#' current body weight. There are 11 semantic differential items measuring attitudes
#' toward dieting (6 tapping the cognitive and 5 tapping the affective component
#' of the attitudes), 3 items measures Norms, 3 items measuring perceived behavioral control,
#' 3 items measuring intentions, and 1 item measuring self-report of behavior.
#' For more details, see Perugini and Bagozzi (2001), and Savalei and Bentler (2006).
#'

#' @format
#' A matrix with 108 rows and 21 columns:
#'
#' \describe{
#'   \item{PBC1, PBC2, PBC3}{Perceived Behavioral Control items; e.g.,
#'   "How much control do you have over sticking to a diet for the next four weeks?"}
#'   \item{NORS1, NORS2, NORS3}{Subjective Norms items; "List three most important
#'   persons in your life and indicate how much each would (dis)approve"}
#'   \item{AT1CPU-AT6CPU,AT7CPP-AT11CPP}{Semantic differential items measuring attitudes;
#'   "I think that to keep to a diet in order to decrease my body weight during the next
#'   four weeks is: Useless - Useful, Ineffective - Effective,
#'   Disadvantageous - Advantageous, Stupid - Intelligent, Punishing - Rewarding, Foolish - Wise,
#'   Unpleasant - Pleasant, Joyless - Joyful, Boring - Exciting, Unattractive - Attractive,
#'   Unenjoyable - Enjoyable." (7 pt scale)}
#'   \item{INT1, INT2, INT3}{Intention items; e.g., "I will expend effort to stick to a diet in the next four weeks".}
#'   \item{BEH}{Self-report of behavior, collected over the phone four weeks later.}
#'    }
#' @references
#'
#' Perugini, M., and Bagozzi, R. P. (2001). The role of desires and anticipated emotions
#' in goal-directed behaviours: Broadening and deepening the theory of planned behaviour.
#' British Journal of Social Psychology, 40(1), 79-98. https://doi.org/10.1348/014466601164704
#'
#' Savalei, V., and Bentler, P. M. (2006). Structural equation modeling.
#' In Handbook of Statistics, Volume 26 (pp. 330-364). Elsevier.
"tpbdata"




#' GaspardEtal2015mathUtility
#'
#'A dataset provided by Rose et al (2019). The dataset contains 26 variables.
#'It has been modified by adding variable names and changing missing data codes
#'from -99 to NA.
#'
#' @format
#'
#' The order of variables in the data set is: 1.	id, 2.	school, 3.	class, 4.	sex, 5.	math
#' 6.	ca, 7.	isei_m, 8.	isei_d, 9.	edu_m, 10.	edu_d, 11.	voc_m, 12.	voc_d, 13.	book
#' 14.	income, 15.	y1 - y12
#'
#' The variables isei_m, isei_d, edu_m, edu_d, voc_m, voc_d, book, and income are z-standardized.
#' More Information about the data and the variables can be found in Rose et al. (2019).
#'
#' @references
#' Rose, N., Wagner, W., Mayer, A., & Nagengast, B. (2019). Model-based manifest and latent
#' composite scores in structural equation models. Collabra: Psychology, 5(1), Article 9.
#' https://doi.org/10.1525/collabra.143
#'
#' @source <https://osf.io/f3kzb/>
#'
"GaspardEtal2015mathUtility"

