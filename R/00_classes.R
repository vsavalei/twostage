setClass("twostage",
         contains = "lavaan")

setClass("SummaryTwostage",
         slots = list(
           TS_table = "data.frame",
           Tres = "numeric",
           df = "numeric",
           pval = "numeric"
         )
)
