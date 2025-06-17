setClass("twostage",
  contains = "lavaan",
  slots = list(twostage = "list")
)

# no need, make S3?
setClass("SummaryTwostage",
  slots = list(
    TS_table = "data.frame",
    Tres = "numeric",
    df = "numeric",
    pval = "numeric"
  )
)
