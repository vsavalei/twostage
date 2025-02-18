
#this test is for a scenario with three composites, one of length one
test_that("df of a PIM model are zero", {
  #C1 is the sum of Y1, Y2, and Y3
  #' #C2 is the sum of Y4, Y5, and Y6
  #' #C3 is Y7
  C<-matrix(0,nrow=3,ncol=7)
  C[1,1:3]<-1
  C[2,4:6]<-1
  C[3,7]<-1
  rownames(C)<-c("C1","C2","C3")
  colnames(C)<-c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
  compmodel<-"C1 ~ C2 + C3"
  model1 <- PIM_syntax(C,compmodel)
  data1 <- misdata_mcar20[,c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")]
  out <- lavaan::sem(model1,data=data1)
  df <- as.numeric(fitmeasures(out)["df"])
  expect_equal(df, 0)
})

#add a test that the result is the same as with composites for complete data

#also, what happens if the composite is named the same as its 1 component, as in
#the tpb example? Is it possible that we will now break the PIM model code?
#how to deal with this?? we may have to revert back to not treating it as a comp
#i guess this is similar to having other exogenous variables in the model, e.g.,
#gender, age, etc. So the data that are being used to create the C matrix can be
#a subcomponent of the dataset that has other observed variables. From this perspective,
#the observed variables that are not composites should not be allowed in C
#we can print an error to that extent?
#however, perhaps we need to know all the variables in the model (incl. age, gender)
#to set PIM? Figure this out tomorrow -- I don't recall what is needed for PIM
