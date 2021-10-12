testLik=function(ratdata, modelData)
{
  modelData <- new("ModelData", Model = "model", creditAssignment = "aca2", sim = sim)
  modelData@alpha = 0.5
  modelData@gamma1 = 0.9
  modelData@gamma2 = 1
  testModel <- PathModel
  computeHmatrix(ratdata, modelData, testModel,sim=2)
}