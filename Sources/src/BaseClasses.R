
#Paths = new("Model", Name = "PathsDummy")
allModels = new("AllModels",Paths = PathModel, Turns = TurnModel,Hybrid1 = Hybrid1,Hybrid2 = Hybrid2, Hybrid3 = Hybrid3,Hybrid4 = Hybrid4)

setClass("TestModels", 
         slots = list(
           Models="character",
           creditAssignment = "character"
         )
)

setClass("BaseModel", 
         slots = list(
           Name = "character", 
           simulateFunc = "function",
           likelihoodFunc ="function",
           probMatFunc = "function",
           rule="character",
           type="character")
)

setClass("ModelData", 
         slots = list(
           Model = "character", 
           creditAssignment = "character",
           alpha = "numeric",
           gamma1 = "numeric",
           gamma2 = "numeric",
           lambda = "numeric",
           likelihood = "numeric",
           probMatrix = "matrix",
           sim = "numeric"
         )
)

setClass("ModelDataList",
         slots = list(
           Model = "character", 
           aca = "ModelData",
           aca2 = "ModelData",
           aca3 = "ModelData",
           gb = "ModelData",
           sarsa = "ModelData"
         )
)


setClass("AllModelRes", 
         representation(
           models = "vector",
           Paths = "ModelDataList",
           Turns = "ModelDataList",
           Hybrid1 = "ModelDataList",
           Hybrid2 = "ModelDataList",
           Hybrid3 = "ModelDataList",
           Hybrid4 = "ModelDataList",
           type = "character"),
         contains = "ModelData"
)

setClass("RatData",
         slots = list(
           rat = "character",
           allpaths="matrix",
           turnTimes = "matrix",
           hybridModel1 = "matrix",
           hybridModel2 = "matrix",
           hybridModel3 = "matrix",
           hybridModel4 = "matrix",
           simModel = "character",
           simMethod = "character",
           simModelData = "ModelData"
          )

)


#### func setModelParams ###
setGeneric("setModelParams", function(x,modelParams) standardGeneric("setModelParams"))
setGeneric("getArgList", function(x,ratdata)  standardGeneric("getArgList"))
setGeneric("setModelResults", function(x,ratdata, allModels)  standardGeneric("setModelResults"))
setGeneric("simulateData", function(x,ratdata,allModels) standardGeneric("simulateData"))
setGeneric("addModelData", function(x,modelData) standardGeneric("addModelData"))
setGeneric("getModelData", function(x,modelName,creditAssignment) standardGeneric("getModelData"))


setMethod("setModelParams",  signature=c("ModelData","numeric"),
          definition=function(x,modelParams)
          {
            if(x@creditAssignment == "aca3")
            {
              x@alpha = modelParams[1]
              x@gamma1 = modelParams[2]
              x@gamma2 = modelParams[3]
            }
            else if(x@creditAssignment == "aca2")
            {
              x@alpha = modelParams[1]
              x@gamma1 = modelParams[2]
            }
            else if(x@creditAssignment == "sarsa")
            {
              x@alpha = modelParams[1]
              x@gamma1 = modelParams[2]
            }
            
            return(x)
          }
)


setMethod("getArgList",  signature=c("ModelData","RatData"),
          definition=function(x,ratdata)
          {
            ratName = ratdata@rat
            #endLearningStage = getEndIndex(ratName, ratdata@allpaths,sim=x@sim, limit=0.95)
            model = x@Model
            testModel = slot(allModels,model)
            #endLearningStage = endLearningStage/2
            
            if(x@creditAssignment == "aca3")
            {
              argList = list(lower = c(0,0,0), 
                             upper = c(1,1,1),
                             ratdata = ratdata,
                             half_index = 0, 
                             modelData = x,
                             testModel = testModel,
                             sim = x@sim)
              
            }
            else if(x@creditAssignment == "aca2")
            {
              argList = list(lower = c(0,0), 
                             upper = c(1,1),
                             ratdata = ratdata,
                             half_index = 0, 
                             modelData = x,
                             testModel = testModel,
                             sim = x@sim)
              
            }
            else if(x@creditAssignment == "sarsa")
            {
              argList = list(lower = c(0,0,0), 
                             upper = c(1,1,0),
                             ratdata = ratdata,
                             half_index = 0, 
                             modelData = x,
                             testModel = testModel,
                             sim = x@sim)
            }
            
            return(argList)
          }
)


setMethod("setModelResults",  signature=c("ModelData","RatData","AllModels"),
          definition=function(x,ratdata,allModels)
          {
            #endLearningStage = getEndIndex(ratdata@allpaths,sim=x@sim, limit=0.95)
            baseModel = getBaseModel(x)
            model = x@Model
            
            # if(model == "Paths")
            #   {
            #     #endLearningStage = endLearningStage/2
            #     x@probMatrix = baseModel@probMatFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
            #     likelihood = baseModel@likelihoodFunc(ratdata@allpaths,x@alpha,x@gamma1,x@gamma2,x@sim)
            #     #x@likelihood = (-1) * sum(likelihood[-(1:endLearningStage)])
            #     x@likelihood = as.numeric(likelihood)
            #   }
            #   else
            #   {
            #     #endLearningStage = endLearningStage/2
            #     testModel = slot(allModels,model)
            #     x@probMatrix = baseModel@probMatFunc(ratdata, x,testModel,x@sim)
            #     likelihood = baseModel@likelihoodFunc(ratdata, x,testModel,x@sim)
            #     #x@likelihood = (-1) * sum(likelihood[-(1:endLearningStage)])
            #     x@likelihood = likelihood
            #     
            #   }
            
            testModel = slot(allModels,model)
            #x@probMatrix = baseModel@probMatFunc(ratdata, x,testModel,x@sim)
            x@probMatrix = TurnsNew::getProbMatrix(ratdata, x,testModel,x@sim)
            likelihood = baseModel@likelihoodFunc(ratdata, x,testModel,x@sim)
            #x@likelihood = (-1) * sum(likelihood[-(1:endLearningStage)])
            x@likelihood = likelihood
            
            return(x)
          }
)

setMethod("simulateData",  signature=c("ModelData","RatData","AllModels"),
          definition=function(x,ratdata,allModels)
          {
            ratName = ratdata@rat
            endStage1 = getEndIndex(ratName,ratdata@allpaths,sim=2,limit=0.5)
            endStage2 = getEndIndex(ratName,ratdata@allpaths,sim=2,limit=0.85)
            endStage3 = length(ratdata@allpaths[,1])
            #pathstages=c(1,endStage1,endStage2,endStage3)
            
            
            # if(x@Model == "Paths")
            # {
            #   testModel = PathModel
            #   turnstages = pathstages
            # }
            # else if(x@Model == "Turns")
            # {
            #   
            #   testModel = TurnModel
            #   turnIdxStage1 = last(which(ratdata@turnTimes[,1]<=endStage1))
            #   turnIdxStage2 = last(which(ratdata@turnTimes[,1]<=endStage2))
            #   turnIdxStage3 = length(ratdata@turnTimes[,1])
            #   turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
            # }
            # else
            # {
            #   model = x@Model
            #   testModel = slot(allModels,model)
            #   testTurnTimes = convertTurnTimes(ratdata,TurnModel,testModel,sim=x@sim)
            #   
            #   turnIdxStage1 = last(which(testTurnTimes[,1]<=endStage1))
            #   turnIdxStage2 = last(which(testTurnTimes[,1]<=endStage2))
            #   turnIdxStage3 = length(testTurnTimes[,1])
            #   turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
            #   
            # }
            # argList = list(ratdata = ratdata, 
            #                modelData = x,
            #                testModel = testModel,
            #                turnstages = turnstages)
            
            model = x@Model
            testModel = slot(allModels,model)

            turnIdxStage1 = last(which(ratdata@turnTimes[,1]<=endStage1))
            turnIdxStage2 = last(which(ratdata@turnTimes[,1]<=endStage2))
            turnIdxStage3 = length(ratdata@turnTimes[,1])
            turnstages = c(1,turnIdxStage1,turnIdxStage2,turnIdxStage3)
            generated_data = TurnsNew::simulateTurnsModels(ratdata,x,testModel,TurnModel,turnstages)
            
            simData = new("RatData", rat = "simulation",allpaths = generated_data$PathData, turnTimes = generated_data$TurnData)
            
            
            # if(x@Model=="Paths")
            # {
            #   slot(simData, "turnTimes") = generated_data$TurnData 
            # }
            # else if(x@Model == "Turns")
            # {
            #   slot(simData, "turnTimes") = generated_data$TurnData
            # }
            # else if(x@Model == "Hybrid1")
            # {
            #   slot(simData, "hybridModel1") = generated_data$TurnData
            # }
            # else if(x@Model == "Hybrid2")
            # {
            #   slot(simData, "hybridModel2") = generated_data$TurnData
            # }
            # else if(x@Model == "Hybrid3")
            # {
            #   slot(simData, "hybridModel3") = generated_data$TurnData
            # }
            # else if(x@Model == "Hybrid4")
            # {
            #   slot(simData, "hybridModel4") = generated_data$TurnData
            # }
            
            
            return(simData)
          }
)


setMethod("addModelData",  signature=c("AllModelRes","ModelData"),
          function(x,modelData)
          {
            model = modelData@Model
            creditAssignment = modelData@creditAssignment
            
            
            slot(slot(x,model),creditAssignment) = modelData
            
            return(x)
          }
)


setMethod("getModelData",  signature=c("AllModelRes","character","character"),
          function(x,modelName,creditAssignment)
          {
            
            return(slot(slot(x,modelName),creditAssignment))
          }
)





### Models 

# pathModelFuncs = new("BaseModel", 
#                      Name = "PathModel", 
#                      simulateFunc = Aca3::simulateTrials, 
#                      likelihoodFunc = Aca3::getPathLikelihood,
#                      probMatFunc = Aca3::getProbMatrix,
#                      type = "paths")

turnModelFuncs = new("BaseModel", 
                     Name = "TurnModel", 
                     simulateFunc = TurnsNew::simulateTurnsModels, 
                     likelihoodFunc = TurnsNew::getTurnsLikelihood,
                     probMatFunc = TurnsNew::getProbMatrix,
                     type = "turns")



# getBaseModel=function(modelData)
# {
#   if(modelData@creditAssignment == "aca3")
#   {
#     if(modelName == "Paths")
#     {
#       baseModel = pathModelFuncs
#     }
#     else
#     {
#       baseModel = turnModelFuncs
#     } 
#   }
#   else if(modelData@creditAssignment == "sarsa")
#   {
#     if(modelName == "Paths")
#     {
#       baseModel = ""
#     }
#     else
#     {
#       baseModel = "turnModelFuncs"
#     } 
#   }
#   
#   return(baseModel)
# }



getBaseModel=function(modelData)
{
  baseModel = turnModelFuncs
  
  return(baseModel)
}



