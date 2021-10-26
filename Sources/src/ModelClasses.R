setClass("Edge", 
         representation(
           edge = "character",
           prob = "numeric"
           )
        )
setClass("Graph", 
         contains = "Edge",
         representation(
           Name = "character",
           State = "numeric",
           Path0 = "character",
           Path1 = "character",
           Path2 = "character",
           Path3 = "character",
           Path4 = "character",
           Path5 = "character"
         )
)

setClass("Model",
         contains = "Graph",
         representation(
           S0 = "Graph",
           S1 = "Graph",
           nodeGroups = "list",
           edges.S0 = "list",
           edges.S1 = "list",
           nodes.S0 = "character",
           nodes.S1 = "character",
           turnNodes.S0 = "list",
           turnNodes.S1 = "list"
         ))

setClass("AllModels",
         contains = "Graph",
         representation(
           Paths = "Model",
           Turns = "Model",
           Hybrid1 = "Model",
           Hybrid2 = "Model",
           Hybrid3 = "Model",
           Hybrid4 = "Model"
         ))
  