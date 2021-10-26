### Paths Model ######

## State 1 ###
edge1 = new("Edge", edge = c("E", "dch"), prob = 0.166)
edge2 = new("Edge", edge = c("E",  "dcbakj"), prob = 0.166)
edge3 = new("Edge", edge = c("E","fgakj"), prob = 0.166)
edge4 = new("Edge", edge = c("E", "fgabch"), prob = 0.166)
edge5 = new("Edge", edge = c("E", "fgabcd"), prob = 0.166)
edge6 = new("Edge", edge = c("E", "dcbagf"), prob = 0.166)

edgeListS0 = list(edge1,edge2,edge3,edge4,edge5,edge6);
nodeListS0 = c("E","dch","fgakj","dcbakj","fgabch","fgabcd","dcbagf")

NodesToTurnsS0 <- list()
NodesToTurnsS0[["dch"]] <- c("dc1","c2h")
NodesToTurnsS0[["fgakj"]] <- c("fga1","a2kj")
NodesToTurnsS0[["dcbakj"]] <- c("dc1","c2ba1","a2kj")
NodesToTurnsS0[["fgabch"]] <- c("fga1","a2bc1","c2h")
NodesToTurnsS0[["fgabcd"]] <- c("fga1","a2bc1","c2d")
NodesToTurnsS0[["dcbagf"]] <- c("dc1","c2ba1","a2gf")

### State 2 ####
edge7 = new("Edge", edge = c("I", "hcd"), prob = 0.166)
edge8 = new("Edge", edge = c("I", "hcbagf"), prob = 0.166)
edge9 = new("Edge", edge = c("I", "jkagf"), prob = 0.166)
edge10 = new("Edge", edge = c("I", "jkabcd"), prob = 0.166)
edge11 = new("Edge", edge = c("I", "jkabch"), prob = 0.166)
edge12 = new("Edge", edge = c("I", "hcbakj"), prob = 0.166)

edgeListS1 = list(edge7,edge8,edge9,edge10,edge11,edge12);
nodeListS1 = c("I","hcd","jkagf","hcbagf","jkabcd","jkabch","hcbakj")


NodesToTurnsS1 <- list()
NodesToTurnsS1[["hcd"]] <- c("hc1","c2d")
NodesToTurnsS1[["jkagf"]] <- c("jka1","a2gf")
NodesToTurnsS1[["hcbagf"]] <- c("hc1","c2ba1","a2gf")
NodesToTurnsS1[["jkabcd"]] <- c("jka1","a2bc1","c2d")
NodesToTurnsS1[["jkabch"]] <- c("jka1","a2bc1","c2h")
NodesToTurnsS1[["hcbakj"]] <- c("hc1","c2ba1","a2kj")


graphS0 = new("Graph", Name="PathsS0",State=0,
              Path0 = c("dch"),
              Path1 = c("fgakj"),
              Path2 = c("dcbakj"),
              Path3 = c("fgabch"),
              Path4 = c("fgabcd"),
              Path5 = c("dcbagf")
)

graphS1 = new("Graph", Name ="PathsS1",State=1,
              Path0 = c("hcd"),
              Path1 = c("jkagf"),
              Path2 = c("hcbagf"),
              Path3 = c("jkabcd"),
              Path4 = c("jkabch"),
              Path5 = c("hcbakj"))


groups = list(grp1=c(1),grp2=c(2,3,4,5,6))

PathModel = new("Model", Name = "PathModel",
                S0 = graphS0,
                S1 = graphS1,
                nodeGroups = groups,
                edges.S0 = edgeListS0,
                edges.S1 = edgeListS1,
                nodes.S0 = nodeListS0,
                nodes.S1 = nodeListS1,
                turnNodes.S0 = NodesToTurnsS0,
                turnNodes.S1 = NodesToTurnsS1
                )

