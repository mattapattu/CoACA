### Turns Model ######

## State 1 ###
edge1 = new("Edge", edge = c("E", "dch"), prob = 0.25)
edge2 = new("Edge", edge = c("E",  "dcba1"), prob = 0.25)
edge3 = new("Edge", edge = c("E","fgakj"), prob = 0.25)
edge4 = new("Edge", edge = c("E", "fgabc1"), prob = 0.25)
edge5 = new("Edge", edge = c("dcba1", "a2gf"), prob = 0.5)
edge6 = new("Edge", edge = c("dcba1", "a2kj"), prob = 0.5)
edge7 = new("Edge", edge = c("fgabc1", "c2d"), prob = 0.5)
edge8 = new("Edge", edge = c("fgabc1", "c2h"), prob = 0.5)


edgeListS0 = list(edge1,edge2,edge3,edge4,edge5,edge6,edge7,edge8);
nodeListS0 = c("E","dch","dcba1","fgakj","fgabc1","a2gf","a2kj","c2h","c2d")


### State 2 ####
edge9 = new("Edge", edge = c("I", "hcd"), prob = 0.25)
edge10 = new("Edge", edge = c("I", "hcba1"), prob = 0.25)
edge11 = new("Edge", edge = c("I", "jkagf"), prob = 0.25)
edge12 = new("Edge", edge = c("I", "jkabc1"), prob = 0.25)
edge13 = new("Edge", edge = c("hcba1", "a2kj"), prob = 0.5)
edge14 = new("Edge", edge = c("hcba1", "a2gf"), prob = 0.5)
edge15 = new("Edge", edge = c("jkabc1", "c2d"), prob = 0.5)
edge16 = new("Edge", edge = c("jkabc1", "c2h"), prob = 0.5)

edgeListS1 = list(edge9,edge10,edge11,edge12,edge13,edge14,edge15,edge16);
nodeListS1 = c("I","hcd","hcba1","jkagf","jkabc1","a2kj","a2gf","c2d","c2h")




graphS0 = new("Graph", Name="turnsS0",State=0,
              Path0 = c("dch"),
              Path1 = c("fgakj"),
              Path2 = c("dcba1","a2kj"),
              Path3 = c("fgabc1","c2h"),
              Path4 = c("fgabc1","c2d"),
              Path5 = c("dcba1","a2gf")
)

graphS1 = new("Graph", Name ="turnsS1",State=1,
              Path0 = c("hcd"),
              Path1 = c("jkagf"),
              Path2 = c("hcba1","a2gf"),
              Path3 = c("jkabc1","c2d"),
              Path4 = c("jkabc1","c2h"),
              Path5 = c("hcba1","a2kj"))

groups = list(grp1=c(1),grp2=c(2,3,4), grp3 = c(5,6,7,8))

Hybrid1 = new("Model", Name = "Hybrid1",
                 S0 = graphS0,
                 S1 = graphS1,
                 nodeGroups = groups,
                 edges.S0 = edgeListS0,
                 edges.S1 = edgeListS1,
                 nodes.S0 = nodeListS0,
                 nodes.S1 = nodeListS1)