## Hybrid2 #####

## State 1 ###
edge1 = new("Edge", edge = c("E", "dc1"), prob = 0.333)
edge2 = new("Edge", edge = c("E",  "fgakj"), prob = 0.333)
edge3 = new("Edge", edge = c("E", "fgabc1"), prob = 0.333)
edge4 = new("Edge", edge = c("dc1","c2h"), prob = 0.333)
edge5 = new("Edge", edge = c("dc1", "c2bakj"), prob = 0.333)
edge6 = new("Edge", edge = c("dc1", "c2bagf"), prob = 0.333)
edge7 = new("Edge", edge = c("fgabc1", "c2h"), prob = 0.5)
edge8 = new("Edge", edge = c("fgabc1", "c2d"), prob = 0.5)


edgeListS0 = list(edge1,edge2,edge3,edge4,edge5,edge6,edge7,edge8);
nodeListS0 = c("E","dc1","fgakj","fgabc1","c2h","c2bakj","c2bagf","c2d")


### State 2 ####
edge9 = new("Edge", edge = c("I", "hc1"), prob = 0.333)
edge10 = new("Edge", edge = c("I", "jkagf"), prob = 0.333)
edge11 = new("Edge", edge = c("I", "jkabc1"), prob = 0.333)
edge12 = new("Edge", edge = c("hc1", "c2d"), prob = 0.333)
edge13 = new("Edge", edge = c("hc1", "c2bakj"), prob = 0.333)
edge14 = new("Edge", edge = c("hc1", "c2bagf"), prob = 0.333)
edge15 = new("Edge", edge = c("jkabc1", "c2d"), prob = 0.5)
edge16 = new("Edge", edge = c("jkabc1", "c2h"), prob = 0.5)

edgeListS1 = list(edge9,edge10,edge11,edge12,edge13,edge14,edge15,edge16);
nodeListS1 = c("I","hc1","jkagf","jkabc1","c2d","c2bakj","c2bagf","c2h")




graphS0 = new("Graph", Name="turnsS0",State=0,
              Path0 = c("dc1","c2h"),
              Path1 = c("fgakj"),
              Path2 = c("dc1","c2bakj"),
              Path3 = c("fgabc1","c2h"),
              Path4 = c("fgabc1","c2d"),
              Path5 = c("dc1","c2bagf")
)

graphS1 = new("Graph", Name ="turnsS1",State=1,
              Path0 = c("hc1","c2d"),
              Path1 = c("jkagf"),
              Path2 = c("hc1","c2bagf"),
              Path3 = c("jkabc1","c2d"),
              Path4 = c("jkabc1","c2h"),
              Path5 = c("hc1","c2bakj"))

groups = list(grp1=c(1,4,7),grp2=c(2,3,5,6))

Hybrid4 = new("Model", Name = "Hybrid4",
              S0 = graphS0,
              S1 = graphS1,
              nodeGroups = groups,
              edges.S0 = edgeListS0,
              edges.S1 = edgeListS1,
              nodes.S0 = nodeListS0,
              nodes.S1 = nodeListS1)