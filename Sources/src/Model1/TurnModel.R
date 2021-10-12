### Turns Model ######

## State 1 ###
edge1 = new("Edge", edge = c("E", "dc1"), prob = 0.5)
edge2 = new("Edge", edge = c("E",  "fga1"), prob = 0.5)
edge3 = new("Edge", edge = c("dc1","c2h"), prob = 0.5)
edge4 = new("Edge", edge = c("dc1", "c2ba1"), prob = 0.5)
edge5 = new("Edge", edge = c("fga1", "a2bc1"), prob = 0.5)
edge6 = new("Edge", edge = c("fga1", "a2kj"), prob = 0.5)
edge7 = new("Edge", edge = c("a2bc1", "c2d"), prob = 0.5)
edge8 = new("Edge", edge = c("a2bc1", "c2h"), prob = 0.5)
edge9 = new("Edge", edge = c("c2ba1", "a2kj"), prob = 0.5)
edge10 = new("Edge", edge = c("c2ba1", "a2gf"), prob = 0.5)


edgeListS0 = list(edge1,edge2,edge3,edge4,edge5,edge6,edge7,edge8,edge9,edge10);
nodeListS0 = c("E","dc1","fga1","c2h","c2ba1","a2bc1","a2kj","a2gf","c2d")


### State 2 ####
edge11 = new("Edge", edge = c("I", "hc1"), prob = 0.5)
edge12 = new("Edge", edge = c("I", "jka1"), prob = 0.5)
edge13 = new("Edge", edge = c("hc1", "c2d"), prob = 0.5)
edge14 = new("Edge", edge = c("hc1", "c2ba1"), prob = 0.5)
edge15 = new("Edge", edge = c("c2ba1", "a2kj"), prob = 0.5)
edge16 = new("Edge", edge = c("c2ba1", "a2gf"), prob = 0.5)
edge17 = new("Edge", edge = c("jka1", "a2bc1"), prob = 0.5)
edge18 = new("Edge", edge = c("jka1", "a2gf"), prob = 0.5)
edge19 = new("Edge", edge = c("a2bc1", "c2d"), prob = 0.5)
edge20 = new("Edge", edge = c("a2bc1", "c2h"), prob = 0.5)

edgeListS1 = list(edge11,edge12,edge13,edge14,edge15,edge16,edge17,edge18,edge19,edge20);
nodeListS1 = c("I","hc1","jka1","c2d","c2ba1","a2bc1","a2kj","a2gf","c2h")




graphS0 = new("Graph", Name="turnsS0",State=0,
              Path0 = c("dc1","c2h"),
              Path1 = c("fga1","a2kj"),
              Path2 = c("dc1","c2ba1","a2kj"),
              Path3 = c("fga1","a2bc1","c2h"),
              Path4 = c("fga1","a2bc1","c2d"),
              Path5 = c("dc1","c2ba1","a2gf")
)

graphS1 = new("Graph", Name ="turnsS1",State=1,
              Path0 = c("hc1","c2d"),
              Path1 = c("jka1","a2gf"),
              Path2 = c("hc1","c2ba1","a2gf"),
              Path3 = c("jka1","a2bc1","c2d"),
              Path4 = c("jka1","a2bc1","c2h"),
              Path5 = c("hc1","c2ba1","a2kj"))


groups = list(grp1=c(1,2,3,6,7,8),grp2=c(4,5))

TurnModel = new("Model", Name = "TurnModel",
                 S0 = graphS0,
                 S1 = graphS1,
                 nodeGroups = groups,
                 edges.S0 = edgeListS0,
                 edges.S1 = edgeListS1,
                 nodes.S0 = nodeListS0,
                 nodes.S1 = nodeListS1)

