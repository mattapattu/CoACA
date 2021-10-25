## Hybrid3 #####

## State 1 ###
edge1 = new("Edge", edge = c("E", "dch"), prob = 0.333)
edge2 = new("Edge", edge = c("E",  "dcba1"), prob = 0.333)
edge3 = new("Edge", edge = c("E","fga1"), prob = 0.333)
edge4 = new("Edge", edge = c("dcba1", "dcba1.a2kj"), prob = 0.5)
edge5 = new("Edge", edge = c("dcba1", "a2gf"), prob = 0.5)
edge6 = new("Edge", edge = c("fga1", "a2bch"), prob = 0.333)
edge7 = new("Edge", edge = c("fga1", "fga1.a2kj"), prob = 0.333)
edge8 = new("Edge", edge = c("fga1", "a2bcd"), prob = 0.333)


edgeListS0 = list(edge1,edge2,edge3,edge4,edge5,edge6,edge7,edge8);
nodeListS0 = c("E","dch","dcba1","fga1","dcba1.a2kj","fga1.a2kj","a2gf","a2bch","a2bcd")

graphS0 = new("Graph", Name="turnsS0",State=0,
              Path0 = c("dch"),
              Path1 = c("fga1","fga1.a2kj"),
              Path2 = c("dcba1","dcba1.a2kj"),
              Path3 = c("fga1","a2bch"),
              Path4 = c("fga1","a2bcd"),
              Path5 = c("dcba1","a2gf")
)

### State 2 ####
edge9 = new("Edge", edge = c("I", "hcd"), prob = 0.333)
edge10 = new("Edge", edge = c("I", "hcba1"), prob = 0.333)
edge11 = new("Edge", edge = c("I", "jka1"), prob = 0.333)
edge12 = new("Edge", edge = c("hcba1", "hcba1.a2gf"), prob = 0.5)
edge13 = new("Edge", edge = c("hcba1", "a2kj"), prob = 0.5)
edge14 = new("Edge", edge = c("jka1", "a2bch"), prob = 0.333)
edge15 = new("Edge", edge = c("jka1", "jka1.a2gf"), prob = 0.333)
edge16 = new("Edge", edge = c("jka1", "a2bcd"), prob = 0.333)

edgeListS1 = list(edge9,edge10,edge11,edge12,edge13,edge14,edge15,edge16);
nodeListS1 = c("I","hcd","hcba1","jka1","hcba1.a2gf","jka1.a2gf","a2kj","a2bch","a2bcd")


graphS1 = new("Graph", Name ="turnsS1",State=1,
              Path0 = c("hcd"),
              Path1 = c("jka1","jka1.a2gf"),
              Path2 = c("hcba1","hcba1.a2gf"),
              Path3 = c("jka1","a2bcd"),
              Path4 = c("jka1","a2bch"),
              Path5 = c("hcba1","a2kj"))

groups = list(grp1=c(1),grp2=c(3,4,5,6),grp3=c(2,7,8))

Hybrid3 = new("Model", Name = "Hybrid3",
              S0 = graphS0,
              S1 = graphS1,
              nodeGroups = groups,
              edges.S0 = edgeListS0,
              edges.S1 = edgeListS1,
              nodes.S0 = nodeListS0,
              nodes.S1 = nodeListS1)