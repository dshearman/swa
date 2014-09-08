## Make sure image URL directory is not readable, only executable!

cat('`r require("igraph"); image_path = "graphs_intro"; image_url = "http://www.scem.uws.edu.au/~lapark/300958/images/graphs_intro"`')
  
questions = list(
    list(
      type =  'NUM',
      known.parameters =  'x `r require("igraph"); png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()`',
      question = '<p>In the following graph, how many vertices are there in the degree 1.5 egocentric graph centred on vertex `r x`?</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r d = degree(g); d[x] + 1`',
      tolerance = "0.1"
    ),
    list(
      type =  'NUM',
      known.parameters =  'x `r require("igraph"); png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()`',
      question = '<p>In the following graph, compute the closeness centrality, using the method shown in the lecture, of vertex `r x`.</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r d = 1/closeness(g); d[x]`',
      tolerance = "0.1"
    ),
    list(
      type =  'NUM',
      known.parameters =  'file_name g `r require("igraph"); png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()`',
      question = '<p>Compute the diameter of the following graph.</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r diameter(g)`',
      tolerance = "0.1"
    ),
    list(
      type =  'NUM',
      known.parameters =  'file_name g `r require("igraph"); png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()`',
      question = '<p>Compute the density of the following graph.</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r graph.density(g)`',
      tolerance = "0.1"
    ),
    list(
      type =  'NUM',
      known.parameters =  'x `r require("igraph"); png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5, 4-5, 5-6, 6-7, 7-2); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()`',
      question = '<p>In the following graph, compute the closeness centrality, using the method shown in the lecture, of vertex `r x`.</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r d = 1/closeness(g); d[which(names(d) == x)]`',
      tolerance = "0.1"
    )
)




yaml.out(questions[[1]], 'x = 1; file_name = "eidee9chaejaiMohy1oh.png"; z = c(1,1,1,1,2,2,3,3,4,5,5,5,6,8,8,8,8,2,3,4,7,3,11,4,5,5,6,8,9,7,9,10,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[1]], 'x = 2; file_name = "eidee9chaejaiMohy1oh.png"; z = c(1,1,1,1,2,2,3,3,4,5,5,5,6,8,8,8,8,2,3,4,7,3,11,4,5,5,6,8,9,7,9,10,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[1]], 'x = 3; file_name = "eidee9chaejaiMohy1oh.png"; z = c(1,1,1,1,2,2,3,3,4,5,5,5,6,8,8,8,8,2,3,4,7,3,11,4,5,5,6,8,9,7,9,10,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[1]], 'x = 4; file_name = "eidee9chaejaiMohy1oh.png"; z = c(1,1,1,1,2,2,3,3,4,5,5,5,6,8,8,8,8,2,3,4,7,3,11,4,5,5,6,8,9,7,9,10,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')

#yaml.out(questions[[1]], 'x = 1; file_name = "eidee9chaejaiMohy1oh.png"; g = graph.formula(1-2, 2-3, 3-4, 1-3, 1-4, 3-5, 4-5, 5-6, 6-7, 1-7, 5-8, 5-9, 8-9, 8-10, 8-11, 8-12, 11-2);')
#yaml.out(questions[[1]], 'x = 2; file_name = "eidee9chaejaiMohy1oh.png"; g = graph.formula(1-2, 2-3, 3-4, 1-3, 1-4, 3-5, 4-5, 5-6, 6-7, 1-7, 5-8, 5-9, 8-9, 8-10, 8-11, 8-12, 11-2);')
#yaml.out(questions[[1]], 'x = 3; file_name = "eidee9chaejaiMohy1oh.png"; g = graph.formula(1-2, 2-3, 3-4, 1-3, 1-4, 3-5, 4-5, 5-6, 6-7, 1-7, 5-8, 5-9, 8-9, 8-10, 8-11, 8-12, 11-2);')
#yaml.out(questions[[1]], 'x = 4; file_name = "eidee9chaejaiMohy1oh.png"; g = graph.formula(1-2, 2-3, 3-4, 1-3, 1-4, 3-5, 4-5, 5-6, 6-7, 1-7, 5-8, 5-9, 8-9, 8-10, 8-11, 8-12, 11-2);')

yaml.out(questions[[1]], 'x = 1; file_name = "KiSxIixEMcoByDRH6n9J.png"; z = c(1,2,1,3,2,2,5,1,2,1,2,6,8,2,5,5,7,8,9,7,3,3,4,4,5,6,6,7,7,8,8,8,9,10,10,11,11,11,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[1]], 'x = 2; file_name = "KiSxIixEMcoByDRH6n9J.png"; z = c(1,2,1,3,2,2,5,1,2,1,2,6,8,2,5,5,7,8,9,7,3,3,4,4,5,6,6,7,7,8,8,8,9,10,10,11,11,11,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[1]], 'x = 7; file_name = "KiSxIixEMcoByDRH6n9J.png"; z = c(1,2,1,3,2,2,5,1,2,1,2,6,8,2,5,5,7,8,9,7,3,3,4,4,5,6,6,7,7,8,8,8,9,10,10,11,11,11,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[1]], 'x = 12; file_name = "KiSxIixEMcoByDRH6n9J.png"; z = c(1,2,1,3,2,2,5,1,2,1,2,6,8,2,5,5,7,8,9,7,3,3,4,4,5,6,6,7,7,8,8,8,9,10,10,11,11,11,11,12); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')


yaml.out(questions[[2]], 'x = 1; file_name = "yooSee7apooX3ooph3ah.png"; z = c(1,1,2,2,2,2,4,5,6,2,3,3,4,5,7,5,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[2]], 'x = 2; file_name = "yooSee7apooX3ooph3ah.png"; z = c(1,1,2,2,2,2,4,5,6,2,3,3,4,5,7,5,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[2]], 'x = 3; file_name = "yooSee7apooX3ooph3ah.png"; z = c(1,1,2,2,2,2,4,5,6,2,3,3,4,5,7,5,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[2]], 'x = 4; file_name = "yooSee7apooX3ooph3ah.png"; z = c(1,1,2,2,2,2,4,5,6,2,3,3,4,5,7,5,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')

yaml.out(questions[[2]], 'x = 1; file_name = "Fwy3FRjIuFBAywMmmURN.png"; z = c(1,1,2,1,3,4,1,5,6,2,3,3,5,5,5,6,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[2]], 'x = 5; file_name = "Fwy3FRjIuFBAywMmmURN.png"; z = c(1,1,2,1,3,4,1,5,6,2,3,3,5,5,5,6,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[2]], 'x = 7; file_name = "Fwy3FRjIuFBAywMmmURN.png"; z = c(1,1,2,1,3,4,1,5,6,2,3,3,5,5,5,6,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[2]], 'x = 2; file_name = "Fwy3FRjIuFBAywMmmURN.png"; z = c(1,1,2,1,3,4,1,5,6,2,3,3,5,5,5,6,6,7); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')


yaml.out(questions[[3]], 'file_name = "deexohlieSahchei1lae.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')
yaml.out(questions[[3]], 'file_name = "shingeidosheephoor4S.png"; g = graph.formula(1-2, 2-3, 3-4, 4-5, 3-5); ')
yaml.out(questions[[3]], 'file_name = "wi7caak3iare1Oacheet.png"; g = graph.formula(1-2, 2-3, 3-4, 2-5, 3-5); ')
yaml.out(questions[[3]], 'file_name = "bo6queiqueeHeic9sho8.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5, 5-3); ')

yaml.out(questions[[3]], 'file_name = "n51rezmCoKqkyezy5TE4.png"; z = c(1,2,2,2,3,4,1,2,3,3,4,5,5,5,6,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[3]], 'file_name = "w58mY6SwP8vecM2pTJL5.png"; z = c(1,2,2,3,1,2,4,2,3,4,5,6,6,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[3]], 'file_name = "J6nTu0cwxn9RqEXzIb0f.png"; z = c(1,1,3,1,2,3,4,3,4,2,4,4,5,5,5,5,6,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[3]], 'file_name = "5DeM95ibmpImj7Wn4j3C.png"; z = c(1,1,2,3,3,4,1,3,5,2,4,4,4,5,5,6,6,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')

yaml.out(questions[[4]], 'file_name = "Bieza2ug3uBahdeeGhei.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')
yaml.out(questions[[4]], 'file_name = "heih0iViungis5Ueng5g.png"; g = graph.formula(1-2, 2-3, 3-4, 4-5, 5-1); ')
yaml.out(questions[[4]], 'file_name = "eigh7aeYeegh9EiMahsu.png"; g = graph.formula(1-2, 1-3, 1-4, 1-5, 2-3, 3-4, 4-5, 5-1); ')
yaml.out(questions[[4]], 'file_name = "kahx6seVeix9utohthae.png"; g = graph.formula(1-2, 1-3, 1-4, 1-5, 2-3, 2-4, 2-5, 3-4, 3-5, 4-5); ')

yaml.out(questions[[4]], 'file_name = "CdqnGf9OeBZ0GvzC1UZX.png"; z = c(1,1,2,3,4,3,4,3,4,4,4,5,6,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[4]], 'file_name = "WwEho7kuoOfJtEOv4g11.png"; z = c(1,1,2,1,2,3,1,2,3,4,1,2,3,4,5,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[4]], 'file_name = "kp6lSFWs92clSVA1PYlX.png"; z = c(1,1,4,2,3,4,5,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')
yaml.out(questions[[4]], 'file_name = "DQepK0mgcOo3tNju9pfP.png"; z = c(1,2,2,3,1,2,1,2,3,4,4,5,5,6); g = graph.edgelist(matrix(z, ncol=2), directed=FALSE);')



# generate random graph values
# g = erdos.renyi.game(6, p =  0.5)
# paste(get.edgelist(g), collapse=",")
#z = c(1,2,2,4,1,2,4,2,3,5,5,6,6,6)
#g = graph.edgelist(matrix(z, ncol=2), directed=FALSE)
# generate random file name
# paste(sample(c(0:9, LETTERS, letters), 20, replace=TRUE), collapse="")
