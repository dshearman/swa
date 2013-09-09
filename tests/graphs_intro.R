## Make sure image URL directory is not readable, only executable!

cat('`r image_path = "graphs_intro"; image_url = "http://www.scem.uws.edu.au/~lapark/300958/images/graphs_intro"`')
  
questions = list(
    list(
      type =  'NUM',
      known.parameters =  'x `r require("igraph"); file_name = "eidee9chaejaiMohy1oh.png"; png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); g = graph.formula(1-2, 2-3, 3-4, 1-3, 1-4, 3-5, 4-5, 5-6, 6-7, 1-7, 5-8, 5-9, 8-9, 8-10, 8-11, 8-12, 11-2); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()`',
      question = '<p>In the following graph, how many vertices are there in the degree 1.5 egocentric graph centred on vertex `r x`?</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r d = degree(g); d[which(names(d) == x)] + 1`',
      tolerance = "0.1"
    ),
    list(
      type =  'NUM',
      known.parameters =  'x `r require("igraph"); file_name = "yooSee7apooX3ooph3ah.png"; png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5, 4-5, 5-6, 6-7, 7-2); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()`',
      question = '<p>In the following graph, compute the closeness centrality, using the method shown in the lecture, of vertex `r x`.</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r d = 1/closeness(g); d[which(names(d) == x)]`',
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
    )
)



yaml.out(questions[[1]], "x = 1")
yaml.out(questions[[1]], "x = 2")
yaml.out(questions[[1]], "x = 3")
yaml.out(questions[[1]], "x = 4")

yaml.out(questions[[2]], "x = 1")
yaml.out(questions[[2]], "x = 2")
yaml.out(questions[[2]], "x = 3")
yaml.out(questions[[2]], "x = 4")

yaml.out(questions[[3]], 'file_name = "deexohlieSahchei1lae.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')
yaml.out(questions[[3]], 'file_name = "shingeidosheephoor4S.png"; g = graph.formula(1-2, 2-3, 3-4, 4-5, 3-5); ')
yaml.out(questions[[3]], 'file_name = "wi7caak3iare1Oacheet.png"; g = graph.formula(1-2, 2-3, 3-4, 2-5, 3-5); ')
yaml.out(questions[[3]], 'file_name = "bo6queiqueeHeic9sho8.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5, 5-3); ')

yaml.out(questions[[4]], 'file_name = "Bieza2ug3uBahdeeGhei.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')
yaml.out(questions[[4]], 'file_name = "heih0iViungis5Ueng5g.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')
yaml.out(questions[[4]], 'file_name = "eigh7aeYeegh9EiMahsu.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')
yaml.out(questions[[4]], 'file_name = "kahx6seVeix9utohthae.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')


