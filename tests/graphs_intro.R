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
      type =  'MC',
      known.parameters =  "",
      question = 'Which of the following words is least likely to be a stop word:',
      choices = list(
            'and',
            'potato',        
            'this',
            'every'),
      correct_answer = '2'
    )

)



yaml.out(questions[[1]], "x = 1")
yaml.out(questions[[1]], "x = 2")
yaml.out(questions[[1]], "x = 3")
yaml.out(questions[[1]], "x = 4")


