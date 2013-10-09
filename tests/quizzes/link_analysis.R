## Make sure image URL directory is not readable, only executable!
cat('`r require("igraph"); library("Matrix"); library("methods"); library("lattice"); library("xtable"); image_path = "link_analysis"; image_url = "http://www.scem.uws.edu.au/~lapark/300958/images/link_analysis"`')



questions = list(
    list(
      type =  'NUM',
      known.parameters =  'file_name g ',
      question = '`r png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off()` <p>Using the following graph, if we start at vertex 1 and take a random walk of length 10,000, what is the probability of us finishing the walk at vertex 1 (given at two decimal places)?</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r degree(g)[1]/sum(degree(g))`',
      tolerance = "0.01"
    ),
    list(
      type =  'NUM',
      known.parameters =  'file_name g',
      question = '`r png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off();  A = t(get.adjacency(g)); n = length(V(g)); D = diag(1/apply(A,2,sum)); T = A %*% D; E = matrix(1/n,n,n); P = alpha*T + (1-alpha)*E;` <p>Using the Random Surfer probability transition matrix of the following graph, with &alpha; = `r alpha`, what is the probability of beginning at vertex 1 and moving to vertex 2 in a random walk of length 1 (given at two decimal places)?</p><img src="`r paste(image_url, file_name, sep="/")`">',
      answer = '`r P[2,1]`',
      tolerance = "0.01"
    ),
    list(
      type =  'MC',
      known.parameters =  'file_name g choices correct',
      question = '`r png(paste(image_path, file_name, sep="/")); par(mar = c(0,0,0,0)); V(g)$label.cex = 1.8; plot(g, layout=layout.fruchterman.reingold, vertex.size = 15); dev.off();` <p>Which of the following edges do we need to add to make the following graph ergodic?</p><img src="`r paste(image_url, file_name, sep="/")`">',
      choices = list(
        '`r choices[1]`',
        '`r choices[2]`',
        '`r choices[3]`',
        '`r choices[4]`'),
      correct_answer = '`r correct`'
    ),
    list(
      type =  'MC',
      known.parameters =  'T choices correct',
      question = '<p>Given the following probability transition matrix</p> `r gsub("\\n","",c(print(xtable(T, digits=2), type="html", comment=FALSE, include.colnames=FALSE, include.rownames=FALSE)))` <p>the stationary distribution (rounded to two deciamal places) is:</p>',
      choices = list(
        '`r choices[1]`',
        '`r choices[2]`',
        '`r choices[3]`',
        '`r choices[4]`'),
      correct_answer = '`r correct`'
    )


)



yaml.out(questions[[1]], 'file_name = "geigai4duG9eoquiePee.png"; g = graph.formula(1-2, 2-3, 1-3, 2-4, 2-5); ')
yaml.out(questions[[1]], 'file_name = "aeM5ohee2yuCeef7KeXu.png"; g = graph.formula(1-2, 2-3, 3-4, 4-5, 3-5); ')
yaml.out(questions[[1]], 'file_name = "xe3uw3Ootah2ohj9eiwi.png"; g = graph.formula(1-2, 1-3, 2-3, 3-4, 2-5, 3-5); ')
yaml.out(questions[[1]], 'file_name = "Rie5Ua8uqu9Ooga2usha.png"; g = graph.formula(1-2, 2-3, 1-3, 1-4, 1-5, 4-5, 2-4, 5-3); ')


yaml.out(questions[[2]], 'file_name = "ae9nohwuoCeeluijudie.png"; g = graph.formula(1-+2, 2-+3, 3-+4, 2-+1, 2-+4); alpha = 0.8')
yaml.out(questions[[2]], 'file_name = "ahfoov0Ro8weij3zai0e.png"; g = graph.formula(1-+2, 1-+3, 2-+3, 3-+4); alpha = 0.8')
yaml.out(questions[[2]], 'file_name = "rae9eik2Xooz4meevuqu.png"; g = graph.formula(1-+2, 1-+3, 1-+4, 2-+3, 3-+4); alpha = 0.8')
yaml.out(questions[[2]], 'file_name = "zai6dake3Vioniepagh8.png"; g = graph.formula(1+-+2, 1-+3, 2-+4, 2-+3, 4-+1); alpha = 0.8')




yaml.out(questions[[3]], 'file_name = "die6Inooj5ooch0Weiph.png"; g = graph.formula(1+-+2, 3-+4, 1-+3); choices = c("4->1","3->2","2->4","2->1"); correct = 1')
yaml.out(questions[[3]], 'file_name = "voh3teipuph3Coowoh1T.png"; g = graph.formula(1-+2, 1-+3, 1-+4, 2-+3, 3-+4); choices = c("3->2","2->4","4->1","2->1"); correct = 3')
yaml.out(questions[[3]], 'file_name = "sup6feiHaegue0Noo5ae.png"; g = graph.formula(1+-2, 1+-3, 1+-+4, 4-+3); choices = c("2->3","3->2","1->3","2->4"); correct = 2')
yaml.out(questions[[3]], 'file_name = "jeideigh5Reghe1aeMae.png"; g = graph.formula(1-+2, 2-+3, 3-+4, 1-+4); choices = c("2->4","3->2","1->3","4->1"); correct = 4')




yaml.out(questions[[4]], 'T = matrix(c(0,0.2,0.8,0.1,0.1,0.8,0.9,0.1,0),3,3); choices = c("[0.41 0.14 0.44]", "[0.41 0.44 0.14]", "[0.14 0.44 0.41]", "[0.14 0.41 0.44]"); correct = 1')
yaml.out(questions[[4]], 'T = matrix(c(0,0.5,0.5,0.5,0.4,0.1,1,0,0),3,3); choices = c("[0.24 0.34 0.41]", "[0.34 0.41 0.24]", "[0.34 0.24 0.41]", "[0.41 0.34 0.24]"); correct = 4')
yaml.out(questions[[4]], 'T = matrix(c(0,0.3,0.7,0.5,0.4,0.1,1,0,0),3,3); pv = function(s) {paste("[", paste(s, collapse=" "), "]")}; e = eigen(T); s = round(e$vectors[,1]/sum(e$vectors[,1]), digits = 2); choices = c(pv(s[c(2,1,3)]), pv(s[c(1,2,3)]), pv(s[c(3,2,1)]), pv(s[c(3,1,2)])); correct = 2')
yaml.out(questions[[4]], 'T = matrix(c(0,0,1,0.5,0,0.5,0.8,0.2,0),3,3); pv = function(s) {paste("[", paste(s, collapse=" "), "]")}; e = eigen(T); s = round(e$vectors[,1]/sum(e$vectors[,1]), digits = 2); choices = c(pv(s[c(3,2,1)]), pv(s[c(1,2,3)]), pv(s[c(2,1,3)]), pv(s[c(3,1,2)])); correct = 2')

