## Make sure image URL directory is not readable, only executable!

#cat('`r image_path = "graphs_intro"; image_url = "http://www.scem.uws.edu.au/~lapark/300958/images/graphs_intro"`')

questions = list(
    list(
      type =  'MC',
      known.parameters =  'D choices correct `r require("xtable");`',
      question = '<p>Which of the following two clusters will we obtain after performing single linkage clustering on a data set with distance matrix:</p> `r gsub("\\n","",c(print(xtable(D, digits=0), type="html", comment=FALSE)))`',
      choices = list(
        '`r choices[1]`',
        '`r choices[2]`',
        '`r choices[3]`',
        '`r choices[4]`'),
      correct_answer = '`r correct`'
    ),
    list(
      type =  'MC',
      known.parameters =  'X centres choices correct `r require("xtable");`',
      question = '<p>Using the data set:</p> `r gsub("\\n","",c(print(xtable(X, digits=0), type="html", include.colnames=FALSE, comment=FALSE)))` <p>The K-means clustering algorithm has provided us with the cluster centres:</p> `r gsub("\\n","",c(print(xtable(centres, digits=2), type="html", include.colnames=FALSE, comment=FALSE)))` <p>Which data points belong to cluster 1?',
      choices = list(
        '`r choices[1]`',
        '`r choices[2]`',
        '`r choices[3]`',
        '`r choices[4]`'),
      correct_answer = '`r correct`'
    ),
    list(
      type =  'MC',
      known.parameters =  'SSW choices correct `r require("xtable");`',
      question = '<p>Using a data set, we obtained the following within sum of sqaures (SSW) values and cluster numbers:</p> `r gsub("\\n","",c(print(xtable(SSW, digits=1), type="html", comment=FALSE, include.colnames=FALSE)))` <p>What is the most appropriate number of clusters for this data set?</p>',
      choices = list(
        '`r choices[1]`',
        '`r choices[2]`',
        '`r choices[3]`',
        '`r choices[4]`'),
      correct_answer = '`r correct`'
    ),
    list(
      type =  'NUM',
      known.parameters =  'X `r require("xtable")',
      question = '`r .mean = t(as.matrix(apply(X,2,mean))); X.dist = t(as.matrix(sqrt(apply((X - X.mean[rep(1,nrow(X)),])^2,1,sum)))); rownames(X.dist) = "Distance to mean"; X.dist = round(X.dist*100)/100; k = kmeans(X,2); cluster.dist = t(as.matrix(sqrt(apply((X - k$centers[k$cluster,])^2,1,sum)))); rownames(cluster.dist) = "Distance to cluster centre"; cluster.dist = round(cluster.dist*100)/100;` <p>In a given data set containing `r nrow(X)` points, the distance of each point to the mean is:</p> `r gsub("\\n","",c(print(xtable(X.dist, digits=2), type="html", comment=FALSE, include.colnames=FALSE)))` <p>and the distance of each point to its cluster centre is:</p> `r gsub("\\n","",c(print(xtable(cluster.dist, digits=2), type="html", comment=FALSE, include.colnames=FALSE)))` <p>Compute the within cluster sum of squares (SSW).</p>',
      answer = '`r SSW = sum(cluster.dist^2); SSW`',
      tolerance = "0.1"
    )
  )




yaml.out(questions[[1]], 'D = matrix(c(0, 6, 4, 6, 6, 0, 5, 1, 4, 5, 0, 6, 6, 1, 6, 0),4,4); rownames(D) = c("A","B","C","D"); colnames(D) = c("A","B","C","D"); choices = c("Cluster 1: A,B, Cluster 2: C,D","Cluster 1: A,C, Cluster 2: B,D","Cluster 1: A,B,C Cluster 2: D","Cluster 1: A,D, Cluster 2: B,C"); correct = 2')

yaml.out(questions[[2]], 'X = matrix(c(3,5,5,1,9,2),3,2); rownames(X) = c("A","B","C"); centres = matrix(c(4,5,1.5,9),2,2); rownames(centres) = c("1","2"); choices = c("A, B and C", "A and B", "A and C", "B and C"); correct = 3')

yaml.out(questions[[3]], 'SSW = t(matrix(c(1,2,3,4,5,6,94.1,12.2,10.5,8.8,7.3,6.8),6,2)); rownames(SSW) = c("Clusters","SSW"); choices = c("1","2","3","4"); correct = 2')

yaml.out(questions[[4]], 'X = matrix(runif(8),4,2)*10')








