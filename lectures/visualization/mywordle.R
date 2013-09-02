mywordle <- function(words, freq, min.freq=10, pause=FALSE) {
  keep <- (freq >= min.freq)
  words <- words[keep]
  freq <- freq[keep]
  
  ord <- order(freq, decreasing=TRUE)
  freq <- freq[ord]
  words <- words[ord]
  
  plot.new()
  op <- par(mar=rep(0,4))
  plot.window(c(-1,1),c(-1,1), asp=1)
  
  smin <- 0.5
  smax <- 4
  sizes <- smin + (smax-smin) *(freq-min(freq))/(max(freq)-min(freq))
  
  thetaStep <- 0.1
  rStep <- 0.05*thetaStep/(2*pi)
  boxes <- list()
  
  box <- function(r, theta, word, size) {
    wid <- strwidth(word, cex=size)
    ht <- strheight(word, cex=size)
    x <- r*cos(theta)
    y <- r*sin(theta)
    return(c(x-wid/2, x+wid/2, y-ht/2, y+ht/2))
  }
  
  is.overlapped <- function(box, boxes) {
    if(length(boxes)==0) return(FALSE)
    for(i in 1:length(boxes)) {
      boxi <- boxes[[i]]
      if(boxi[1]>box[2]  || boxi[2]<box[1] || boxi[3]>box[4] || boxi[4] < box[3]) next
      else return(TRUE)
    }
    return(FALSE)
  }
  r <- 0
  theta <- 0
  for(i in 1:length(words)) {
    repeat {
      b<-box(r, theta, words[i], sizes[i])
      if(!is.overlapped(b, boxes)) break
      theta <- theta + thetaStep
      r <- r + rStep
    }
    text(r*cos(theta),r*sin(theta), words[i], adj=c(0.5,0.5), cex=sizes[i])
    if(pause) ani.pause(1)
    boxes <- c(list(b), boxes)
  }
  par(op)
 invisible() 
}