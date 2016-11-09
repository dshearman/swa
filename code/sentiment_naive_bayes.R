#!/usr/bin/R
# The automatic document query problem generator
# To run this script, open R and type source("sentiment_naive_bayes.R")

generateDocument = function(terms, prob, avgDocLen) {
    prob = sample(prob)
    nd = rpois(1, lambda = avgDocLen - 1) + 1
    d = sample(terms, nd, replace = TRUE, prob = prob)
    return(d)
}

tabulateDocument = function(document, terms) {
    table(factor(document, levels = terms))
}

tabulateDocuments = function(documents, terms) {
    t(sapply(documents, tabulateDocument, terms))
}

printDocument = function(document) {
    paste(document, collapse = " ")
}

printDocuments = function(documents) {
    D = lapply(documents, printDocument)
    for (d in D) {
        cat(d, "\n")
    }
}

generateProblem = function(
## set parameters
    M, # number of words
    N, # number of documents
    avgDocLen, # average number of words per document
    Q # number of query terms
    ) {
    
    terms = LETTERS[1:M]
    probPos = 1/(1:M)
    probNeg = 1/(M:1)

    N1 = sample(2:4, 1)
    N2 = sample(2:4, 1)
    
    ## generate documents
    documentsPos = replicate(N1, generateDocument(terms, probPos, avgDocLen))
    documentsNeg = replicate(N2, generateDocument(terms, probNeg, avgDocLen))

    selectedTermsPos = sort(unique(unlist(documentsPos)))
    selectedTermsNeg = sort(unique(unlist(documentsNeg)))
    selectedTermsSub = intersect(selectedTermsPos, selectedTermsNeg)
    selectedTerms = union(selectedTermsPos, selectedTermsNeg)

    docTablePos = tabulateDocuments(documentsPos, selectedTerms)
    docTableNeg = tabulateDocuments(documentsNeg, selectedTerms)

    ## generate query
    queryPos = sample(length(selectedTermsSub), Q, replace = TRUE)
    query = selectedTermsSub[queryPos]
    queryTable = table(factor(query, levels = selectedTerms))

    return(list(docTablePos = docTablePos, docTableNeg = docTableNeg, queryTable = queryTable, documentsPos = documentsPos, documentsNeg = documentsNeg, query = query))
}

printProblem = function(problem) {

    NP = length(problem$documentsPos)
    NN = length(problem$documentsNeg)
    
    cat("Given the following ", NP, " positive sentiment tweets:\n", sep = "")
    printDocuments(problem$documentsPos)
    cat("And the following ", NN, " negative sentiment tweets:\n", sep = "")
    printDocuments(problem$documentsNeg)
    cat("Compute the sentiment of the tweet ", printDocument(problem$query), ".\nRun printSolution(solution) to see the solution.\n", sep = "")
}

problem = generateProblem(
    M = 5, # number of words
    N = 4, # number of documents
    avgDocLen = 5, # average number of words per document
    Q = 3 # number of query terms
    )


printProblem(problem)
#computeDocumentScores(problem)

computeSentiment = function(problem) {

    colpos = as.numeric(factor(problem$query, levels = colnames(problem$docTablePos)))
    pw.sp = colSums(problem$docTablePos[,colpos] > 0)/nrow(problem$docTablePos)

    colpos = as.numeric(factor(problem$query, levels = colnames(problem$docTableNeg)))
    pw.sn = colSums(problem$docTableNeg[,colpos] > 0)/nrow(problem$docTableNeg)

    psp = nrow(problem$docTablePos)/(nrow(problem$docTablePos) + nrow(problem$docTableNeg))
    psn = 1 - psp

    pt.sp = prod(pw.sp)
    pt.sn = prod(pw.sn)

    pt = pt.sp*psp + pt.sn*psn

    psp.t = pt.sp*psp/pt
    psn.t = pt.sn*psn/pt

    score = log(psp.t/psn.t)

    return(list(word.given.posSent = pw.sp, word.given.negSent = pw.sn, posSent = psp, negSent = psn, tweet.given.posSent = pt.sp, tweet.given.negSent = pt.sn, tweet = pt, score = score))
}

solution = computeSentiment(problem)

printSolution = function(solution) {
    cat("P(term|sent = Pos) = \n"); print(solution$word.given.posSent)
    cat("P(term|sent = Neg) = \n"); print(solution$word.given.negSent)
    cat("P(sent = Pos) = ", solution$posSent, "\n",
        "P(sent = Neg) = ", solution$negSent, "\n",
        "P(tweet|sent = Pos) = ", solution$tweet.given.posSent, "\n",
        "P(tweet|sent = Neg) = ", solution$tweet.given.negSent, "\n",
        "P(tweet) = ", solution$tweet, "\n",
        "Sentiment score = ", solution$score, "\n", sep = "")
}


#printSolution(solution)
