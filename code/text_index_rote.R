#!/usr/bin/R
# The automatic document query problem generator
# To run this script, open R and type source("text_index_rote.R")

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

IDF = function(docTable) {
    ft = colSums(docTable > 0)
    N = nrow(docTable)
    return(log(N/ft))
}

TF = function(docTable) {
    return(log(docTable + 1))
}

TFIDF = function(docTable, idf) {
    tf = TF(docTable)
    tfIdf = tf %*% diag(idf)
    return(tfIdf)
}

cosineSimilarity = function(X, y) {
    dnorms = sqrt(rowSums(X^2))
    qnorm = sqrt(sum(y^2))

    X = diag(1/dnorms) %*% X
    y = y/qnorm
    return(X %*% t(y))
}


generateProblem = function(
## set parameters
    M = 5, # number of words
    N = 4, # number of documents
    avgDocLen = 5, # average number of words per document
    Q = 2 # number of query terms
    ) {
    
    terms = LETTERS[1:M]
    prob = 1/(1:M)

    ## generate documents
    documents = replicate(N, generateDocument(terms, prob, avgDocLen))
    selectedTerms = sort(unique(unlist(documents)))
    docTable = tabulateDocuments(documents, selectedTerms)

    ## generate query
    queryPos = sample(length(selectedTerms), Q)
    query = selectedTerms[queryPos]
    queryTable = table(factor(selectedTerms[queryPos], levels = selectedTerms))

    return(list(docTable = docTable, queryTable = queryTable, documents = documents, query = query))
}

printProblem = function(problem) {
    N = length(problem$documents)
    cat("Given the following ", N, " documents:\n", sep="")
    printDocuments(problem$documents)
    cat("and query ", printDocument(problem$query), ", compute the document scores using TF-IDF weighting and\nCosine similarity.\nRun computeDocumentScores(problem) to see the solution.\n", sep="")
}

computeDocumentScores = function(problem) {
    ## compute solution
    idf = IDF(problem$docTable)
    tfIdf = TFIDF(problem$docTable, idf)
    qfIdf = TFIDF(problem$queryTable, idf)
    scores = cosineSimilarity(tfIdf,qfIdf)
    return(list(IDF = idf, TFIDF = tfIdf, queryTFIDF = qfIdf, scores = scores))
}

problem = generateProblem(
    M = 5, # number of words
    N = 4, # number of documents
    avgDocLen = 5, # average number of words per document
    Q = 2 # number of query terms
    )


printProblem(problem)
#computeDocumentScores(problem)
