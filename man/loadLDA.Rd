% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topics.R
\name{loadLDA}
\alias{loadLDA}
\title{Get LDA models}
\usage{
loadLDA(mongo, n = 2:15)
}
\arguments{
\item{mongo}{A \link[rmongodb]{mongo} collection.}

\item{n}{A numeric vector. Controls which particular models should be retrieved. (ie. 2-topic model, 10-topic model)}
}
\value{
a list of \link[text2vec]{LatentDirichletAllocation} objects
}
\description{
Retrieves LDA models saved in Mongo's GridFS system.
}
