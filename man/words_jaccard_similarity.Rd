% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/address_similarity_func.R
\name{words_jaccard_similarity}
\alias{words_jaccard_similarity}
\alias{words_cos_similarity}
\title{\code{words_jaccard_similarity} is for measuring jaccard similarity/dissimilarity in documents
\code{words_cos_similarity} is for measuring cos similarity/dissimilarity in documents}
\usage{
words_jaccard_similarity(words1, words2, engine = NULL,
  quick_seg = TRUE)

words_cos_similarity(words1, words2, engine = NULL, quick_seg = TRUE)
}
\arguments{
\item{words1}{Sentence 1.}

\item{words2}{Sentence 2.}

\item{engine}{Words segment engine of jiebar packages.}

\item{quick_seg}{Logical,quick segmentation of sentence.Default is TRUE.}
}
\value{
A number of similarity
}
\description{
\code{words_jaccard_similarity} is for measuring jaccard similarity/dissimilarity in documents
\code{words_cos_similarity} is for measuring cos similarity/dissimilarity in documents
}
\examples{
data(dict_add_cn)
words1 = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][4],1,3))
words2 = paste(substr(dict_add_cn[[1]][1],1,2),substr(dict_add_cn[[1]][2],1,2),
substr(dict_add_cn[[1]][3],1,3),substr(dict_add_cn[[1]][5],1,3))
words_jaccard_similarity(words1,words2)
words_cos_similarity(words1,words2)
}
