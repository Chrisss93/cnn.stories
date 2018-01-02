#' @include iterator.R

#' @title Label LDA topics
#' 
#' @description Use vector algebra on word embeddings of most relevant words in topics to derive single topic label
# On a bit of shaky ground here since by definition, LDA treats topics as mixtures of words, and we are selecting
# a single one. Hopefully the syntatic and semantic relationships of word embeddings produce a single label that 
# closely represents the most relevant words in the topic, both in vector-space and meaning
#'
#' @param lambda A numeric from 0 to 1. This determines the relevancy score
#' @param lda A pre-trained \link[text2vec]{LatentDirichletAllocation} model.
#' @param word_vector A numeric matrix. Column-bound word vectors.
#'
#' @return A character vector
getTopicLabel <- function(lambda = 0.3, lda, word_vector) {
	topic_word_score <- lambda * log(lda$topic_word_distribution) +
		(1 - lambda) * log(t(t(lda$topic_word_distribution) / (colSums(lda$components) / sum(lda$components)) ))

	apply(topic_word_score, 1, function(topic) {
		relevant_words <- sort( minMaxScale(topic), decreasing = TRUE, method = "radix")[seq_len(10)]
		relevant_words <- relevant_words[intersect(names(relevant_words), row.names(word_vector))] # why do i need this?
		sum_vec <- t(relevant_words) %*% word_vector[names(relevant_words), ]
		cosine  <- sim2(word_vector, sum_vec)
		closest <- names(cosine[which.max(cosine), ])
		if (closest != names(relevant_words)[1]) {
			closest <- paste(closest, names(relevant_words)[1], sep = "/")
		}
		return(closest)
	})
}

#' @title Distribute topics from LDA models onto new corpora
#'
#' @description Using an iterator from new corpora, a document-term matrix compatible with existing LDA models is
#' created. The \code{transform} method from the \link[text2vec]{LatentDirichletAllocation} handles the rest
#'
#' @description The iterator from new corpora must be transformed to take into account collocations present in the
#' LDA models. Moreover, any terms present in the new corpora's document-term matrix, not present in the LDA models
#' must be dropped.
#'
#' @param articles Used to create corpora.
#' @param mongo A \link[rmongodb]{mongo} connection.
#'
#' @return A list
#' @export
distributeTopics <- function(articles, mongo) {
	models       <- loadLDA(mongo)
	model_words  <- colnames(models[[1]]$topic_word_distribution)
	collocations <- grep("_", model_words, value = TRUE)
	
	all_text  <- sapply(articles, function(x) { paste(x$title, x$subtitle, x$summary, x$content, collapse = " ") })
	names(all_text) <- sapply(articles, "[[", "url")
	it        <- itoken(all_text, removePunctuation)
	it_class  <- class(it)
	# Add model collocations to articles
	it        <- text2vec:::itoken_transformer_R6$new(it$clone(deep = TRUE), function(y) {
			text2vec:::collapse_collocations_cpp(y$tokens, text2vec:::create_xptr_unordered_set(collocations), "_")	
	})
	setattr(it, "class", c("itoken", class(it)))
	dtm           <- create_dtm(it, vocab_vectorizer(create_vocabulary(it, stopwords = stopwords)), "dgCMatrix")
	missing_terms <- setdiff(model_words, colnames(dtm))

	m   <- Matrix(0, nrow = nrow(dtm), ncol = length(missing_terms), 
		dimnames = list(NULL, missing_terms), sparse = TRUE)	
	dtm <- cbind(dtm, m)[, model_words]
	
	for (model in models) {
		topic <- model$transform(dtm)
		colnames(topic) <- colnames(model$.__enclos_env__$private$doc_topic_matrix)
		for (i in seq_along(articles)) {
			a <- articles[[i]]
			articles[[i]]$topics <- modifyList(
				as.list(a$topics), 
				setNames( list(round(topic[a$url, ], 3)), nrow(model$components) )
			)
		}
	}
	return(articles)
}

#' @title Get LDA models
#'
#' @description Retrieves LDA models saved in Mongo's GridFS system.
#'
#' @param mongo A \link[rmongodb]{mongo} collection.
#' @param n A numeric vector. Controls which particular models should be retrieved. (ie. 2-topic model, 10-topic model)
#'
#' @return a list of \link[text2vec]{LatentDirichletAllocation} objects
loadLDA <- function(mongo, n = 2:15) {
	if (!mongo.is.connected(mongo)) {
		mongo.reconnect(mongo)
	}
	models <- list()
	gridfs <- mongo.gridfs.create(mongo, Sys.getenv("MONGO_CNN_GRID"))
	tmp <- tempfile()
	for (i in n) {
		gf <- mongo.gridfs.find(gridfs, paste0("topic_", i))
		if (is.null(gf)) { stop(paste0(i, "-topic model does not exist")) }
		conn <- file(tmp)
		mongo.gridfile.pipe(gf, conn)
		mongo.gridfile.destroy(gf)
		models[[as.character(i)]] <- readRDS(tmp)
		file.remove(tmp)
	}
	return(models)
}

#' @export
trainLDA <- function(iterator) {
	on.exit( mongo.destroy(mongo) )
	
	if (missing(iterator)) {
		iterator <- createIterator()
	}
	mongo    <- mongoConnect()
	gridfs   <- mongo.gridfs.create(mongo, Sys.getenv("MONGO_CNN_GRID"))
	dtm      <- create_dtm(iterator[["it"]], vocab_vectorizer(iterator[["vocab"]]), "dgCMatrix")
	cat("Document-term matrix created.\n")
	
	word_vector <- loadGloveEmbeddings()
	topic_list  <- list()
	tmp         <- tempfile()
	for (i in 2:15) {
		lda_model   <- LDA$new(n_topics = i)
		topic_distr <- lda_model$fit_transform(dtm)
		topic_label <- getTopicLabel(0.3, lda_model, word_vector)
		colnames(topic_distr) <- topic_label
		colnames(lda_model$.__enclos_env__$private$doc_topic_matrix) <- topic_label
		topic_list[[as.character(i)]] <- topic_distr

		saveRDS(lda_model, tmp)
		mongo.gridfs.store.file(gridfs, tmp, paste0("topic_", i))
		file.remove(tmp)
	}
	cat("LDA models trained and saved.\n")
	mongo.gridfs.destroy(gridfs)
	
	ns <- paste(Sys.getenv("MONGO_CNN_DB"), Sys.getenv("MONGO_CNN_COLLECTION"), sep = ".")
	for (id in row.names(dtm)) {
		topic_n <- lapply(topic_list, function(x) { round(x[id,], 3) })
		bson <- list("$set" = list("topics" = topic_n))
		mongo.update(mongo, ns, list(url = id), bson)
	}
	cat("Document-topic distributions for training corpora saved as 'topics' field in collections.\n")
}

minMaxScale <- function(x) {
	r <- range(x, finite = TRUE)
	(x - r[1])/ diff(r)
}