#' @include iterator.R

#' @export
loadGloveEmbeddings <- function() {
	on.exit( try(dbDisconnect(mysql), TRUE) )

	mysql <- mySQLConnect()
	word_vector <- dbGetQuery(mysql, paste("SELECT * FROM", Sys.getenv("MYSQL_CNN_TBL")))
	dbDisconnect(mysql)
	row.names(word_vector) <- word_vector[["rn"]]
	word_vector[["rn"]] <- NULL
	word_vector <- as.matrix(word_vector)
	return(word_vector)
}

#' @export
trainGloveEmbeddings <- function(iterator, learning_rate = 0.15) {
	# RcppParallel::setThreadOptions(numThreads = CORES)
	if (missing(iterator)) {
		iterator <- createIterator()
	}
	tcm <- create_tcm(iterator[["it"]], vocab_vectorizer(iterator[["vocab"]]), skip_grams_window = 5)
	cat("Term co-occurence matrix created.\n")
	while(learning_rate > 0.05) {
		cat("Trying", learning_rate * 100, "% learning-rate.\n")
		gv       <- GloVe$new(word_vectors_size = 600, vocabulary = iterator[["vocab"]], 10, learning_rate = learning_rate)
		wv_main  <- tryCatch( gv$fit_transform(tcm, n_iter = 500, convergence_tol = 0.01), warning = function(w) w )
		learning_rate <- learning_rate - 0.01
		if (!("warning" %in% class(wv_main))) { break }
	}
	cat("Global Vector word embeddings created.\n")
	rm(tcm)
	saveGloveEmbeddings( wv_main + t(gv$components) )
}

saveGloveEmbeddings <- function(w_vector) {
	on.exit( dbDisconnect(mysql) )
	
	tmp      <- tempfile(fileext = ".csv")
	w_vector <- as.data.table(w_vector, keep.rownames = TRUE)
	fwrite(w_vector, tmp, sep = "\t", nThread = CORES)
	rm(w_vector)
	
	mysql <- mySQLConnect()
	sql_cmd <- paste("LOAD DATA LOCAL INFILE", sQuote(tmp), 
					"REPLACE INTO TABLE", Sys.getenv("MYSQL_CNN_TBL"), 
					"IGNORE 1 LINES;")
	dbGetQuery(mysql, paste("TRUNCATE", Sys.getenv("MYSQL_CNN_TBL")))
	dbGetQuery(mysql, sql_cmd)
	dbDisconnect(mysql)
	file.remove(tmp)
	cat("Word embeddings saved to the database.\n")
}