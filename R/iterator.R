createIterator <- function() {
	on.exit( mongo.destroy(mongo) )

	mongo    <- mongoConnect()
	ns       <- paste(Sys.getenv("MONGO_CNN_DB"), Sys.getenv("MONGO_CNN_COLLECTION"), sep = ".")
	articles <- mongo.find.all(mongo, ns, 
		fields = list("_id" = 0L, url = 1L, content = 1L, title = 1L, subtitle = 1L, summary = 1L))
	mongo.disconnect(mongo)
	
	# Read in all article text
	all_text <- sapply(articles, function(x) { paste(x$title, x$subtitle, x$summary, x$content, collapse = " ") })
	names(all_text) <- sapply(articles, "[[", "url")
	rm(articles)
	cat("Articles retrieved.\n")

	# Create iterator and vocabulary with predictable pre/post-processing
	it        <- itoken_parallel(all_text, removePunctuation, n_chunks = CHUNKS)
	vocab     <- it %>% 
		create_vocabulary(stopwords = stopwords) %>% 
		prune_vocabulary(doc_proportion_min = 0.005)
	vocab <- vocab[-grep("^[[:punct:]]+$", vocab[["term"]], perl = TRUE), ]
	rm(all_text)
	cat("Vocabulary initialized.\n")

	# Collapse statistically significant collocating bi-grams into uni-grams
	phrases <- Collocations$new(vocab, conf.level = 0.99, lfmd_min = -19)
	phrases$fit(it)
	cat("Collocations identified.\n")
	# Re-create iterator and vocabulary with new collocating uni-grams
	it <- phrases$transform(it)
	vocab <- it %>% 
		create_vocabulary(stopwords = stopwords) %>% 
		prune_vocabulary(doc_proportion_min = 0.005)
	vocab <- vocab[-grepl("^[[:punct:]]+$", vocab[["term"]], perl = TRUE), ]
	cat("Vocabulary updated.\n")
	
	return( list(it = it, vocab = vocab) )
}

#' @export
removePunctuation <- function(x) {
	x <- gsub("(?![_-])[[:punct:]]+", "", tolower(x), perl = TRUE)
	x <- gsub("cnn|'s", "", x, perl = TRUE)
	return(x)
}