#' @export
CNN <- "http://www.cnn.com"

#' @export
SOURCES <- list( c(name = "Africa",      selector = "#africa-zone-2 a, #africa-zone-1 a"),
				 c(name = "Americas",    selector = "#americas-zone-2 a, #americas-zone-1 a"),
				 c(name = "Asia",        selector = "#asia-zone-2 a, #asia-zone-1 a"),
				 c(name = "Europe",      selector = "#europe-zone-2 a, #europe-zone-1 a"),
				 c(name = "Middle-East", selector = "#middleeast-zone-2 a, #middleeast-zone-1 a"),
				 c(name = "US",          selector = "#us-zone-1 a, #us-zone-2 a, #us-zone-3 a"))

#' @export
scrapeStories <- function(source) {
	stories <- paste(CNN, source["name"], sep = "/") %>%
		read_html() %>%
		html_nodes(source["selector"])

	stories_d <- data_frame(Title  = html_text(stories, TRUE),
							href   = html_attr(stories, "href"),
							Source = source["name"]) %>%
		filter(nchar(Title) > 0, grepl("*/index\\.html$", href), !grepl("^(http|ftp)s?://", href))
	stopifnot(nrow(stories_d) >= 5)
	return(stories_d)
}

#' @export
scrapeArticle <- function(dta) {
	url <- paste0(CNN, unique(dta$href))
	doc <- read_html(url)
	lt  <- list(
		content  = html_nodes(doc, ".el__leafmedia--sourced-paragraph .zn-body__paragraph , div.zn-body__paragraph") %>%
						html_text(TRUE) %>%
						paste(collapse = " ") %>%
						sub("^\\(CNN\\)", "", .),
		author   = html_nodes(doc, ".metadata__byline__author a") %>%
						html_text(TRUE),
		title    = html_nodes(doc, ".pg-headline") %>%
						html_text(TRUE),
		subtitle = html_nodes(doc, "#body-text h3") %>%
						html_text(TRUE) %>%
						grep("^JUST WATCHED$|^Story highlights$", ., value = TRUE, invert = TRUE) %>%
						unique(),
		summary  = html_nodes(doc, ".el__storyhighlights--normal") %>%
						html_text(TRUE) %>%
						paste(collapse = " "),
		links    = html_nodes(doc, "#body-text a") %>%
						html_attr("href") %>%
						.[!xml2:::is_url(.) & grepl("index\\.html$", .)] %>%
						unique(),
		date     = html_nodes(doc, ".update-time") %>%
						html_text(TRUE) %>%
						sub("^.*?, ", "", .) %>%
						strptime("%a %B %d, %Y", tz = "America/New_York"),
		url      = url,
		source   = first(dta$Source))
	lt$alternate_titles <- setdiff( unique(dta$Title), lt$title)
	rm(doc)
	return(lt)
}

#' @export
parseLocations <- function(articles) {
	location_key$Keywords <- location_key$Keywords %>%
		tolower() %>%
		gsub("(?!-)[[:punct:]]+", "", ., perl = TRUE) %>%
		gsub(" ", "_", ., perl = TRUE)
	names(articles) <- sapply(articles, "[[", "url")
	all_text        <- sapply(articles, function(x) { paste(x$content, x$title, x$summary, collapse = " ") })
	it              <- itoken(all_text, removePunctuation, word_tokenizer)
	vocab           <- create_vocabulary(it, c(1L, 3L), stopwords, sep_ngram = "_")
	dtm             <- create_dtm(it, vocab_vectorizer(vocab))
	idx             <- which(dimnames(dtm)[[2]] %in% tolower(location_key$Keywords))
	dtm             <- as.matrix(dtm[, idx])
	locations       <- adply(dtm, 1, function(x) { data_frame(Freq = x[x>0], Keywords = names(x[x>0])) }, .id = "url") %>%
		left_join(location_key, "Keywords") %>%
		dlply("url", findCenter)
	no_key_url      <- setdiff(names(articles), names(locations))
	no_key_list     <- list(center = NA, location = list(type = "Point", coordinates = c(0, 0))) %>% 
		list() %>% 
		rep(length(no_key_url)) %>% 
		setNames(no_key_url)
	locations       <- append(locations, no_key_list)[names(articles)]

	stopifnot(all.equal(names(articles), names(locations)))
	articles        <- Map(c, articles, locations)
	return(articles)
}

findCenter <- function(locs) {
	center <- locs %>%
		group_by(Parent, Location, pop, lat, lon) %>%
		summarize(N = sum(Freq)) %>%
		ungroup() %>%
		filter(N == max(N))
	if (nrow(center) > 1) {
		# If 2 centers are mentioned the same amount of times
		center <- centerTieBreak(center, locs)
	} else if (is.na(center$Parent) & center$Location %in% locs$Parent) {
		# If the center is a country and has a child city
		center <- filter(locs, Parent == center$Location) %>% filter(pop == max(pop)) %>% head(1)
	}
	# popup <- a(href = center$url, paste0(strong(center$title), br(), center$summary)) %>% as.character()
	center_output <- list(center = center$Location,
		# In the structure required for mongoDB's 2dsphere index.
		location = list(type = "Point", coordinates = c(center$lon, center$lat))
	)
	return(center_output)
}

centerTieBreak <- function(centers, locs) {
	if (all(is.na(centers$Parent))) {
		# If all centers are countries
		centers <- filter(locs, Parent %in% centers$Location) %>% filter(Freq == max(Freq))
		if (nrow(centers) == 0) {
			message("Ambiguous location for article: ", first(locs$url))
			return(data.frame(Location = NA, lon = 0, lat = 0))
		} else {
			centers <- cityTieBreak(centers, locs)
		}
	} else if (all(!is.na(centers$Parent))) {
		# If all centers are cities
		centers <- cityTieBreak(centers, locs)
	} else {
		# If some centers are cities, some are countries
		centers <- filter(centers, !is.na(Parent))
		if (nrow(centers) > 1) {
			centers <- cityTieBreak(centers, locs)
		}
	}
	return(centers)
}

cityTieBreak <- function(centers, locs) {
	locs$Parent[is.na(locs$Parent)] <- locs$Location[is.na(locs$Parent)]
	parent <- locs %>% filter(Parent %in% centers$Parent) %>%
		group_by(Parent) %>%
		summarize(N = sum(Freq)) %>%
		filter(N == max(N))
	center <- filter(centers, Parent %in% parent$Parent)
	if (nrow(center) > 1) {
		# If each center's parents are mentioned an equal amount of times (ie. nrow(parent) > 1)
		# or if both centers have the same parent (ie. nrow(parent) == 1 & nrow(center) > 1)
		center <- filter(centers, Parent %in% parent$Parent) %>% filter(pop == max(pop))
	}
	return(center)
}

#' @export
`%nin%`      <- Negate(`%in%`)
