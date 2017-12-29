#' @export
mongoConnect <- function() {
	mongo.create(
		host     = Sys.getenv("MONGO_CNN_HOST"),
		username = Sys.getenv("MONGO_CNN_USER"),
		password = Sys.getenv("MONGO_CNN_PASSWORD"),
		db       = Sys.getenv("MONGO_CNN_DB"))
}

#' @export
mySQLConnect <- function() {
	dbConnect(MySQL(),
		host     = Sys.getenv("MYSQL_CNN_HOST"), 
		username = Sys.getenv("MYSQL_CNN_USER"),
		password = Sys.getenv("MYSQL_CNN_PASSWORD"), 
		dbname   = Sys.getenv("MYSQL_CNN_DB"))
}