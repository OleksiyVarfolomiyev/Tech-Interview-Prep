# R Script for
# CONNECTING TO DATABASE
library(RMySQL)
dbCredentialsFile <- read.csv("database_credentials.csv", stringsAsFactors = FALSE, header = TRUE)
con <- dbConnect(MySQL(),
                 user = dbCredentialsFile$user[1],
                 password = dbCredentialsFile$password[1],
                 host = dbCredentialsFile$host[1],
                 dbname = dbCredentialsFile$dbname[1])
# remove object
rm(dbCredentialsFile)
# garbage collection
gc()

# FUNCTION FOR QUERYING DATABASE
query <- function(...) dbGetQuery(con, ...)

# EXAMPLE FOR QUERYING DATABASE
testDF <- query("SELECT * FROM employees limit 10")

#drv <- dbDriver("MySQL")
#con <- dbConnect(drv, user="root", pass="imbest", dbname = "employees")
#rs <- dbSendQuery(con, statement = "SHOW DATABASES;")
#fetch(rs)
