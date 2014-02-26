
db=2

if (db == 1) { # the sqlite connection   
  library('RSQLite')  
  setwd("/media/FD/BIOSCREEN/R")  
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname="../SQLdata/epic1.2.sqlite")  
  tablenames <- dbListTables(con)
  n_tables <- length(tablenames)
} else if (db == 2) { # the remote mysql connection
  require(RMySQL)  
  con <- dbConnect(MySQL(), user="guest", password="bioguest123", dbname="bioscreen", host="myelin.ucsf.edu")
  tablenames <- dbListTables(con)
  n_tables <- length(tablenames)
}

# EXAMPLE CODE
#
# 
# ## Importing tables as data frames...
# fields <- dbListFields(con, tables[1])
# table_i <- dbReadTable(con, tablenames[1])
# table_i2 <- data.frame(table_i, stringsAsFactors=TRUE)
# # if(dbExistsTable(con, "new_results"))
# #   dbRemoveTable(con, "new_results")
# # dbWriteTable(con, "new_results", new.output)
# 
# 
# ### Submitting queries ###
# ## 'fetch' way 
# rs <- dbSendQuery(con, paste("select * from HTTP_ACCESS where IP_ADDRESS = '127.0.0.1'", "[]")) # The data is stored anywhere between the server and R memory, depending on the driver.
# df <- fetch(rs, n = 50)
# df2 <- fetch(rs, n = -1)  #  return all elements
# # Fetch everything sequentially
# out <- NULL
# while(!dbHasCompleted(res)){
#   chunk <- fetch(res, n = 10000)
#   out <- c(out, doit(chunk))
# }
# # Free up resources
# dbClearResult(res)
# dbDisconnect(con)
# dbUnloadDriver(drv)
# # Be more direct
# dbGetQuery(); #submit the statement, fetches the output records, clears the result set)
# 
# 
