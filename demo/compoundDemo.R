require("RPostgreSQL")
require("transmartRClient")

# Setup connection to local transmart server and database
connectToTransmart("http://lobster.nd4.thehyve.net/transmart")
dbConnection <- dbConnect(PostgreSQL(), host = "localhost", port = "5433", user="tm_cz", password="tm_cz", dbname="transmart")

# Fetch subjects with treatment-concepts linked to certain compound (across studies)
getSubjectsByCompound(dbConnection, "c1")
getSubjectsByCompound(dbConnection, "c2")

# Close connection to local database
postgresqlCloseConnection(dbConnection)
