require("RPostgreSQL")

# Setup connection to local transmart server and database
connectToTransmart("http://localhost:8080/transmart")
dbConnection <- dbConnect(PostgreSQL(), host = "localhost", port = "5432", user="timdo", password="timdo", dbname="transmart")

# Fetch subjects with treatment-concepts linked to certain compound (across studies)
getSubjectsByCompound(dbConnection, "c1")
getSubjectsByCompound(dbConnection, "c2")

# Close connection to local database
postgresqlCloseConnection(dbConnection)
