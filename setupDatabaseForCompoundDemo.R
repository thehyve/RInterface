connectToTransmart("http://localhost:8080/transmart")
localEnvironment <- transmartClientEnv

studyName <- "RA_NZEUSSEU_GSE36700"
concepts <- getConcepts(studyName)
tagKeyName <- "Compound collaborator ID"

require("RPostgreSQL")
con <- dbConnect(PostgreSQL(), host = "localhost", port = "5432", user="timdo", password="timdo", dbname="transmart")
dbSendQuery(con, paste(sep = "",
        "insert into i2b2metadata.i2b2_tags(tag_id, path, tag, tag_type, tags_idx)
        values (1, '", concepts[grep("^colchicine$", concepts$name),]$fullName,
        "', 'c1', '", tagKeyName, "', 1);"))
dbSendQuery(con, paste(sep = "",
       "insert into i2b2metadata.i2b2_tags(tag_id, path, tag, tag_type, tags_idx)
        values (1, '", concepts[grep("^NSAIDs$", concepts$name),]$fullName,
       "', 'c2', '", tagKeyName, "', 1);"))
dbSendQuery(con, paste(sep = "",
       "insert into i2b2metadata.i2b2_tags(tag_id, path, tag, tag_type, tags_idx)
        values (1, '", concepts[grep("^NSAIDs and colchicine$", concepts$name),]$fullName,
       "', 'c2+c1', '", tagKeyName, "', 1);"))

postgresqlCloseConnection(con)
