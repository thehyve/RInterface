# setup connections to tramsart server and its database
require("transmartRClient")
connectToTransmart("http://lobster.nd4.thehyve.net/transmart")
require("RPostgreSQL")
con <- dbConnect(PostgreSQL(), host = "localhost", port = "5433", user="tm_cz", password="tm_cz", dbname="transmart")

# This study contains a single categorical concept containing three different treatment levels.
# Each of these treatment levels needs to be annotated with a different metadata tag containing the compound ID.
# This would normally be done as part of the ETL of this study.
studyName <- "RA_NZEUSSEU_GSE36700"
concepts <- getConcepts(studyName)
tagKeyName <- "Compound collaborator ID"

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

# This study contains a single categorical concept containing one treatment levels.
# Annotating this treatment level with a compound ID.
studyName <- "RA_Raterman_GSE37107"
concepts <- getConcepts(studyName)
tagKeyName <- "Compound collaborator ID"

dbSendQuery(con, paste(sep = "",
        "insert into i2b2metadata.i2b2_tags(tag_id, path, tag, tag_type, tags_idx)
        values (1, '", concepts[grep("^baseline$", concepts$name),]$fullName,
        "', 'c1', '", tagKeyName, "', 1);"))

postgresqlCloseConnection(con)
