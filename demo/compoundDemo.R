require("transmartRClient")

# Setup connection to local transmart server and database
connectToTransmart("http://lobster.nd4.thehyve.net/transmart")

# Fetch all concepts linked to a compound, for all accessible studies
getCompoundConcepts()

# Fetch list of all compounds for a certain collaboratorId within one study
getCompoundConcepts(collaboratorId = "1", studies = "RA_NZEUSSEU_GSE36700")

# Find specific compound across all studies
getCompoundConcepts(collaboratorId = "1", compoundId = "2")


# If you have an account with an LSP server, you can look up more detailed information on a compound
require("LSPRClient")

tokenFromWebClient <- "yourPersonalAPIToken"
connectToLSP( 'https://location.to.your.server', tokenFromWebClient )

# get compounds contained in your accessible studies of the transmart server you are connected to
compoundsInTransmart <- getCompoundConcepts()

# Get more information on the first compound from LSP
getCompound(compoundsInTransmart$`Compound collaboratorId`[1], compoundsInTransmart$`Compound compoundId`[1])
