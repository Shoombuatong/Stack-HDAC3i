# Set working directory
setwd("~/Desktop/Stack-HDAC3i")

# Importing libraries
library(prospectr)

# Read CSV
df <- read.csv("Dataset.csv")

# Check for missing values
sum(is.na(df))

chembl_names <- c("Molecule.ChEMBL.ID","Smiles","Standard.Type","Standard.Relation","Standard.Value","Standard.Units") 

df <- df[, chembl_names]


# Displays compounds having IC50 greater than 100000 nM
df1 <- df[df$Standard.Value>100000,]
df2 <- df[df$Standard.Value<100000,]

df2$Standard_value <- -log10(df2$Standard.Value*10E-9)

# Drop columns that are not needed
drops <- c("Standard.Type", "Standard.Relation", "Standard.Units", "Standard.Value")
df3 <- df2[ , !(names(df2) %in% drops)]

# Write out the file 
write.csv(df3, "ER_alpha_pic50.csv")

################################################################################

# Write SMILES + ChEMBL ID as a SMI file (for subsequent Fingerprint Calculation)

ER_alpha_smi <- df3[,c("Smiles","Molecule.ChEMBL.ID")]

write.table(ER_alpha_smi, "ER_alpha.smi", row.names=FALSE, col.names=FALSE, quote = FALSE, na="")

##################################################################

er_smi <- read.table("ER_alpha.smi", quote = "", header = FALSE, comment.char = "")

# Calculate fingerprints

try(system("bash FP_Fingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_ExtendedFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_EstateFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_GraphOnlyFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_MACCSFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_PubchemFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_SubstructureFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_SubstructureFingerprintCount.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_KlekotaRothFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_KlekotaRothFingerprintCount.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_AtomPairs2DFingerprinter.sh", intern = TRUE, ignore.stderr = TRUE))
try(system("bash FP_AtomPairs2DFingerprintCount.sh", intern = TRUE, ignore.stderr = TRUE))

##################################################################
################################################################################
