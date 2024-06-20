Class = read.csv("Dataset.csv" , header = TRUE)

#########################################################
Cdes = read.csv("AP2D.csv", header = TRUE)
D = data.frame(scale(Cdes[, -1]) , Activity = Class[,4])
D = remove_empty(D, "cols")
active   <- subset(D, Activity == 'active')
inactive <- subset(D, Activity == 'inactive')
ntr1 <- floor(0.8 * nrow(active))
ntr2 <- floor(0.8 * nrow(inactive))
set.seed(123)
train_ind1 <- sample(seq_len(nrow(active)), size = ntr1)
train_ind2 <- sample(seq_len(nrow(inactive)), size = ntr2)
trpos <- active[train_ind1, ]
tspos <- active[-train_ind1, ]
trneg <- inactive[train_ind2, ]
tsneg <- inactive[-train_ind2, ]
Dtr = rbind(trpos, trneg)
Dts = rbind(tspos, tsneg)
id <- sample(1:k,nrow(Dtr),replace=TRUE)
