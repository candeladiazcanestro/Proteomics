#####This script is used for performing paired Wilcoxon test

args = commandArgs(trailingOnly = TRUE)

olink_0w <- read.csv(args[1], row.names = 1)
row.names(olink_0w) <- paste(row.names(olink_0w), "_before", sep = "")

olink_12w <- read.csv(args[2], row.names = 1)
row.names(olink_12w) <- paste(row.names(olink_12w), "_after", sep = "")

olink_all <- rbind(olink_0w,olink_12w)

wilcoxon <- function(sample){
  x <- sample[1:36]
  y <- sample[37:72]
  z <- wilcox.test(x, y, paired = TRUE)
  p <- z$p.value
}

olink_p <- as.data.frame(apply(olink_all, 2, wilcoxon))
colnames(olink_p) <- "p_original"
olink_p$p_fdr <- p.adjust(olink_p$p_original, method="fdr")

write.csv(olink_p, "Paired_Wilcoxon_Test_Result.csv")