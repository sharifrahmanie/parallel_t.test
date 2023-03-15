library(doParallel)
# By @biomedical_informatics Edris Sharif Rahmani March 15, 2023
set.seed(123)
num_genes <- 20000
num_samples <- 100
mat1 <- matrix(rnorm(num_genes*num_samples,
                     mean = 5, sd = 1), nrow = num_genes)
rownames(mat1) <- paste0("Gene", 1:num_genes)
colnames(mat1) <- paste0("Sample", 1:num_samples)
mat2 <- matrix(rnorm(num_genes*num_samples,
                     mean = 6, sd = 1.5), nrow = num_genes)
rownames(mat2) <- paste0("Gene", 1:num_genes)
colnames(mat2) <- paste0("Sample", 1:num_samples)
registerDoParallel()
pvals <- foreach(i = seq_len(ncol(mat1)),
                 .combine = rbind,
                 .multicombine = TRUE,
                 .inorder = TRUE,
                 .packages = c('data.table', 'stats')) %dopar% {
  x <- mat1[, i]
  y <- mat2[, i]
  sapply(seq_len(ncol(mat2)), function(j) {
    t.test(x, mat2[, j])$p.value
  })

}
stopImplicitCluster()
View(pvals)
