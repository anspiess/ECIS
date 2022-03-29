getECIS2 <- function(file = NULL, names = NULL, na.omit = TRUE) {
  
  ## get file
  if (is.null(file)) file <- file.choose()
  
  ## get ECIS data, either xlsx or csv
  if (grepl(".xlsx", file)) IN <- as.data.frame(read_excel(file, col_names = TRUE))
  if (grepl(".csv", file)) IN <- as.data.frame(readLines(file))
  
  if (!is.null(names) & length(names) != ncol(IN)) stop("'names' length does not match number of columns!")
  
  ## remove NAs, this can be switched off
  if (na.omit) IN <- na.omit(IN)
  
  ## create new Names (optional)
  if (!is.null(names)) colnames(IN) <- names
  
  ## split ECIS data into list of x/y data
  LIST <- vector("list", length = ncol(IN) - 1)
  for (i in 1:length(LIST)) {
    DAT <- cbind(IN[, 1], IN[, i + 1])
    colnames(DAT) <- c(paste("Time", colnames(IN)[i + 1], sep = "_"), colnames(IN)[i + 1])
    LIST[[i]] <- DAT
  }
  names(LIST) <- colnames(IN)[-1]
  
  ## assign classes
  for (i in 1:length(LIST)) class(LIST[i]) <- c("matrix", "ecis")
  class(LIST) <- c("ecis", "list")
  return(LIST)
}