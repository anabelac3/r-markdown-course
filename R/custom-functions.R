# Read in data ----
read_data_excel <- function(rel_directory, pattern) {
    files <- dir(paste0(dirname(getwd()),"/", rel_directory), pattern = pattern, full.names = FALSE)
    df_list <- vector("list", length(files))
    for (fname in files) {
        df_list[[fname]] <- read_excel(paste0(dirname(getwd()),"/", rel_directory ,fname))
    }
    names(df_list) <- paste0("", gsub(pattern,"",names(df_list)))
    return(df_list)
}


# mean_median -------------------------------------------------------------

# function example - get measures of central tendency
# and spread for a numeric vector x. The user has a
# choice of measures and whether the results are printed.
mysummary <- function(x,npar=TRUE,print=TRUE) {
    if (!npar) {
        center <- mean(x); spread <- sd(x)
    } else {
        center <- median(x); spread <- mad(x)
    }
    if (print & !npar) {
        cat("Mean=", center, "\n", "SD=", spread, "\n")
    } else if (print & npar) {
        cat("Median=", center, "\n", "MAD=", spread, "\n")
    }
    result <- list(center=center,spread=spread)
    return(result)
}
