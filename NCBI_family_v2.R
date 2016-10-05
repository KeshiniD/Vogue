files <- list.files(pattern = "viral_ncbi_code.csv$")

invisible <- lapply(files, function(file) {
  if (grepl("_family.csv", file) || grepl("_ncbi.csv", file)) {
    return()
  }
  message("doing ", file)
  
  ids <- suppressWarnings(read.csv(file, header = FALSE))
  colnames(ids) <- "uid"
  ids$uid <- as.character(ids$uid)
  filename <- tools::file_path_sans_ext(file)
  
  idsunique <- unique(ids)
  total <- length(idsunique$uid)
  start <- 1
  result <- list()
  while(TRUE) {
    end <- min(start + 499, total)
    taxinfo <- taxize::ncbi_get_taxon_summary(idsunique$uid[start:end])
    clas <- taxize::classification(idsunique$uid[start:end], db = 'ncbi', return_id = FALSE)
    clas_mapping <- lapply(clas, function(x) {
      if(!is.data.frame(x)) {
        -99
      } else {
        idx <- which(x$rank == "family")
        if (length(idx) == 0) {
          -99
        } else {
          x$name[idx]
        }
      }
    })
    clas_mapping_df <- 
      data.frame(uid = names(clas_mapping),
                 family = as.character(clas_mapping),
                 stringsAsFactors = FALSE)
    nomatch_idx <- clas_mapping_df$family == -99
    clas_mapping_df$family[nomatch_idx] <- clas_mapping_df$uid[nomatch_idx]
    
    result <- c(result, list(clas_mapping_df))
    if (end >= total) {
      break()
    }
    start <- end + 1
  }
  
  result <- do.call(rbind, result) 
  
  merged <- merge(ids, result, all.x = TRUE)
  
  outfile <- paste0(filename, "_family.csv")
  write.table(merged, outfile, quote = FALSE, row.names = FALSE,
              col.names = FALSE, sep = ",")
})
