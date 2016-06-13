files <- list.files(pattern = ".csv$")

invisible <- lapply(files, function(file) {
  if (grepl("_ncbi.csv", file)) {
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
  taxinfos <- list()
  while(TRUE) {
    end <- min(start + 499, total)
    taxinfo <- taxize::ncbi_get_taxon_summary(idsunique$uid[start:end])
    taxinfos <- c(taxinfos, list(taxinfo))
    if (end >= total) {
      break()
    }
    start <- end + 1
  }
  
  taxinfos <- do.call(rbind, taxinfos) 
  
  merged <- merge(ids, taxinfo, all.x = TRUE)
  nomatch_idx <- is.na(merged$name)
  merged$name[nomatch_idx] <- merged$uid[nomatch_idx]
  
  outfile <- paste0(filename, "_ncbi.csv")
  write.table(merged$name, outfile, quote = FALSE, row.names = FALSE, col.names = FALSE)
})

#get freq of file
#lala <- read.csv("Vogue1A_52_DNA_1.viralcol4_ncbi.csv", header = FALSE)
#sort(table(lala$V1)) #can set to variable and write that to file
