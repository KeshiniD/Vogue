files <- list.files(pattern = "_root.csv$")

invisible <- lapply(files, function(file) {
  if (grepl("_family.csv", file)) {
    return()
  }
  message("doing ", file)
  
  ids <- suppressWarnings(read.csv(file, header = TRUE))
  colnames(ids) <- "uid"
  ids$uid <- as.character(ids$uid)
  filename <- tools::file_path_sans_ext(file)
  
  idsunique <- unique(ids)
  total <- length(idsunique$uid)
  start <- 1
  tax_names <- list()
  while(TRUE) {
    end <- min(start + 499, total)
    taxinfo <- taxize::ncbi_get_taxon_summary(idsunique$uid[start:end])
    tax_name <- tax_name(query = taxinfo$name, get = "family", db = "ncbi")
    tax_names <- c(tax_names, list(tax_name))
    if (end >= total) {
      break()
    }
    start <- end + 1
  }
  
  tax_names <- do.call(rbind, tax_names) 
  
  merged <- merge(ids, tax_name, all.x = TRUE)
  nomatch_idx <- is.na(merged$family)
  merged$family[nomatch_idx] <- merged$uid[nomatch_idx]
  
  outfile <- paste0(filename, "_family.csv")
  write.table(merged$family, outfile, quote = FALSE, row.names = FALSE, col.names = FALSE)
})

#get freq of file
#lala <- read.csv("Vogue1A_52_DNA_1.viralcol4_ncbi.csv", header = FALSE)
#sort(table(lala$V1)) #can set to variable and write that to file
