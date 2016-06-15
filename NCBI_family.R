files <- list.files(pattern = "_condensed.csv$")

invisible <- lapply(files, function(file) {
  if (grepl("_family.csv", file)) {
    return()
  }
  message("doing ", file)
  
  ids <- suppressWarnings(read.csv(file, header = TRUE))
  filename <- tools::file_path_sans_ext(file)
  
  a <- classification(c(ids$Var1), db="ncbi")
  c <- rbind(a) %>% 
    filter(rank == "family") %>% 
    .$name 
  
  outfile <- paste0(filename, "_family.csv")
  write.csv(c, outfile, quote = FALSE, row.names = FALSE)
})


#get freq of file
#lala <- read.csv("Vogue1A_52_DNA_1.viralcol4_ncbi.csv", header = FALSE)
#sort(table(lala$V1)) #can set to variable and write that to file
#a <- taxize::ncbi_get_taxon_summary(c(13097, 13098, 3554))

#doesn't work the way I want
#files <- list.files(pattern = "_ncbi.csv$")
#
#invisible <- lapply(files, function(file) {
#  if (grepl("_family.csv", file)) {
#    return()
#  }
#  message("doing ", file)
#  
#  ids <- suppressWarnings(read.csv(file, header = TRUE))
#  colnames(ids) <- "uid"
#  ids$uid <- as.character(ids$uid)
#  filename <- tools::file_path_sans_ext(file)
#  
#  idsunique <- unique(ids)
#  total <- length(idsunique$uid)
#  start <- 1
#  tax_names <- list()
#  while(TRUE) {
#    end <- min(start + 499, total)
#    taxinfo <- taxize::ncbi_get_taxon_summary(idsunique$uid[start:end])
#    tax_name <- tax_name(query = taxinfo$name, get = "family", db = "ncbi")
#    tax_names <- c(tax_names, list(tax_name))
#    if (end >= total) {
#      break()
#    }
#    start <- end + 1
#  }
#  
#  tax_names <- do.call(rbind, tax_names) 
#  
#  merged <- merge(ids, tax_names, all.x = TRUE)
#  
#  outfile <- paste0(filename, "_family.csv")
#  write.table(merged$family, outfile, quote = FALSE, row.names = FALSE, col.names = FALSE)
#})

#get freq of file
#lala <- read.csv("Vogue1A_52_DNA_1.viralcol4_ncbi.csv", header = FALSE)
#sort(table(lala$V1)) #can set to variable and write that to file
