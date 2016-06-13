cd ~/Desktop/ViromeFASTQ_all #tab complete and press enter
for f in *.taxonomer; do name=$(echo $f | cut -f1 -d"."); grep "^viral" $f | gawk ' $2 == "C" {print $4}' > $name.viralcol4.csv; done;

#enter
#will be in viromefastq folder

#taxonomer reader
a <- taxize::ncbi_get_taxon_summary(c(13097, 13098, 3554))
#instead of giving numbers, do data$colname with numbers in it