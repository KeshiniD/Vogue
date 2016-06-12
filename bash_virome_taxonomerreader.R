cd ~/Desktop/ViromeFASTQ_all #tab complete and press enter
for f in *.taxonomer; do name=$(echo $f | cut -f1 -d"."); grep "^viral" $f | gawk ' $2 == "C" {print $4}' > $name.viralcol4.csv; done;

#enter
#will be in viromefastq folder