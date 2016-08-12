cd ~/Desktop/ViromeFASTQ_all #tab complete and press enter
for f in *.taxonomer;
do name=$(echo $f | cut -f1 -d".");
grep "^viral" $f | gawk ' $2 == "C" {print $4}' > $name.viralcol4.csv; done;

#enter
#will be in viromefastq folder

#taxonomer reader
a <- taxize::ncbi_get_taxon_summary(c(13097, 13098, 3554))
#instead of giving numbers, do data$colname with numbers in it

#transfer files to salk
#set working directory to desktop
scp -r ~/Desktop/ViromeFASTQ_all/DNA_Viruses/C935CANXX_7_DNA/Controls/DNA_Adeno/* kdevakandan@salk.bccdc.med.ubc.ca:/home/kdevakandan/virome_kesh/

  
inputs=$(find . -name "*.fastq.gz")  #find file names with "
for f in $inputs; do
  name=$(basename $f .fastq.gz)
  dir=$(dirname $f)
  cutadapt -a CTGTCTCTTATACACATCT $f > ${dir}/${name}_cutadpt.fastq
done


# step 1
inputs=$(find . -name "*.fastq.gz"); for f in $inputs; do fastqc $f; done

# step 2
inputs=$(find . -name "*.fastq.gz"); for f in $inputs; do name=$(basename $f .fastq.gz); dir=$(dirname $f); cutadapt -a CTGTCTCTTATACACATCT $f > ${dir}/${name}_cutadpt.fastq; done

# step 3 same
inputs=$(find . -name "*_cutadpt.fastq"); for f in $inputs; do fastqc $f; done

# step 4 trimming
inputs=$(find . -name "*_1_cutadpt.fastq"); for f in $inputs; do name=${f%????????????????}; dir=$(dirname $f); trim_galore -o $dir --stringency 5 --paired --adapter GTTTCCCACTGGAGGATA --length 60 --quality 20 --retain_unpaired -r1 69 -r2 69 --fastqc --phred33 --keep "${name}_1_cutadpt.fastq" "${name}_2_cutadpt.fastq"; done

###
#need to filter; script below afterwards run merge and fastqc loops
###

#step 5 merge
inputs=$(find . -name "*_1_cutadpt_val_1.fq"); for f in $inputs; do name=${f%???????????????????}; pandaseq -f ${name}_1_cutadpt_val_1.fq -r ${name}_2_cutadpt_val_2.fq -B -F -u ${name}.unaligned.fastq > ${name}.aligned.fastq; done

nohup sh step4trimming.sh > step4trimming.out 2>&1& #running script and renamed nohup.out output file

#step 6 covert fasta to fastq; dont need
# wget https://fasta-to-fastq.googlecode.com/files/fasta_to_fastq.pl  
# chmod u+x fasta_to_fastq.pl
# perl fasta_to_fastq.pl reads.fasta > my_converted_fasta.fastq
  
#step 7 same as 1&3
inputs=$(find . -name "*.aligned.fastq"); for f in $inputs; do fastqc $f; done

#step 6 - dont need
#inputs=$(find . -name "*.aligned.fasta"); for f in $inputs; do name=${f%??????????????}; dir=$(dirname $f); perl fasta_to_fastq.pl ${name}.aligned.fasta > ${name}aligned.converted.fastq; done

########

# gzip all the aligned fastq files
find /projects5/kdevakandan/virome/ -name "*.aligned.fastq" -exec gzip {} \;

# make links from all the fastq files into one folder, for easier downloading
find /projects5/kdevakandan/virome/ -name "*.aligned.fastq.gz" -exec ln -s {} /projects5/kdevakandan/virome/fastqc_aligned/ \;

################
#filtering
#step 1: convert fastq to fasta (only negative controls)
#fastq_to_fasta -n -i input(fq) -o output(fa)
inputs=$(find . -name "*.fq"); for f in $inputs; do name=$(basename $f .fq); fastq_to_fasta -n -i ${name}.fq -o ${name}.fasta; done
#should be exact name convention except fasta instead of fq

#step2: clustering redundant sequences in negative controls
#uclust
#usearch7_64 -cluster_fast R1.fa -id 0.99 -centriods repseq.fasta 
inputs=$(find . -name "*.fasta"); for f in $inputs; do name=$(basename $f .fasta); usearch7_64 -cluster_fast ${name}.fasta -id 0.99 -centriods ${name}_uclust.fasta; done

#step3: bowtie-build, what is in negative controls?
# bowtie2-build R1.fa, R2.fa filename

inputs=$(find . -name "*_uclust.fasta"); for f in $inputs; do name=${f%?????????????????????????????}; bowtie2-build ${name}_1_cutadpt_val_1_uclust.fasta, ${name}_2_cutadpt_val_2_uclust.fasta ${name}__btref; done
#do not need to cat, can list in order which reads want to include
# for single negative control would have 2 reads
#and for two negative controls have 4 files (2 for each)
#filename is output (do not include "filetype")

#step4: removing reads found from samples
inputs=$(find . -name "*_1_cutadpt_val_1.fq"); for f in $inputs; do name=${f%???????????????????}; bowtie2 -x negativesample -U ${name}_1_cutadpt_val_1.fq, ${name}_2_cutadpt_val_2.fq --un -S ${name}_filtered
#bowtie2 -x(filename; output from build script) -U(forward, reverse samples) -S(output)
#--un: Write unpaired reads that fail to align to file
#write in loop;to apply to applicable samples

#convert sam files to fastq
java -jar picard.jar SamToFASTQ I=input.bam FASTQ=output.fastq




