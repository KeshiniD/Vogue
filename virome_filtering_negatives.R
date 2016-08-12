#filtering reads from negative controls

#need to convert to fasta for blat; fq may be alright for bowtie

#step1: cluster redundant sequences
#uclust
usearch7_64 -cluster_fast R1.fa -id 0.99 -centriods repseq.fasta -uc clusters.uc
#R1 is read #1
#rep is output
#do not need -uc clusters.uc (remove; it gives identity of the reads)
#do this for each read for each negative control

#step 2: what is in negative control, and remove from samples
#step 2 part 1
#bowtie2-build: what is in negative controls
bowtie2-build R1.fa, R2.fa filename
#do not need to cat, can list in order which reads want to include
# for single negative control would have 2 reads
#and for two negative controls have 4 files (2 for each)
#filename is output (do not include "filetype")

#step 2 part 2:remove what is found in negative control from samples
bowtie2 -x filename -U R1.fa,R2.fa --un -S output
#bowtie2 -x(filename; output from build script) -U(forward, reverse samples) -S(output)
#--un: Write unpaired reads that fail to align to file
#write in loop;to apply to applicable samples

#blat: another method to match what is in negative control to sample
#only identifies, manually remove afterwards
#only uses fasta files so convert my fq
#here would need to combine neg into single file
blat negR1.fa sample.fa output.psl

#negR1: negative control file
#sample: participant file; list both reads
#output in psl format

#next step after blat, manually remove files it identified in participant 