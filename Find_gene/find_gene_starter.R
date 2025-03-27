## find_gene_starter.R
##
## Simulate a DNA sequence then find all open reading frames
## using functions we've already learned and writing our own custom functions.
###############################################################################

## 1. Simulate DNA sequence data

bps <- c("a", "c", "g", "t")   # base pairs in DNA
dna <- sample(bps, 500, replace=T)  # random DNA sequence
basefreq <- table(dna)/length(dna)

## 2. Save your sequence to "yourname_dna.csv"

# If you want to use saved simulated dna, use:
# dna <- read.csv("butler_dna.csv")[[1]]

## 3. Write a function to find a particular codon that you can specify via arguments

# start codon "atg"
bp1 <- "a"
bp2 <- "t"
bp3 <- "g"

              # find codon takes the dna sequence as it's input argument
              # the start codon is used as default arguments so if we don't 
              # specify what we're looking for, it looks for start. You can specify 
              # different values for bp1, bp2, bp3 on the function call to look for 
              # other condons
find_codon <- function( dna, bp1="a", bp2="t", bp3="g") { 

	n <- which(dna== bp1 )   # the index position of "a"s in sequence seq1
	match2 <- dna[ n + 1 ] == bp2    # does codon position 2 match "t"? 
	match3 <- dna[ n + 2 ] == bp3  # does codon position 3 match "g"?
	match123	<- n[ which( match2 & match3 )]  # index position of all matching codons (all 3 match)
	
	return(match123)
}	

## 4. Find all start and stop codons 

start <- find_codon( dna )
amber <- find_codon( dna, "t", "a", "g")
# ochre <- 
# opal <- 
     
    ## check that they are actually correct
	dna[ start[1] : ( start[1] + 2 ) ]  # can you turn this into a function?
	dna[ start[2] : ( start[2] + 2 ) ]    

## 5. Write a function that tells us which frame each codon is in

   # dd is the vector of indices for the codon we're looking for (e.g. start codons)
   # length is the length of the dna sequence
   # returns the frame number for each codon position in dd
which_frame <- function( dd, length ) {     
  frame1 <- seq(1, length, by=3) 
  frame2 <- seq(2, length, by=3) 
  frame3 <- seq(3, length, by=3) 
  
  return((dd %in% frame1)*1  + (dd %in% frame2)*2 + (dd %in% frame3)*3) # matches only one of these
}

which_frame(start, length(dna))
## find them all

## 6. Of the start and stop codons, which ones are in frames 1, 2, and 3? (because start and stop codons must be in the same frame to define an open reading frame). How can we do this? Brainstorm.   


## 7. Now do the same for the reverse direction frames 4, 5, 6
## first need to make reverse complement (because only transcribed in 5' to 3' direction)
rdna <- rev(dna)
rcdna <- rdna
## translate the rdna to rcdna 


## 7. What are all of the possible open reading frames?
# possible ORF's are between each start codon and the next stop codon in the DNA. 
# We can ignore all subsequent stop codons

orf <- data.frame()   # container to hold orf information

## Forward

## Reverse 

## 8. Which one is the longest?  Print to the console 
## Tell us which frame, start position, stop position, and the sequence.


## 9. Print all ORF's to file
## remember to use reverse complement dna for frames 4,5,6
## Tell us which frame, start position, stop position, and the sequence.

## 10. Write output to a text file `yourname\_orf.txt` using the `cat()` function which prints the following information for each open reading frame on separate lines: orf number, start position, stop position, and the sequence (
`orf1 start:xx stop:xx sequence:atgaggtc.....taa`).  _Make sure you include the complete stop codon (all 3 base pairs) in the output_ so you can check that it worked.

## 11. Save your script as `yourname\_find\_gene.R`. Make sure you clear your workspace, test it the code, clean it up and comment it before turning it in. Also save your data `yourname\_dna.csv`, and output `yourname\_orf.txt`.




