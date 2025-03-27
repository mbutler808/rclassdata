bps <- c("a", "c", "g", "t")   # base pairs in DNA
dna <- sample(bps, 500, replace=T)  # random DNA sequence
#write.csv(dna, file="butler_dna.csv", row.names=F)
dna <- read.csv("butler_dna.csv", header=F, as.is=T)[[1]]
basefreq <- table(dna)/length(dna)

# start codon
bp1 <- "a"
bp2 <- "t"
bp3 <- "g"
              # for example, the start codon is "atg"

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

## Find all start and stop codons -- check that they are actually correct

start <- find_codon( dna )
amber <- find_codon( dna, "t", "a", "g")
ochre <- find_codon( dna, "t", "a", "a")
opal <- find_codon( dna, "t", "g", "a")

check_codons <- function( codon_vector, dna, type="") {
  print(paste("Type of codon", type))
  for(i in 1:length(codon_vector)) {
    print(dna[ codon_vector[i] : ( codon_vector[i] + 2 ) ])  # check: is it the correct triplet?  # note: seq vs   addition precedence
  }
}
check_codons(start, dna, "start")
check_codons(amber, dna, "amber")
check_codons(opal, dna, "opal")
check_codons(ochre, dna, "ochre")
stopsall <- sort(c(amber, ochre, opal))


## Write a function that tells us which frame each codon is in

               # dd is the vector of indices for the codon we're looking for (e.g. start codons)
               # length is the length of the dna sequence
               # returns the frame number for each codon position in dd
which_frame <- function( dd, length ) {     
  frame1 <- seq(1, length, by=3) 
  frame2 <- seq(2, length, by=3) 
  frame3 <- seq(3, length, by=3) 
  
  return((dd %in% frame1)*1  + (dd %in% frame2)*2 + (dd %in% frame3)*3)
}

which_frame(start, length(dna))
which_frame(amber, length(dna))
which_frame(ochre, length(dna))
which_frame(opal, length(dna))

## Which start and stop codons are in the same frame?

ones <- which_frame(start, length(dna)) == 1
start1 <- start[ones]
twos <- which_frame(start, length(dna)) == 2
start2 <- start[twos]
threes <- which_frame(start, length(dna)) == 3
start3 <- start[threes]

# etc. also same for stops OR 
# can also write a function to pull out all in same frame (not required): 

same_frame <- function( codon_vector, frameN, length) {
	ii <- which_frame(codon_vector, length) == frameN    # finds all codons in frame number frameN
	return(codon_vector[ii])          # returns the vector of codons in frameN
}

start1 <- same_frame( start, 1, length(dna))  
start2 <- same_frame( start, 2, length(dna))  
start3 <- same_frame( start, 3, length(dna))  

stops1 <- same_frame( stopsall, 1, length(dna))
stops2 <- same_frame( stopsall, 2, length(dna))
stops3 <- same_frame( stopsall, 3, length(dna))

# could also write a function or a loop for the above:

starts <- vector(mode="list", length=6)
stops <- vector(mode="list", length=6)

for (i in 1:3) {
	starts[[i]] <- same_frame( start, i, length(dna))  # contains all start codons in frame 1,2,3
	stops[[i]] <- same_frame( stopsall, i, length(dna)) # contains all stop codons in fr 1, 2, 3
}


## Now do the same for the reverse direction frames 4, 5, 6
## first need to make reverse complement (because only transcribed in 5' to 3' direction)
rdna <- rev(dna)
rcdna <- rdna
rcdna[rdna=="a"] <- "t"
rcdna[rdna=="t"] <- "a"
rcdna[rdna=="g"] <- "c"
rcdna[rdna=="c"] <- "g"

start <- find_codon( rcdna )
amber <- find_codon( rcdna, "t", "a", "g")
ochre <- find_codon( rcdna, "t", "a", "a")
opal <- find_codon( rcdna, "t", "g", "a")
stopsall <- sort(c(amber, ochre, opal))

check_codons(start, rcdna, "start")
check_codons(amber, rcdna, "amber")
check_codons(opal, rcdna, "opal")
check_codons(ochre, rcdna, "ochre")


for (i in 1:3) {
	starts[[i+3]] <- same_frame( start, i, length(rcdna))  # contains all start codons in frame 1,2,3
	stops[[i+3]] <- same_frame( stopsall, i, length(rcdna)) # contains all stop codons in fr 1, 2, 3
}


## What are all of the possible open reading frames?
# possible ORF's are between each start codon and the next stop codon in the DNA. 
# We can ignore all subsequent stop codons

orf <- data.frame()   # container to hold orf information

## Forward

for (i in 1:3) {     # i is frame 1, 2, 3
	for (j in 1:length(starts[[i]])) {   # j loops through all starts in each frame
	  begin <- starts[[i]][j]    # beginning of start
	  ii <- stops[[i]] > begin   # find stops after each start
	  end <- stops[[i]][stops[[i]] > begin][1]  # find the first stop after start
	  dat <- data.frame(frame=i, start=begin, stop=end, orflength=end-begin+2)
	  orf <- rbind(orf, dat)   # save orf info: frame, start, stop, and length
	}
	
}

## Reverse

for (i in 4:6) {     # i is frame 4,5,6
	for (j in 1:length(starts[[i]])) {   # j loops through all starts in each frame
	  begin <- starts[[i]][j]    # beginning of start
	  ii <- stops[[i]] > begin   # find stops after each start
	  end <- stops[[i]][stops[[i]] > begin][1]  # find the first stop after start
	  dat <- data.frame(frame=i, start=begin, stop=end, orflength=end-begin+2)
	  orf <- rbind(orf, dat)   # save orf info: frame, start, stop, and length
	}
	
}

orf <- orf[ !is.na(orf$orflength), ]   # get rid of any rows with missing values because start after stop

## Which one is the longest?

print("Longest ORF")
print(orf[ orf$orflength == max(orf$orflength), ])

## Print all ORF's to file
## remember to use reverse complement dna for frames 4,5,6

forward <- which(orf$frame<=3)
reverse <- which(orf$frame>=4)

for (i in forward) {
  cat(	"ORF", i, 
  		" start: ", orf[i, "start"], 
  		" stop: ", orf[i, "stop"], 
  		" length: ", orf[i, "orflength"], 
  		" frame: ", orf[i, "frame"], 
  		" sequence: ", dna[orf[i, "start"]: (orf[i, "stop"]+2)], 
  		"\n", 
  		file="butler_orf.txt", sep="", append=T)
}

for (i in reverse) {
  cat(	"ORF", i, 
  		" start: ", orf[i, "start"], 
  		" stop: ", orf[i, "stop"], 
  		" length: ", orf[i, "orflength"], 
  		" frame: ", orf[i, "frame"], 
  		" sequence: ", rcdna[orf[i, "start"]: (orf[i, "stop"]+2)], 
  		"\n", 
  		file="butler_orf.txt", sep="", append=T)
}


