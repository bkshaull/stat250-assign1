stat250-assign1
===============
First I use the shell command to create a frequency table from the 15th column of each csv file.  I could not get the csv files to load on my computer (not enough disc space), so I used the bunzip command, which I believe worked, until year 2002 at least. In year 2002, there was the error 'Illegal byte sequence.' I need to try installing the sed software to account for this, ran out of time to do so. So my estimates for mean, median and standard deviation are currently only for the years 1987-2002.  

After running the shell command which creates the document 'freq.txt', I run the R code which uses loops to create the mean, median and standard deviation. I realize this is not the most efficient way to calculate them, so I will try to improve on these methods before submitting next Thursday. 

The run time statistics as well as the results are stored in an object called, 'bshaull_assign1.rda."
I got a mean of 7.076252, median of 3 and standard deviation of 27.116784. Data collection time through shell was approximately 30 minutes, again this was only through 2002. 
