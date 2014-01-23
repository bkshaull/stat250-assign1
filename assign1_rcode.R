setwd('/Volumes/MTN_LION_BU/Users/Shared/Delays1987_2013')
time1=proc.time()
freq=read.table("freq.txt", header= FALSE)
numfreq=nrow(freq)
freq$V2=as.integer(as.character(freq$V2)) #converting delay minutes into integers, originially stored differently
freq[is.na(freq)]=0 #gets rid of the NAs
freq=freq[order(freq$V2), ]

#calculating Mean:
sum_track=0
i=1
while (i<=numfreq) {
        mult=freq[i,1]*freq[i,2] #multiplying row elements together
        sum_track=sum_track+mult #summing the multiplied elements across all rows
        i=i+1
        }
total_obs=sum(freq$V1)
mean=sum_track/total_obs  #mean is the sum of the multiplied elements divided by the total number of observations

#Calculating Median
#Median is where 50% of observations lie above and below that number. This differs depending on whether or not the number of observations is even or odd.  In our case the only time this matters is when the number of observations is even and on the boundary because then the two numbers need to be averaged. I use an if statement to account for this case. Otherwise the median can be calculated by summing the frequency points in the first column as long as the sum is less than half. Then the next arrival delay in the following row will be the median.
half=total_obs/2
sum_v1=0
j=1
while (sum_v1<half) {  
        sum_v1=sum_v1+freq[j,1]
        j=j+1
        }
median=freq[j+1,2]
if (is.integer(half)==TRUE & sum_v1+freq[j+1,1]==half) {
        median=(freq[j+1,2]+freq[j+2,2])/2 }


#calculating Standard deviation
k=1
sum_sq=0
while (k<=numfreq) {
        sum_sq=sum_sq+freq[k,1]*((freq[k,2]-mean)^2)
        k=k+1}
st_dev=(sum_sq/(total_obs-1))^(1/2)
time2=proc.time()-time1

bshaull_assign1=list(time = time2, results = c(mean = mean, median = median, sd = st_dev),
     system = Sys.info(),  session = sessionInfo())
save(bshaull_assign1, file="bshaull_assign1.rda")
