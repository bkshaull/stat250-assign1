setwd(/Network/Servers/mars.ucdavis.edu/Volumes/UHome/Xtra/shaull/Documents/STA250)
con=pipe('bunzip2 -c /Network/Servers/mars.ucdavis.edu/Volumes/UHome/Xtra/shaull/Documents/STA250/1987.csv.bz2 | cut -f 15 -d ","')
con1=pipe('bunzip2 -c /Network/Servers/mars.ucdavis.edu/Volumes/UHome/Xtra/shaull/Documents/STA250/1987.csv.bz2 | wc -l')
 n_max=scan(con1)
 n_max=as.integer(n_max)

i=1
m_n=0
m_n_one=0
s_n=0
block=1000000
med=rep(0, as.integer(n_max/block)+1)

while (i*block<=n_max) {
	#reading in data.  blocks of 1000 at a time and skipping the 
	#first i-1*1000
	data=scan(con, nmax=(i*block), skip=((i-1)*block)+1, quiet=TRUE)
	data[is.na(data)]=0
	#running mean calculation
	m_data=mean(data)
	m_n_one=m_n
	m_n=m_n+(m_data-m_n)/(i)

	#running variance calculation
	s_n=s_n+(m_data-m_n_one)*(m_data-m_n)
	
	#calculating median for each data set
	med[i]=median(data)
	

	i=i+1
	}
mean_1987=m_n
sd_1987=(s_n)^(1/2)
med_1987=median(med)
