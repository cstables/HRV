 # http://rhrv.r-forge.r-project.org/documentation.html
 # http://cran.at.r-project.org/web/packages/RHRV/RHRV.pdf
 # install.packages("RHRV")
 
 # DO THIS FIRST
 # library(RHRV)
 
#Get list of Hearts and Days to process
HeartDay <- c("RR_Wk10_ECG1_T1_Day1_","RR_Wk10_ECG1_T1_Day2_", "RR_Wk10_ECG1_T1_Day3_", 
"RR_Wk10_ECG2_T2_Day1_", "RR_Wk10_ECG2_T2_Day2_", "RR_Wk10_ECG2_T2_Day3_", 
"RR_Wk10_ECG3_T5_Day1_", "RR_Wk10_ECG3_T5_Day2_", "RR_Wk10_ECG3_T5_Day3_", 
"RR_Wk10_ECG5_T9_Day1_", "RR_Wk10_ECG5_T9_Day2_", "RR_Wk10_ECG5_T9_Day3_", 
"RR_Wk10_ECG6_T10_Day1_", "RR_Wk10_ECG6_T10_Day2_","RR_Wk10_ECG6_T10_Day3_",
"RR_Wk10_ECG7_T3_Day1_", "RR_Wk10_ECG7_T3_Day2_", "RR_Wk10_ECG7_T3_Day3_",
"RR_Wk10_ECG8_T4_Day1_", "RR_Wk10_ECG8_T4_Day2_", "RR_Wk10_ECG8_T4_Day3_", 
"RR_Wk10_ECG9_T7_Day1_", "RR_Wk10_ECG9_T7_Day2_", "RR_Wk10_ECG9_T7_Day3_", 
"RR_Wk10_ECG11_T11_Day1_", "RR_Wk10_ECG11_T11_Day2_", "RR_Wk10_ECG11_T11_Day3_", 
"RR_Wk10_ECG12_T12_Day1_", "RR_Wk10_ECG12_T12_Day2_", "RR_Wk10_ECG12_T12_Day3_", 
"RR_Wk10_ECG13_T13_Day1_", "RR_Wk10_ECG13_T13_Day2_", "RR_Wk10_ECG13_T13_Day3_")
 
for (j in seq_along(HeartDay)) {
	#Get list of files to process
	files <- list.files(pattern = (paste(HeartDay[j])))
	# Get table to add data to
	HRV_results <- read.table("HRV_results.txt", head=TRUE, sep = "\t", stringsAsFactors=F) #add stringsAsFactors=F to allow characters in table
	#Set up loop to process each file in turn
	for (i in seq_along(files)) {
	 
		 hrv.data <- CreateHRVData()
		 hrv.data <- SetVerbose(hrv.data, TRUE) # when tried the LoadBeatRR function without doing this first it gave me an error
		 hrv.data <- LoadBeatRR(hrv.data, files[i], scale = 0.001)
		 hrv.data <- BuildNIHR(hrv.data) # creates non-interpolated HR
		 hrv.data <- CreateTimeAnalysis(hrv.data, numofbins = 1)
		 hrv.data = CreateNonLinearAnalysis(hrv.data)
		 hrv.data = PoincarePlot(hrv.data, indexNonLinearAnalysis=1, timeLag=1, doPlot=FALSE) #timeLag=1 indicates classic poincare plot (not based on confidence regions), doPlot is whether to plot or not
		 hrv.data <- InterpolateNIHR(hrv.data, freqhr = 20) #freq = interpolation frequency. Use 20Hz for mouse data (used in Kubios)
		 hrv.data <- CreateFreqAnalysis(hrv.data) #use interpolated data
		 #calculate power band: use size = 285 (300-5%) to get one window for all files
		 hrv.data <- CalculatePowerBand(hrv.data, size=285, shift=30, type="fourier", ULFmin=0, ULFmax=0.01, VLFmin=0.01, VLFmax=0.15, LFmin=0.15, LFmax=1.5, HFmin=1.5, HFmax=5)
		 
		 HR <- mean(hrv.data$Beat$niHR)
		 SDNN <- hrv.data$TimeAnalysis[[1]]$SDNN # gives time domain analysis value for SDNN
		 RMSSD <- hrv.data$TimeAnalysis[[1]]$rMSSD
		 LF <- hrv.data$FreqAnalysis[[1]]$LF # gives freq domain analysis value for LF
		 HF <- hrv.data$FreqAnalysis[[1]]$HF
		 LFHF <- hrv.data$FreqAnalysis[[1]]$LFHF
		 SD1 <- hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD1
		 SD2 <- hrv.data$NonLinearAnalysis[[1]]$PoincarePlot$SD2
		
		#Append Data to HRV_results
		NewRow <- list(i,(files[i]), HR, SDNN, RMSSD, LF, HF, LFHF, SD1, SD2)
		HRV_results <- rbind(HRV_results, NewRow)
	 }

	#write New_HRV_results
	filename <- paste("HRV_results_",(paste(HeartDay[j])),".csv", sep = "")
	write.csv(HRV_results, file = filename, row.names = FALSE)
}