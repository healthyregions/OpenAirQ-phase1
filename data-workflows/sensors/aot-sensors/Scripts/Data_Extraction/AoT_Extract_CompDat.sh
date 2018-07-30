# Bulk download of the complete dataset and extract data from the last day. 
# Print the script runtime.

start=`date +%s`
cd "/Users/irene/Documents/AL/Data/AoT/Complete/" #Change the working directory (cd path)
wget -O file.tar.gz http://www.mcs.anl.gov/research/projects/waggle/downloads/datasets/AoT_Chicago.complete.latest.tar
tar -xvzf file.tar.gz
cd AoT_Chicago.complete.2018-06-11/ #Change with corresponding date
cat data.csv | grep "2018/06/10" > yesterday.csv #Change the data with previous day date
cat data.csv | grep "alphasense" > pm.csv #Extract only the PM measurements (alphasense board)
end=`date +%s`
runtime=$((end-start)) #Print the script runtime