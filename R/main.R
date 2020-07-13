# DESCRIPTION -------------------------------------------------------------

# This script calls all the script used for the development of the paper.  
# It reproduces the figures as well as other results 
# that were used to elaborate the paper. 
# The documents generated after running this script are stored in ../Reports
# and in ../figures.
# However, you can always dig into the Rmd scripts used to render them, you 
# will find them in ../R.



# Data downloading reports ------------------------------------------------


rmarkdown::render(input = here::here('R', 'Data_downloading', '00_Datadownload.Rmd'), output_file = here::here('Reports', 'Data_downloading', '00_Datadownload.html'))

rmarkdown::render(input = here::here('R', 'Data_downloading', '00.1_Real_operation_reproduction.Rmd'), output_file = here::here('Reports', 'Data_downloading', '00.1_Real_operation_reproduction.html'))


# Pre-processing reports --------------------------------------------------

rmarkdown::render(input = here::here('R', 'Pre-processing', '00_Synthetic_data_validation.Rmd'), output_file = here::here('Reports', 'Pre-processing', '00_Synthetid_data_validation.html'))

rmarkdown::render(input = here::here('R', 'Pre-processing', '01_Data_combination.Rmd'), output_file = here::here('Reports', 'Pre-processing', '01_Data_combination.html'))

rmarkdown::render(input = here::here('R', 'Pre-processing', '01.1_Noise_addition_example.Rmd'), output_file = here::here('Reports', 'Pre-processing', '01.1_Noise_addition_example.html'))

rmarkdown::render(input = here::here('R', 'Pre-processing', '01.2_Extracted_descriptors.Rmd'), output_file = here::here('Reports', 'Pre-processing', '01.2_Extracted_descriptors.html'))


# Analysis reports --------------------------------------------------------

rmarkdown::render(input = here::here('R', 'Analysis', '00_Descriptor_filtering.Rmd'), output_file = here::here('Reports', 'Analysis', '00_Descriptor_filtering.html'))

rmarkdown::render(input = here::here('R', 'Analysis', '01_Hybrid_diagnosis_all_loads.Rmd'), output_file = here::here('Reports', 'Analysis', '01_Hybrid_diagnosis_all_loads.html'))

rmarkdown::render(input = here::here('R', 'Analysis', '02_Loadwise_diagnosis.Rmd'), output_file = here::here('Reports', 'Analysis', '02_Loadwise_diagnosis.html'))

rmarkdown::render(input = here::here('R', 'Analysis', '03_Context_detection.Rmd'), output_file = here::here('Reports', 'Analysis', '03_Context_detection.html'))

rmarkdown::render(input = here::here('R', 'Analysis', '04_Benchmark.Rmd'), output_file = here::here('Reports', 'Analysis', '04_Benchmark.html'))

