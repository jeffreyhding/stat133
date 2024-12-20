## Car Accidents in California
**Description:** Graphical analysis and interactive map of car accidents in California from 2014 to 2023

Interactive web app can be found here: https://jeffreyd.shinyapps.io/crashes/

___

This data was provided by Amalia Stahl of UC Berkeley's [SafeTREC](https://safetrec.berkeley.edu/) for use by the STAT 133 course.

Due to the limitations of Shiny web apps, the data used in the published app is a random sample of 20,000 data points in the years from 2019 to 2021, cut down from 2014 to 2023. The file `crashes.R` is the version used in the app above. To use the full data set, see below.

___

The `crashes-full.R` version omits the random sampling step if you wish to run it yourself in RStudio (cannot publish).
If you're having issues loading, I recommend commenting out the map output as there are likely too many data points for the Leaflet plot to handle.

I have split the full data set from 2014 to 2023 into zipped `.csv` files for each year in order to get under the GitHub file size limit. If you plan on using the full set, note that the combined `.csv` file will be roughly 400 MB in size.

<br>

To combine these subsets into the full data set (or whatever years you wish):
  1. Unzip the compressed `.csv` files for the years you want in a **separate folder**
  2. Run the following code on your RStudio console:
     ```
     library(dplyr)
      
     # Define the folder path containing your CSV files
     folder_path <- "path/to/your/csv/folder"
      
     # Puts all CSV files in the folder into a list
     csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
      
     # Reads and combines all the CSV files into one data frame
     combined_data <- csv_files %>%
       lapply(read.csv) %>%     # Read each CSV file into a data frame
       bind_rows()              # Combine all data frames by rows
      
     # Writes the combined data to a new CSV file
     write.csv(combined_data, "combined_data.csv", row.names = FALSE)
     ```
     The new `.csv` file will appear in the same folder as the other `.csv` files
  3. Move the new `.csv` file into your working directory (where your `.R` file is)
  4. Adjust the `read_csv()` command in your `.R` file according to what you named your new `.csv` (line 20)
  5. Adjust the `sliderInput` widgets according to the years you chose to include in your combined data (lines 123-125, 165-167)

<br>

If you're having issues:
- `folder_path` should be the path to your `.csv` folder **in quotes**
- Ensure **all** `.csv` files in the folder have the same column structure/order (they should if you unzipped them all in a separate folder)
