Interactive web app can be found here: https://jeffreyd.shinyapps.io/crashes/

___

The data set used by the published app is cut down to a random sample of 20,000 points in the years 2019 to 2020 due to the limitations of Shiny web apps. The file `crashes.R` is the version used in the app above.

`crashes-full.R` omits the sampling if you wish to run it yourself in RStudio (comment the map output if you're having issues loading).

___

I have split the full data set (2014-2023) into zipped `.csv` files for each year to get under the GitHub file size limit. 

If you would like to combine these subsets into the full data set from 2014 to 2023 (or whatever years you wish):
  1. Unzip the compressed `.csv` files in a separate folder
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
  3. The new `.csv` file will appear in the same folder as the other `.csv` files
  4. Move the new `.csv` file into your working directory (where your `.R` file is)
  5. Adjust the `read_csv()` command in your `.R` file according to what you named your new `.csv`

If you're having issues:
- `folder_path` should be the path to your `.csv` folder in quotes
- Ensure all `.csv` files in the folder have the same column structure and order (they should if they're in a separate folder)
