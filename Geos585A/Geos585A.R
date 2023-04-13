library(openxlsx)
file_path <- "/OneDrive/Documents/gisclass/mgisdata/604A Applied GIS/MEKO/RCodeMeko/Geos585A/GeosTrialData.xlsx"
xlsx_file1 <- loadWorkbook(file_path)
print(xlsx_file1)



sheet_names <- getSheetNames(xlsx_file1)
first_sheet <- readWorksheet(xlsx_file, sheet = sheet_names[1])
row1_str <- paste(first_sheet[1, ], collapse = ", ")
row2_str <- paste(first_sheet[2, ], collapse = ", ")
print(row1_str)
print(row2_str)



