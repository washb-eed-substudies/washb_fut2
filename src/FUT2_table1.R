# Install and load necessary packages
library(tidyverse)
library(tableone)
library(officer)
library(flextable)

d<- readRDS(file="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FU2_clean_data.RDS")

#distinct to hoursehold
head(d)
dim(d)
d <- d %>% filter(!is.na(mFU2) | !is.na(cFU2)) %>%
  filter(!is.na(dmy(st_date)) | !is.na(dmy(ur_date)) | !is.na(an_aged)) %>%
  distinct(dataid, .keep_all = TRUE) 
dim(d)

table(is.na(d$st_date))

table(is.na(d$cFU2) & is.na(d$mFU2))

# Suppose 'data' is your dataframe and 'tr' is your treatment variable
d <- d %>%
  rename(Sex = sex,
         `Birth order` = birthord,
         `Maternal age` = momage,
         `Maternal height` = momheight,
         `Maternal education` = momedu,
         `Household food security` = hfiacat,
         `Number under 18 in compound` = Nlt18,
         `Number in compound` = Ncomp,
         `Distance to water source` = watmin,
         `Improved walls` = walls,
         `Improved floor` = floor,
         `Electricity` = elec,
         `Number of cattle` = n_cows,
         `Number of goats` = n_goats,
         `Number of chickens` = n_chickens) 


Wvars<-c(
         'Sex', 'Birth order',
         'Maternal age', 'Maternal height',
         'Maternal education','Household food security',
         'Number under 18 in compound','Number in compound','Distance to water source',
         'Improved walls', 'Improved floor',
         'Electricity', 
         'Number of cattle', 'Number of goats', 'Number of chickens')
tab1 <- CreateTableOne(vars = Wvars, strata = "tr", data = d, test = FALSE)

# Convert tableone object to dataframe for use with flextable
tab1_df <- print(tab1, printToggle = FALSE, quote = FALSE, noSpaces = TRUE)
row_names <- rownames(tab1_df)
rownames(tab1_df) <- NULL
tab1_df <- data.frame(row_names, tab1_df)
# Create a flextable
tab1_flex <- flextable::flextable(tab1_df)

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width=8.5, height=11),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0))

save_as_docx("Table 1" = tab1_flex, 
             path="C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FUT2_table1.docx", 
             pr_section = sect_properties) 




tbl1flex <- flextable(tab1)
tbl1flex <- set_header_labels(tbl1flex, values = list("Variable" = "Variable", "Statistic" = "n (%) or median (IQR)"))
tbl1flex <- hline_top(tbl1flex, part="header", border=fp_border(color="black", width = 1))
tbl1flex <- hline_bottom(tbl1flex, part="all", border=fp_border(color="black", width = 1))
tbl1flex <- autofit(tbl1flex, part = "all")
tbl1flex <- align(tbl1flex, j = c(1, 2), align = "left", part="all")
#tbl1flex <- align(tbl1flex, j = 4, align = "center", part="all")
tbl1flex <- fit_to_width(tbl1flex, max_width=8)
#tbl1flex %>% add_footer_row(top=F, values = "*CESD-20 = Center for Epidemiologic Studies Depression Scale Revised.", colwidths = 4)



# Create a new Word document and add the table
doc <- officer::read_docx()
doc <- body_add_flextable(doc, tab1_flex)
print(doc, "C:/Users/andre/Dropbox/WASHB-EE-analysis/WBB-EE-analysis/Results/Andrew/FUT2_table1.docx")  # replace "TableOne.docx" with your preferred filename
print(doc, "C:/Users/andre/Documents/EE/WASHB_EE_Analysis/WBB EE Analysis/R EE outcome output/FUT2_table1.docx")  # replace "TableOne.docx" with your preferred filename


