# Excel Settings (openxlsx)
# 2024-03-22
# Erik.Leppo@tetratech.com
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy here to not clutter main script
# For use with baytrends 4d diagnostic reports
# Copy tables from report to table
# openxlsx requires some setup (e.g., styles)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# not including a NOTES worksheet as this time


# Global ----

xl_SR <- 8 # number of rows to skip for new worksheets
xl_SR_trans <- 2 # for transposed df, skip worksheet title

# Excel, Formatting ----
## Excel, Formatting, Styles ----
style_title <- openxlsx::createStyle(fontName = "Cambria"
                                     , fontSize = 18
                                     , fontColour = "#1F497D"
                                     , textDecoration = "bold")
style_h1 <- openxlsx::createStyle(fontName = "Calibri"
                                  , fontSize = 15
                                  , fontColour = "#1F497D"
                                  , textDecoration = "bold"
                                  , border = "Bottom"
                                  , borderColour = "#4F81BD"
                                  , borderStyle = "thick")
style_h2 <- openxlsx::createStyle(fontName = "Calibri"
                                  , fontSize = 13
                                  , fontColour = "#1F497D"
                                  , textDecoration = "bold"
                                  , border = "Bottom"
                                  , borderColour = "#A7BFDE"
                                  , borderStyle = "thick")
style_hyperlink <- openxlsx::createStyle(fontName = "Calibri"
                                         , fontSize = 11
                                         , fontColour = "#0000FF"
                                         , textDecoration = "underline")
style_bold <- openxlsx::createStyle(textDecoration = "bold")
style_date <- openxlsx::createStyle(numFmt = "DATE")
style_halign_center <- openxlsx::createStyle(halign = "center")



