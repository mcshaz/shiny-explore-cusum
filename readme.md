# RA-CUSUM - Shiny demonstration

### Synopsis

Demonstrate how shiny might be used to help explore risk adjusted CUSUM[[1]](#1) charts as part of an investigation into the "trigger" or "warning" threshold being crossed.

A working example is hosted [here](https://mcshaz.shinyapps.io/shiny-explore-cusum/)

### Data

The data used here are fictitious data, with a distribution of risk of death as would be expected to be found in a large adult ICU using APACHE3 or ANZROD to generate risk scores.

### Use

This tool is only of use *after* data have been examined to ensure they are as accurate and complete as possible (especially minimising missing values). Once these steps have been complete, the cases within the clustering of deaths (which lead to the threshold being crossed) should be examined by clinicians. There is not a huge benefit in examining deaths in patients who had a high predicted mortality. For example, a patient with an 80% predicted risk of death  will only move the CUSUM 2% of the way towards the warning threshold (assuming an h of 4.6), but a patient with a 10% risk of death will move the CUSUM line up 13% of the way.

## Step by Step

On loading, you will see the following CUSUM

![CUSUM on commencement](https://raw.githubusercontent.com/mcshaz/shiny-explore-cusum/main/screenshots/Screenshot%20full.png)

Note that the cases we want to examine are from about case #2500 to case #3000, so we set the slider on the left accordingly. The CUSUM will zoom in on the region we want. You will note as you hover your mouse over the deep pink or blue alternating upticks, the relevant line highlights yellow with a pop-up displaying brief details of the death which lead to that uptick.

![Zoomed CUSUM](https://raw.githubusercontent.com/mcshaz/shiny-explore-cusum/main/screenshots/Screenshot%20Narrow%20down.png)

In the upper right of the CUSUM plot there is a green and a red lasso, for selecting and deselecting parts of the CUSUM. Here we have selected the region we are interested in and the selected upticks have turned red.

![Zoomed CUSUM](https://raw.githubusercontent.com/mcshaz/shiny-explore-cusum/main/screenshots/Screenshot%20selection.png)

Next we change tabs (upper right of the window) to the "Selected Cases" where we can see a table listing all the selected cases (this is a multi-page table with the arrow at the upper right of the table for moving between pages). The copy button copies all cases into the clipboard, ready to be pasted into a spreadsheet or word processor.

![Zoomed CUSUM](https://raw.githubusercontent.com/mcshaz/shiny-explore-cusum/main/screenshots/Screenshot%20table.png)

Armed with the focused data in the tables, it is envisaged case mix can be examined and relevant case reviews commence.

## References
<a id="1">[1]</a> 
Cumulative sum of the Resetting Risk Adjusted Sequential Probability Ratio Test (RA-SPRT)
see Grigg OA, Farewell VT, Spiegelhalter DJ. Use of risk-adjusted CUSUM and RSPRT charts for monitoring in medical contexts. Stat Methods Med Res. 2003 Mar;12(2):147-70. doi: 10.1177/096228020301200205. PMID: 12665208.
