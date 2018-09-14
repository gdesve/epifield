# epifield
R epifield function 


First priority functions :

count

freq

epitable (2-way tables, with Fisher’s p-values, Chi square p-values, row percentages and column percentages; order of variables to be as epidemio as possible)

epiorder : turns integers and characters into factors, with yes-no labels (or unexposed-exposed, vaccinated-unvaccinated, +/-, or whatever you want; in the correct order to make tables easier (e.g. case first, then control)

Describe: describe(dataset), describe(variable)

Clear: clear values, data, function / pattern

Drop: drop variables (column), drop records

Graphs: line graphs, bar charts, histogrammes [for secular trends exercise]
-----------------
- checkMissing
- Syntax: checkMissing(data, vars=NULL, sort=FALSE)
- Description: Missing data: The equivalent of misschk in Stata (but without the patterns table)
-----------------

Label values (for non-binary variables or non obvious order binary variables, e.g. male/female)

-----------------
- rename 
- Syntax: rename( data, colname, newname)
- Description:
-----------------
logreg  logistic regression

