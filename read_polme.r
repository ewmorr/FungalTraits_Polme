require(dplyr)

#Read Polme database
polme.csv = read.csv("Polme_FungalTraits_1.2_ver_16Dec_2020.csv")
colnames(polme.csv)

polme.csv$primary_lifestyle %>% unique
polme.csv$Secondary_lifestyle %>% unique

paste(polme.csv$primary_lifestyle, polme.csv$Secondary_lifestyle, sep = "; ") %>% unique

trophic.p_s = data.frame(
    Genus.join = polme.csv$GENUS, #named as such bc this is the column to join by
    primary = polme.csv$primary_lifestyle,
    secondary = polme.csv$Secondary_lifestyle,
    stringsAsFactors = F
)

#Read test taxa and test table
#Both of these tables have rownames as ASV names.
#Note that the Genus column follows the standard for txonomic designation in metabarcoding and so the genus name is preceded by "g__" which will need to be removed before joining with trophic data
test_tax = read.table("ASVs_taxonomy.tsv")
test_tab = read.table("ASVs_counts.tsv")

test_tax.names = data.frame(ASV = rownames(test_tax),
    test_tax,
    Genus.join = sub("g__", "", test_tax$Genus)
)

#
#Join with taxonomy by genus
tax_w_troph = left_join(test_tax.names, trophic.p_s, by = "Genus.join")
tax_w_troph$primary %>% unique
tax_w_troph$secondary %>% unique

#Join troph with counts table by ASV
test_tab.troph = right_join( #Using right_join to filter by ASV tab BUT keep all non-numeric in consecutive cols at beginning of table
    tax_w_troph,
    data.frame(ASV = rownames(test_tab), test_tab),
    by = "ASV"
)

#Total read counts per group
test_tab.troph.count_sum = test_tab.troph %>% group_by(primary) %>% dplyr::summarize_if(is.numeric, sum)

#For richness per group first convert to presence-absence matrix and then sum as above
test_tab.troph.presAbs = test_tab.troph[,12:ncol(test_tab.troph)]
test_tab.troph.presAbs[test_tab.troph.presAbs > 0] = 1
test_tab.troph.presAbs.names = cbind(test_tab.troph[,1:11], test_tab.troph.presAbs)
test_tab.troph.rich = test_tab.troph.presAbs.names %>% group_by(primary) %>% dplyr::summarize_if(is.numeric, sum)

test_tab.troph.rich[,2:ncol(test_tab.troph.rich)] %>% rowSums


