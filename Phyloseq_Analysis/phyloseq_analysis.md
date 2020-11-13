Phyloseq Analysis
================
Kai Oda
11/12/2020

``` r
# BiocManager::install("phyloseq")
```

## Load in the necessary libraries

``` r
library(tidyverse)
library(phyloseq)
library(RColorBrewer)
```

## Import Data from weeks 4 and 5

``` r
count.tab <- read_rds("../DaDa2/seqtab-nochimtaxa.rds") 
tax.tab <- read_rds("../DaDa2/taxa.rds") 
sample.tab <- read_rds("../Bacterial_Abundance/bacterialCarbon.RDS") %>% 
  drop_na(DNA_SampleID) %>% 
  column_to_rownames(var = "DNA_SampleID") 
```

## Create a phyloseq object

Essentially, all we are doing is combining the three datasets together

``` r
OTU = otu_table(count.tab, taxa_are_rows = TRUE) 
TAX = tax_table(tax.tab)
SAM = sample_data(sample.tab)
ps = phyloseq(OTU,TAX,SAM) 
```

## Filtering out chloroplasts and mitochondria

We only want bacteria in our phyloseq object, so let’s remove
chloroplast and mitochondria taxa with the subset\_taxa() command

``` r
sub_ps <- ps %>%
  subset_taxa(Family  != "mitochondria" & Order  != "Chloroplast")
```

## Plotting distribution of read lengths

``` r
sample_sum_df <- data.frame(sum = sample_sums(sub_ps))

ggplot(sample_sum_df, aes(x=sum))+
  geom_histogram(color="black", fill="white", binwidth = 5000)+
  xlab("Read counts")+
  ylab("Occurences")+
  ggtitle("Distribution of sample sequencing depth")+
  theme_bw()
```

![](phyloseq_analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
## Outputs mean, min and max of the read counts from the dataframe
summary(sample_sum_df)
```

    ##       sum       
    ##  Min.   : 2064  
    ##  1st Qu.:23420  
    ##  Median :28835  
    ##  Mean   :28809  
    ##  3rd Qu.:33571  
    ##  Max.   :53948

One thing to note from this output: the mean read length is
SIGNIFICANTLY larger than the minimum read length (different from the
videos).

This means that when we rarefy the data there will be a massive effect
due to subsampling. Essentially, we are taking a dataset that has an
average of 28000+ in read length, and only randomly sampling 2064
sequences from it. This seems like a LOT of lost data but we push on.

## Rarefying the data

As I understand from the video series:

  - In order to calculate beta diversity we need to calculate pairwise
    comparisons between the data
  - Pairwise comparisons are influenced by read depth
  - Therefore, it is best practice to normalize/standardize read depth
    across samples
  - This standardization process is called “rarefying” data
  - Essentially, we scale to the smallest library size and sub sample
    all sequences until we have a library equal to the smallest size…

<!-- end list -->

``` r
ps_min <-  rarefy_even_depth(sub_ps, sample.size = min(sample_sums(sub_ps)))
```

    ## You set `rngseed` to FALSE. Make sure you've set & recorded
    ##  the random seed of your session for reproducibility.
    ## See `?set.seed`

    ## ...

    ## 140OTUs were removed because they are no longer 
    ## present in any sample after random subsampling

    ## ...

``` r
mean(sample_sums(sub_ps))#28808.5
```

    ## [1] 28808.5

``` r
mean(sample_sums(ps_min)) #2064 
```

    ## [1] 2064

## Ordinating and visualizing beta diversity

``` r
## Calculating the stress level for the non-subsampled datset 
set.seed(1)
nmds <- ordinate(sub_ps, method = "NMDS",  distance = "bray") 
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.07478252 
    ## Run 1 stress 0.07737625 
    ## Run 2 stress 0.15681 
    ## Run 3 stress 0.07497276 
    ## ... Procrustes: rmse 0.005616706  max resid 0.02034819 
    ## Run 4 stress 0.08305166 
    ## Run 5 stress 0.07594774 
    ## Run 6 stress 0.07563811 
    ## Run 7 stress 0.1625683 
    ## Run 8 stress 0.07768767 
    ## Run 9 stress 0.1639005 
    ## Run 10 stress 0.07563811 
    ## Run 11 stress 0.1189943 
    ## Run 12 stress 0.07478232 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0001755016  max resid 0.0006976295 
    ## ... Similar to previous best
    ## Run 13 stress 0.1685886 
    ## Run 14 stress 0.08452349 
    ## Run 15 stress 0.07768767 
    ## Run 16 stress 0.07563811 
    ## Run 17 stress 0.1767327 
    ## Run 18 stress 0.1588891 
    ## Run 19 stress 0.08294498 
    ## Run 20 stress 0.07563814 
    ## *** Solution reached

Stress level for the **nonsubsampled dataset is 0.076.** This is lower
than the cutoff (0.2). Therefore, this is a reasonably good translation
of multi-dimensional data to a 2D space.

``` r
set.seed(1)
nmds_min <- ordinate(ps_min, method = "NMDS",  distance = "bray") 
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.08107243 
    ## Run 1 stress 0.08065208 
    ## ... New best solution
    ## ... Procrustes: rmse 0.01459754  max resid 0.05436779 
    ## Run 2 stress 0.08107226 
    ## ... Procrustes: rmse 0.014631  max resid 0.05444873 
    ## Run 3 stress 0.08130101 
    ## Run 4 stress 0.08065353 
    ## ... Procrustes: rmse 0.000364251  max resid 0.001279838 
    ## ... Similar to previous best
    ## Run 5 stress 0.1534458 
    ## Run 6 stress 0.08844628 
    ## Run 7 stress 0.1704192 
    ## Run 8 stress 0.08065283 
    ## ... Procrustes: rmse 0.0002286006  max resid 0.000805836 
    ## ... Similar to previous best
    ## Run 9 stress 0.172952 
    ## Run 10 stress 0.1663489 
    ## Run 11 stress 0.136938 
    ## Run 12 stress 0.08130094 
    ## Run 13 stress 0.1702487 
    ## Run 14 stress 0.156142 
    ## Run 15 stress 0.08065217 
    ## ... Procrustes: rmse 4.886625e-05  max resid 0.0001510708 
    ## ... Similar to previous best
    ## Run 16 stress 0.1459539 
    ## Run 17 stress 0.1722936 
    ## Run 18 stress 0.08844628 
    ## Run 19 stress 0.08844628 
    ## Run 20 stress 0.08844628 
    ## *** Solution reached

Stress level for the subsampled dataset is **0.08**. This means that the
subsampled datset is also a good translation from multi-dimensional data
to 2D space.

## Plotting beta diversity

``` r
levels <- c("Control", "Ash Leachate", "Mud Leachate", "Glucose_Nitrate_Phosphate")

nmds.plot <- plot_ordination(sub_ps, nmds, title="NMDS Nonsubsampled")+
  geom_point(aes(fill=days, shape = factor(Treatment, levels = levels)), alpha = 0.6, stroke = 2, size = 4)+
  scale_shape_manual(values = c(21, 22, 23, 24))+
  scale_fill_gradient(low = "blue", high = "#d31f2a")+
  theme_bw()

#remove the point layer (removes the point within the point)
nmds.plot$layers <- nmds.plot$layers[-1]

nmds.plot + 
  facet_grid(~Treatment)+
  guides(fill = guide_colorbar(title="Days"), shape= guide_legend(title="Treatment"))
```

![](phyloseq_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
nmds_min.plot <- plot_ordination(ps_min, nmds_min, title="NMDS Subsampled")+
  geom_point(aes(fill=days, shape = factor(Treatment, levels = levels)), alpha = 0.6, stroke = 2, size = 4)+
  scale_shape_manual(values = c(21, 22, 23, 24))+
  scale_fill_gradient(low = "blue", high = "#d31f2a")+
  theme_bw()

#remove the point layer (removes the point within the point)
nmds_min.plot$layers <- nmds_min.plot$layers[-1]

nmds_min.plot + 
  facet_grid(~Treatment)+
  guides(fill = guide_colorbar(title="Days"), shape= guide_legend(title="Treatment"))
```

![](phyloseq_analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
