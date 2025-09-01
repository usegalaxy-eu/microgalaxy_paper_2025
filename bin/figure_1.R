
## Load data analytics `libraries`


library(data.table)
library(stringr)


## Input data 


df <- "https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_technical_paper_2024/refs/heads/main/docs/supplementary/supplementary_table_1.tsv" |> fread()


### Correct column names 


df$V1 <- NULL

df$Timestamp <- df$Timestamp |> str_split_i("\\ ", 1) |> lubridate::as_date(format = "%d/%m/%Y")

df$`Which country are you based in?` <- df$`Which country are you based in?` |>
    str_to_title() |>
    str_split_i("\\,", 1) |>
    str_squish() |>
    str_replace("Bazil", "Brazil") |>
    str_replace("Hunagry", "Hungary") |>
    str_replace("England", "United Kingdom")


### Add continent 


df$Continent <- df$`Which country are you based in?` |> countrycode::countrycode(origin = "country.name", destination = "continent")


### Columns of interest 


coi <- c(
    "Timestamp", 
    "What is your main target?",
    "Which techniques do you use?",
    "Which analysis do you use or would like to do?",
    "Galaxy Obstacles",
    "Which country are you based in?",
    "Continent",
    "If the tools, platforms or databases from the previous question could be deployed in Galaxy would use prefer to use in there?"
)

df_1 <- df[, coi, with = FALSE]

colnames(df_1) = c(
    "Timestamp", 
    "MainTarget",
    "Techniques",
    "Analysis",
    "ReasonNotUse",
    "Country",
    "Continent",
    "PreferenceToUse"
)


## What is your main target? 


p1 <- df_1$MainTarget |> 
    str_remove_all("\\(|Protozoa, Helminths|Plasmodium falciparum|\\)") |>  
    str_to_title() |>
    str_replace("Viruses", "Virus") |>
    str_replace("And Fungal Pathogens", "Fungal Pathogens") |>
    str_split("\\,") |> 
    lapply(str_squish) |> 
    lapply(function(x) data.table("target" = x)) |> 
    rbindlist(idcol = "id")

p1 <- p1[which(target != ""), by = target, .(N = id |> unique() |> length())]

p1$group <- ifelse(p1$N > 1, p1$target, "Other")

p1 <- p1[, by = group, .( N = N |> sum())]

p1$Prop <- p1$N / sum(p1$N)


## Which techniques do you use? 


p2 <- df_1$Techniques |>
    str_to_title() |>
    str_remove("I Am Trying To Learn! Mostly Outsourced To Service Providers But Need To Learn As I Have Observed Inconsistent Results") |>
    str_remove_all("Amplicon|\\/") |>
    str_replace("Chip-Seq", "ChIP-seq") |>
    str_replace("Proteomics Or Metaproteomics", "(Meta)Proteomics") |>
    str_replace("Transcriptomics Or Metatranscriptomics", "(Meta)Transcriptomics") |>
    str_replace("Metatranscriptomics", "(Meta)Transcriptomics") |>
    str_replace("Single Organism Transcriptomics", "(Meta)Transcriptomics") |>
    str_replace("Md Simulations", "MD simulations") |>
    str_split("\\,") |>
    lapply(str_squish) |>
    lapply(function(x) data.table("technique" = x)) |>
    rbindlist(idcol = "id")


p2 <- p2[which(technique != ""), by = technique, .(N = id |> unique() |> length())]

p2$group <- ifelse(p2$N > 1, p2$technique, "Other")

p2 <- p2[, by = group, .( N = N |> sum())]

p2$Prop <- p2$N / sum(p2$N)


## Which analysis do you use or would like to do? 


p3 <- df_1$Analysis |>
    str_to_title() |>
    str_replace("Gwas", "GWAS") |>
    str_replace("Mag", "MAG") |>
    str_replace("Snp", "SNP") |>
    str_replace("Mlst", "MLST") |>
    
    str_replace("MAGs Building", "Metagenome Assembled Genome (MAG) Building") |>
    str_replace("SNP Identification", "Single Nucleotide Polymorphism (SNP) Identification") |>
    str_replace("Transcriptome Assembler", "Transcriptome Assembly") |>
    
    str_split("\\,") |>
    lapply(str_squish) |>
    lapply(function(x) data.table("analysis" = x)) |>
    rbindlist(idcol = "id")


p3 <- p3[which(analysis != ""), by = analysis, .(N = id |> unique() |> length())]

p3$group <- ifelse(p3$N > 1, p3$analysis, "Other")

p3 <- p3[, by = group, .( N = N |> sum())]

p3$Prop <- p3$N / sum(p3$N)





## Galaxy Obstacles 

p4 <- df_1$ReasonNotUse |> 
    str_split("\\,") |>
    lapply(str_squish) |>
    lapply(function(x) data.table("reason" = x)) |>
    rbindlist(idcol = "id")

p4 <- p4[which(reason != ""), by = reason, .(N = id |> unique() |> length())]

p4$Prop <- p4$N / sum(p4$N)


## Which continent are you based in? 

p5 <- df_1[, by = Continent, .N]


p5$Continent <- p5$Continent |> str_replace_na("Unknown")


p5$Prop <- p5$N / sum(p5$N)


## If the tools, platforms or databases from the previous question could be deployed in Galaxy would use prefer to use in there?


p6 <- df_1[, by = PreferenceToUse, .N]

p6[which(PreferenceToUse == "")]$PreferenceToUse = "Unknown"

p6$Prop <- p6$N / sum(p6$N)


## Merge data table lsit -----------

df_plot <- list(
    "What is your main target?"                                = p1,
    "Which techniques do you use?"                             = p2,
    "Which analysis do you use or would like to do?"           = p3,
    # "If you never or rarely use Galaxy, what are the reasons?" = p4,
    # "Survey answer by continent"                               = p5,
    "If the tools, platforms or databases from the previous question could be deployed in Galaxy would use prefer to use in there?" = p6
    
) |>
    lapply(function(x) {
        
        colnames(x) <- c("ylabel", "N", "Prop")
        
        x <- x[order(-N)]
        
        x$label <- x$Prop |> round(digits = 4) |> scales::percent()
        
        return(x)
    }) |>
    rbindlist(idcol = "question")

## Output Figure 

### Load plotting `libraries`


library(ggplot2)
library(ggh4x)

library(shadowtext)

library(paletteer)
library(tidytext)

library(extrafont)


### Plot

df_plot$question2 <- df_plot$question |> str_wrap(width = 30)

df_plot$question2 <- df_plot$question2 |> factor(levels = df_plot$question2 |> unique())

gr <- df_plot |>
    ggplot(aes(x = N, y = reorder_within(ylabel, N, question2))) +
    
    geom_point(size = 2.95 + 2.20, aes(fill = Prop), shape = 21, stroke = .25, color = "grey25") +
    geom_col(width = .75, aes(fill = Prop), lineend = "round", color = "grey25", linewidth = .15) +
    geom_point(size = 2.71 + 2.20, aes(fill = Prop, color = Prop), shape = 21, stroke = .25) +
    
    
    geom_shadowtext(aes(label = label), size = 3, hjust = 0, vjust = .5, position = position_nudge(x = 4.5),
                    bg.r = .05, bg.color = "white", color = "black") +

    scale_y_reordered() +
    scale_x_continuous(expand = c(0, 0), 
                       limits = c(0, 123), 
                       breaks = c(10, 25, 50, 75, 100), 
                       minor_breaks = c(17.5, 37.5, 62.5, 87.5, 112.5)) +
    
    facet_grid2(vars(question2), scales = "free_y", space = "free_y") +
    
    scale_fill_stepsn(colors = paletteer_c("ggthemes::Red-Blue Diverging", 5, direction = -1),
                      breaks = c(.2, .4, .6, .8),
                      limits = c(0, .8),
                      labels = scales::percent,
                      guide = guide_colorsteps(barwidth = unit(12, "lines"), 
                                               barheight = unit(.5, "lines"))) +
    
    scale_color_stepsn(colors = paletteer_c("ggthemes::Red-Blue Diverging", 5, direction = -1),
                       breaks = c(.2, .4, .6, .8),
                       limits = c(0, .8),
                       guide = "none") +
    
    # scale_fill_gradient(low = "#4E79A7" |> lighten(.25), high = "#E15759" |> darken(.3)) +
    # scale_color_gradient(low = "#4E79A7" |> lighten(.25), high = "#E15759" |> darken(.3)) +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_family = "Calibri") +
    theme(
        # legend.position.inside = TRUE,
        legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(family = "Calibri"),
        
        strip.clip = "off",
        strip.text.y.right = element_text(face = "bold", angle=0, vjust=.5, hjust = 0),
        strip.background = element_blank(),
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5)),
        
        panel.grid.major.y = element_blank(),
        
        panel.grid.minor.x = element_line(color = "grey85", linetype = "dotted", lineend = "round"),
        panel.grid.major.x = element_line(color = "grey85"),
        
        panel.background = element_rect(fill = NA, color = "grey85"),
        
        axis.line = element_line(lineend = "round"),
        axis.ticks = element_line(lineend = "round")
    ) +
    
    labs(x = "Number of Responses", fill = "Proportion")

df_plot$question2 <- NULL



### Citations part



all_papers <- fread("https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/results/citations/all_papers.csv")
microbial_papers <- fread("https://raw.githubusercontent.com/usegalaxy-eu/microgalaxy_paper_2025/refs/heads/main/results/citations/microbial_papers.csv")


all_papers <- all_papers[which(year >= 2006)]
microbial_papers <- microbial_papers[which(year >= 2006)]



### Barplot


library(ggstream)
library(ggtext)
library(colorspace)

# p0 <- all

p0 <- all_papers[, by = year, .(counts = title |> unique() |> length())]
p1 <- microbial_papers[, by = .(year, Topics), .(counts = title |> unique() |> length())]

p0$Topics <- "All"

gr1_df <- rbind(p0, p1)

# gr1_df$counts2 <- log10(gr1_df$counts)

gr1_df$Topics <- gr1_df$Topics |> factor(levels = c("Ecosystem Dynamics & Biodiversity",
                                                    "Ecosystem Dynamics & Biodiversity, Health & Disease",
                                                    "Health & Disease",
                                                    "All"))

gr1 <- gr1_df |>
    ggplot(aes(year, counts)) +
    geom_stream(aes(fill = Topics), color = "grey25", linewidth = .25, type = "ridge") +
    geom_vline(xintercept = c(2010, 2015, 2017, 2020, 2023), linewidth = .5, linetype = "dotted", lineend = "round", color = "white") +

    # geom_stream(aes(fill = Topics), type = "ridge") +
    
    # geom_line(aes(color = variable, group = variable)) +
    
    # geom_point(aes(fill = Topics), color = "grey25",
    #            shape = 21, size = 5.3 - 1.23, stroke = .25,
    #            position = position_dodge(width = .9)) +

    # geom_col(aes(fill = Topics), color = "grey25",
    #          linewidth = .15, position = "stack") +

    # geom_point(aes(color = Topics), size = 4.65 - 1.23,
    #            position = position_dodge(width = .9)) +
    
    scale_x_continuous(expand = c(0, 0), breaks = c(2006, 2010, 2015, 2017, 2020, 2023, 2025)) +
    scale_y_continuous(breaks = seq(100, 1350, by = 100), expand = c(0, 0), limits = c(0, 1350)) +
    # scale_y_continuous(expand = c(0, 0), breaks = c(10, 20, 50, 100, 200, 500, 1000)) +
    # scale_y_continuous(expand = c(0, 0), transform = "log2", limits = c(0, 1100), breaks = seq(200, 1000, by = 200)) +
    
    scale_fill_manual(values = c("All" = "grey" |> lighten(.25),
                                 "Ecosystem Dynamics & Biodiversity" = "#7ca07aff",
                                 "Ecosystem Dynamics & Biodiversity, Health & Disease" = "#BCB45D",
                                 "Health & Disease" = "#fbc73fff")) +
    # scale_color_manual(values = c("All" = "#4E79A7" |> lighten(.25), 
    #                               "Microbial" = "#E15759" |> darken(.3))) +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_family = "Calibri") +
    
    theme(
        legend.position = "inside",
        legend.position.inside = c(.3, .75),
        
        # axis.line.x = element_line(lineend = "round"),
        # axis.ticks.y = element_line(lineend = "round"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        
        panel.grid.minor.y = element_line(linetype = "dashed", lineend = "round"),
        
        axis.text.x = element_text(face = "bold", size = 12),
        
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_markdown(margin = margin(r = 10))
    ) +
    
    labs(x = "Year", y = "**Number of publications** citing Galaxy papers")


### Output files



library(patchwork)

multi <- (free(gr1) | gr) +
    plot_layout(widths = c(2, 1)) +
    plot_annotation(tag_levels = "A") &
    theme(
        plot.tag = element_text(size = 14, face = "bold")
    )



outfolder <- "../docs/extended/"

dir.create(outfolder, showWarnings = FALSE)

save_plot <- function(plot, filename, w, h) {
    
    ggsave(
        plot = plot, filename = paste0(filename, ".png"),
        width = w, height = h, units = "in", dpi = 600
    )
    
    ggsave(
        plot = plot, filename = paste0(filename, ".svg"),
        width = w, height = h, units = "in", dpi = 600
    )
    
    ggsave(
        plot = plot, filename = paste0(filename, ".pdf"),
        width = w, height = h, units = "in", device = cairo_pdf
    )
    
}

save_plot(multi, "../docs/extended/figure_1", 16, 8)
fwrite(df_plot, "../docs/extended/survey_descriptive_stats.txt", sep = "\t", row.names = FALSE, quote = FALSE)




















