
## SETUP -----

# load libraries
library(readxl)
library(gridExtra)
library(ggplot2)
library(grid)
library(forestplot)

# set working directory
setwd("G:/DCAC/DCAC_PEPFAR/RV329/Analyses/NCD/Matt/tables")


## FOREST PLOT -----

# load data
#data <- read_excel("untrt_htn_model_v3.xlsx")
data <- read_excel("untrt_dm_model_v3.xlsx")
names(data)

# indent categories of each variable
data$Group <- ifelse(is.na(data$Flag), 
                      data$Group,
                      paste0("        ", data$Group))

# create a confidence interval column to display
data$`RR (95% CI)` <- ifelse(is.na(data$RR), "", paste0(sprintf("%.02f", data$RR), " (", 
                                                        sprintf("%.02f", data$LL), "-", 
                                                        sprintf("%.02f", data$UL), ")"))

data$`aRR (95% CI)` <- ifelse(is.na(data$RRadj), "", paste0(sprintf("%.02f", data$RRadj), " (", 
                                                        sprintf("%.02f", data$LLadj), "-", 
                                                        sprintf("%.02f", data$ULadj), ")"))

unadj <- forestplot(data[,c(2,12)],
            mean = data$RR,
            lower = data$LL, 
            upper = data$UL,
            boxsize = 0.3,
            line.margin = 0.1,
            is.summary = FALSE,
            xlog = TRUE,
            clip = c(0.1, 2),
            zero = 1,
            xticks = c(-0.69, 0, 0.695),
            txt_gp = fpTxtGp(ticks=gpar(cex=0.8), xlab=gpar(cex=1), label=gpar(cex=0.9)),
            xlab = "Unadjusted Risk Ratio (95% CI)",
            title = "Unadjusted Risk Ratio (95% CI)")
plot(unadj)

adj <- forestplot(data[,c(2,13)],
                  mean = data$RRadj,
                  lower = data$LLadj, 
                  upper = data$ULadj,
                  boxsize = 0.3,
                  line.margin = 0.1,
                  is.summary = FALSE,
                  xlog = TRUE,
                  clip = c(0.1, 2),
                  zero = 1,
                  xticks = c(-0.69, 0, 0.695),
                  txt_gp = fpTxtGp(ticks=gpar(cex=0.8), xlab=gpar(cex=1), label=gpar(cex=0.9)),
                  xlab = "Adjusted Risk Ratio (95% CI)",
                  title = "Adjusted Risk Ratio (95% CI)")
plot(adj)

# export
#png('untreated_htn_forestplot_v2.png', res = 300, width = 20, height = 15, units = "in")
png('untreated_dm_forestplot_v2.png', res = 300, width = 20, height = 15, units = "in")

grid.newpage()
borderWidth <- unit(4, "pt")
width <- unit(convertX(unit(1, "npc") - borderWidth, unitTo = "npc", valueOnly = TRUE)/2, "npc")
pushViewport(viewport(layout = grid.layout(nrow = 1, 
                                           ncol = 3, 
                                           widths = unit.c(width,
                                                           borderWidth,
                                                           width))
)
)
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 1))
unadj |> fp_set_style(line = "black")
upViewport()
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 2))
grid.rect(gp = gpar(fill = "#dddddd", col = "#eeeeee"))
upViewport()
pushViewport(viewport(layout.pos.row = 1,
                      layout.pos.col = 3))
adj |> fp_set_style(line = "black")
upViewport(2)

dev.off()


## REFERENCES -----
# https://cran.r-project.org/web/packages/forestplot/vignettes/forestplot.html#ticks-and-grids
# https://search.r-project.org/CRAN/refmans/forestplot/html/fpTxtGp.html
# https://rpubs.com/mbounthavong/forest_plots_r
# https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
# https://stackoverflow.com/questions/15420621/reproduce-table-and-plot-from-journal