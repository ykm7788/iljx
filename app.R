
options(encoding = "UTF-8",expressions = 10000,max.print = 999999)
library(ggpubr)
library(shiny)
library(ggthemes)
library(RColorBrewer)
library(ggsci)
library(Rmisc)
library("ape")
library("vegan")

theme_ykm <- function(..., bg = "white") {
  theme_few() +
    theme(
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      axis.text.x = element_text(size = 8, color = "black"),
      axis.text.y = element_text(size = 8, color = "black"),
      axis.title.y = element_text(colour = "black", size = 10),
      axis.title.x = element_text(colour = "black", size = 10),
      legend.text = element_text(size = 8)
    )
}

plot.parameter <- function(data, parameter) {
  if (is.na(parameter) != T) {
    data[, parameter] <- as.factor(as.character(data[, parameter]))
    out.parameter <- data[, parameter]
  } else {
    out.parameter <- as.factor(rep("1", nrow(data)))
  }
  return(out.parameter)
}

plot.raw <- function(sa.data, input, rawplot) {
  sa.rawplot <- rawplot +
    theme_ykm() +
    xlab(colnames(sa.data)[input$sa.x]) +
    ylab(colnames(sa.data)[input$sa.y])

  if (is.na(input$sa.title) != T) {
    sa.rawplot <- sa.rawplot + labs(title = input$sa.title)
  }
  if (input$sa.fill.palette == "npg") {
    sa.rawplot <- sa.rawplot + scale_fill_npg()
  }
  if (input$sa.fill.palette == "aaas") {
    sa.rawplot <- sa.rawplot + scale_fill_aaas()
  }
  if (input$sa.fill.palette == "nejm") {
    sa.rawplot <- sa.rawplot + scale_fill_nejm()
  }
  if (input$sa.fill.palette == "lancet") {
    sa.rawplot <- sa.rawplot + scale_fill_lancet()
  }
  if (input$sa.fill.palette == "jama") {
    sa.rawplot <- sa.rawplot + scale_fill_jama()
  }
  if (input$sa.fill.palette == "jco") {
    sa.rawplot <- sa.rawplot + scale_fill_jco()
  }
  if (input$sa.fill.palette == "igv") {
    sa.rawplot <- sa.rawplot + scale_fill_igv()
  }
  if (input$sa.fill.palette == "uchicago") {
    sa.rawplot <- sa.rawplot + scale_fill_uchicago()
  }
  if (input$sa.fill.palette == "ucscgb") {
    sa.rawplot <- sa.rawplot + scale_fill_ucscgb()
  }
  if (input$sa.fill.palette == "simpsons") {
    sa.rawplot <- sa.rawplot + scale_fill_simpsons()
  }
  if (input$sa.fill.palette == "rickandmorty") {
    sa.rawplot <- sa.rawplot + scale_fill_rickandmorty()
  }
  if (input$sa.fill.palette == "brewer") {
    sa.rawplot <- sa.rawplot + scale_fill_brewer(palette = "Paired")
  }
  if (input$sa.fill.palette == "discrete") {
    sa.rawplot <- sa.rawplot + scale_fill_discrete()
  }
  if (input$sa.fill.palette == "continuous") {
    sa.rawplot <- sa.rawplot + scale_fill_continuous()
  }
  if (input$sa.color.palette == "npg") {
    sa.rawplot <- sa.rawplot + scale_color_npg()
  }
  if (input$sa.color.palette == "aaas") {
    sa.rawplot <- sa.rawplot + scale_color_aaas()
  }
  if (input$sa.color.palette == "nejm") {
    sa.rawplot <- sa.rawplot + scale_color_nejm()
  }
  if (input$sa.color.palette == "lancet") {
    sa.rawplot <- sa.rawplot + scale_color_lancet()
  }
  if (input$sa.color.palette == "jama") {
    sa.rawplot <- sa.rawplot + scale_color_jama()
  }
  if (input$sa.color.palette == "jco") {
    sa.rawplot <- sa.rawplot + scale_color_jco()
  }
  if (input$sa.color.palette == "igv") {
    sa.rawplot <- sa.rawplot + scale_color_igv()
  }
  if (input$sa.color.palette == "uchicago") {
    sa.rawplot <- sa.rawplot + scale_color_uchicago()
  }
  if (input$sa.color.palette == "ucscgb") {
    sa.rawplot <- sa.rawplot + scale_color_ucscgb()
  }
  if (input$sa.color.palette == "simpsons") {
    sa.rawplot <- sa.rawplot + scale_color_simpsons()
  }
  if (input$sa.color.palette == "rickandmorty") {
    sa.rawplot <- sa.rawplot + scale_color_rickandmorty()
  }
  if (input$sa.color.palette == "brewer") {
    sa.rawplot <- sa.rawplot + scale_color_brewer(palette = "Paired")
  }
  if (input$sa.color.palette == "discrete") {
    sa.rawplot <- sa.rawplot + scale_color_discrete()
  }
  if (input$sa.color.palette == "continuous") {
    sa.rawplot <- sa.rawplot + scale_color_continuous()
  }
  if (is.na(input$sa.facet) != T) {
    if (input$sa.facet.scale == T) {
      sa.rawplot <- sa.rawplot + facet_wrap(colnames(sa.data)[input$sa.facet], scales = "free")
    } else {
      sa.rawplot <- sa.rawplot + facet_wrap(colnames(sa.data)[input$sa.facet])
    }
  }
  return(sa.rawplot)
}

plot.ma.raw <- function(input, rawplot) {
  ma.rawplot <- rawplot +
    theme_ykm()

  if (is.null(input$ma.title) != T) {
    ma.rawplot <- ma.rawplot + labs(title = input$ma.title)
  }
  if (input$ma.fill.palette == "npg") {
    ma.rawplot <- ma.rawplot + scale_fill_npg()
  }
  if (input$ma.fill.palette == "aaas") {
    ma.rawplot <- ma.rawplot + scale_fill_aaas()
  }
  if (input$ma.fill.palette == "nejm") {
    ma.rawplot <- ma.rawplot + scale_fill_nejm()
  }
  if (input$ma.fill.palette == "lancet") {
    ma.rawplot <- ma.rawplot + scale_fill_lancet()
  }
  if (input$ma.fill.palette == "jama") {
    ma.rawplot <- ma.rawplot + scale_fill_jama()
  }
  if (input$ma.fill.palette == "jco") {
    ma.rawplot <- ma.rawplot + scale_fill_jco()
  }
  if (input$ma.fill.palette == "igv") {
    ma.rawplot <- ma.rawplot + scale_fill_igv()
  }
  if (input$ma.fill.palette == "uchicago") {
    ma.rawplot <- ma.rawplot + scale_fill_uchicago()
  }
  if (input$ma.fill.palette == "ucscgb") {
    ma.rawplot <- ma.rawplot + scale_fill_ucscgb()
  }
  if (input$ma.fill.palette == "simpsons") {
    ma.rawplot <- ma.rawplot + scale_fill_simpsons()
  }
  if (input$ma.fill.palette == "rickandmorty") {
    ma.rawplot <- ma.rawplot + scale_fill_rickandmorty()
  }
  if (input$ma.fill.palette == "brewer") {
    ma.rawplot <- ma.rawplot + scale_fill_brewer(palette = "Paired")
  }
  if (input$ma.fill.palette == "discrete") {
    ma.rawplot <- ma.rawplot + scale_fill_discrete()
  }
  if (input$ma.fill.palette == "continuous") {
    ma.rawplot <- ma.rawplot + scale_fill_continuous()
  }
  if (input$ma.color.palette == "npg") {
    ma.rawplot <- ma.rawplot + scale_color_npg()
  }
  if (input$ma.color.palette == "aaas") {
    ma.rawplot <- ma.rawplot + scale_color_aaas()
  }
  if (input$ma.color.palette == "nejm") {
    ma.rawplot <- ma.rawplot + scale_color_nejm()
  }
  if (input$ma.color.palette == "lancet") {
    ma.rawplot <- ma.rawplot + scale_color_lancet()
  }
  if (input$ma.color.palette == "jama") {
    ma.rawplot <- ma.rawplot + scale_color_jama()
  }
  if (input$ma.color.palette == "jco") {
    ma.rawplot <- ma.rawplot + scale_color_jco()
  }
  if (input$ma.color.palette == "igv") {
    ma.rawplot <- ma.rawplot + scale_color_igv()
  }
  if (input$ma.color.palette == "uchicago") {
    ma.rawplot <- ma.rawplot + scale_color_uchicago()
  }
  if (input$ma.color.palette == "ucscgb") {
    ma.rawplot <- ma.rawplot + scale_color_ucscgb()
  }
  if (input$ma.color.palette == "simpsons") {
    ma.rawplot <- ma.rawplot + scale_color_simpsons()
  }
  if (input$ma.color.palette == "rickandmorty") {
    ma.rawplot <- ma.rawplot + scale_color_rickandmorty()
  }
  if (input$ma.color.palette == "brewer") {
    ma.rawplot <- ma.rawplot + scale_color_brewer(palette = "Paired")
  }
  if (input$ma.color.palette == "discrete") {
    ma.rawplot <- ma.rawplot + scale_color_discrete()
  }
  if (input$ma.color.palette == "continuous") {
    ma.rawplot <- ma.rawplot + scale_color_continuous()
  }
  return(ma.rawplot)
}

locate.new.col <- function(olddata, oldcol, newdata) {
  new.col <- as.numeric(grep(pattern = as.character(colnames(olddata)[oldcol]), colnames(newdata)))
  return(new.col)
}

dataframe2matrix  <- function(input){
  dataframe = input
  rownames(dataframe) = dataframe[,1]
  dataframe = dataframe[,-1]
  return(dataframe)
}

#############################  Statistical Analysis  #################################

panel.sa.data <- tabPanel(
  "Statistical Data  ",
  fileInput("sa.file", "Choose text File",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  ),
  tags$hr(),
  checkboxInput("sa.header", "First row as header", TRUE),
  checkboxInput("sa.checkname", "Checkname (default:F)", F),
  textInput("sa.sep", "Separator (default:tab)",
    value = "\t",
    placeholder = NULL
  ),
  textInput("sa.quote", "Quote marker (default:Null)",
    value = "",
    placeholder = NULL
  )
)

panel.general <- tabPanel(
  "General   ",
  numericInput("sa.x", "col num. for x", value = ""),
  numericInput("sa.y", "col num. for y", value = ""),
  selectInput(
    "sa.fill.palette", "color palette for fill",
    c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "igv", "uchicago", "simpsons", "rickandmorty", "brewer", "discrete", "continuous", "default")
  ),
  selectInput(
    "sa.color.palette", "color palette for color",
    c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "igv", "uchicago", "simpsons", "rickandmorty", "brewer", "discrete", "continuous", "default")
  ),
  numericInput("sa.facet", "col num. for facet, empty for NO facet", value = ""),
  checkboxInput("sa.facet.scale", "Scare free ?", TRUE),
  textInput("sa.title", "Plot title", value = ""),
  numericInput("sa.plot.width", "Plot width for download, inch", value = "4"),
  numericInput("sa.plot.height", "Plot height for download, inch", value = "4")
)

panel.statistic <- tabPanel(
  "Statistic   ",
  selectInput("sa.stat.method", "Comparison method",
    c("anova", "t.test", "wilcox.test"),
    selected = NULL
  ),
  selectInput("sa.stat.label", "Significance layout",
    c("p.signif", "p.format"),
    selected = NULL
  ),
  checkboxInput("sa.stat.paired", "Paired comparison ?", F),
  selectInput("sa.stat.type", "Comparison type",
    c("Within X-axis group comparison", "Reference comparison", "Between X-axis group comparison"),
    selected = NULL
  ),
  helpText("For ' Within X-axis group cpmparison '. Enter col num. which contains all group names."),
  numericInput("sa.stat.group", "col num. for group names", value = ""),
  helpText("For ' Reference comparison '. Reference group vs other groups. Enter Reference group name.'anova' method is unavailable here."),
  textInput("sa.stat.ref", "Reference group name for comparison", value = ""),
  helpText("For ' Between X-axis group cpmparison '. Use ',' to separate group names in each comparison pair. Use ';' to separate different comparison pairs. e.g. 'A,B;B,C' to compare A-B and B-C. 'anova' method is unavailable here."),
  textInput("sa.stat.compar", "Comparison groups", value = "")
)

panel.box <- tabPanel(
  "Box plot",
  numericInput("sa.box.fill", "Column num. for fill, empty for default", value = ""),
  checkboxInput("sa.box.jitter", "plot jitter ?", F),
  checkboxInput("sa.box.stat", "Comparison ?", F)
)

panel.violin <- tabPanel(
  "Violin plot",
  numericInput("sa.violin.fill", "Column num. for fill, empty for default", value = ""),
  checkboxInput("sa.violin.jitter", "plot jitter ?", F),
  checkboxInput("sa.violin.stat", "Comparison ?", F),
  checkboxInput("sa.violin.box", "plot box ?", F),
  numericInput("sa.violin.boxwidth", "Boxwidth , default = 0.5", value = "0.5"),
  numericInput("sa.violin.boxdodge", "Box interval , default = -0.9", value = "-0.9")
)

panel.density <- tabPanel(
  "Density plot",
  selectInput("sa.density.type", "Histogram or density",
    c("Histogram", "Density"),
    selected = NULL
  ),
  numericInput("sa.density.fill", "Column num. for fill, empty for default", value = ""),
  checkboxInput("sa.density.rug", "plot rug ?", T),
  checkboxInput("sa.density.mean", "plot mean ?", T),
  numericInput("sa.density.bin", "Bin num. , empty = 30 ", value = "30")
)

panel.bar <- tabPanel(
  "Bar plot",
  numericInput("sa.bar.barfill", "Column num. for bar fill, empty for default", value = ""),
  selectInput("sa.bar.bartype", "Stack or Dodge ",
    c("stack", "dodge "),
    selected = NULL
  ),
  numericInput("sa.bar.dodge", "Dodge width ? , default = 0.6 ", value = "0.6"),
  numericInput("sa.bar.barwidth", "bar width ? , default = 0.5 ", value = "0.5"),
  checkboxInput("sa.bar.mean", "Plot mean data ?", F),
  helpText("Use ',' to separate col num. for group_by mean. e.g. '1,2' for mean by groups in both col 1 and col 2"),
  textInput("sa.bar.groups", "Groups of mean", value = ""),
  checkboxInput("sa.bar.errorbar", "Errorbar ?", F),
  selectInput("sa.bar.errorbartype", "Errorbar = sd/se",
    c("sd", "se"),
    selected = NULL
  )
)

panel.line <- tabPanel(
  "Line plot",
  numericInput("sa.line.linecolor", "Column num. for line color, empty for default", value = ""),
  numericInput("sa.line.linetype", "Column num. for line type, empty for default", value = ""),
  numericInput("sa.line.linewidth", "Line width ? , default = 1 ", value = "1"),
  numericInput("sa.line.dotsize", "Dot size ? , default = 3 ", value = "3"),
  numericInput("sa.line.dodge", "Position dodge , default = 0 ", value = "0"),
  checkboxInput("sa.line.mean", "Plot mean data ?", F),
  helpText("Use ',' to separate col num. for group_by mean. e.g. '1,2' for mean by groups in both col 1 and col 2"),
  textInput("sa.line.groups", "Groups of mean", value = ""),
  checkboxInput("sa.line.errorbar", "Errorbar ?", F),
  selectInput("sa.line.errorbartype", "Errorbar = sd/se",
    c("sd", "se"),
    selected = NULL
  )
)

panel.lm <- tabPanel(
  "Liner fit plot",
  numericInput("sa.lm.lmcolor", "Column num. for line color, empty for default", value = ""),
  numericInput("sa.lm.lmtype", "Column num. for line type, empty for default", value = ""),
  numericInput("sa.lm.lmwidth", "lm width ? , default = 1 ", value = "1"),
  numericInput("sa.lm.dotsize", "Dot size ? , default = 3 ", value = "3"),
  selectInput("sa.lm.method", "Smoothing method",
    c("lm", "glm", "gam", "loess"),
    selected = NULL
  )
)


panel.sa <- tabPanel(
  "Statistical Analysis",
  sidebarPanel(
    tabsetPanel(
      panel.sa.data,
      panel.general,
      panel.statistic,
      panel.box,
      panel.violin,
      panel.density,
      panel.bar,
      panel.line,
      panel.lm
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Data",
        tableOutput("sa.contents")
      ),
      tabPanel(
        "Box plot",
        plotOutput("sa.boxplot", height = "600px", width = "600px"),
        downloadButton('download.sa.boxplot','Download Plot')
      ),
      tabPanel(
        "Violin plot",
        plotOutput("sa.violinplot", height = "600px", width = "600px"),
        downloadButton('download.sa.violinplot','Download Plot')
      ),
      tabPanel(
        "Density plot",
        plotOutput("sa.densityplot", height = "600px", width = "600px"),
        downloadButton('download.sa.densityplot','Download Plot')
      ),
      tabPanel(
        "Bar plot",
        plotOutput("sa.barplot", height = "600px", width = "600px"),
        downloadButton('download.sa.barplot','Download Plot')
      ),
      tabPanel(
        "Line plot",
        plotOutput("sa.lineplot", height = "600px", width = "600px"),
        downloadButton('download.sa.lineplot','Download Plot')
      ),
      tabPanel(
        "Liner fit plot",
        plotOutput("sa.lmplot", height = "600px", width = "600px"),
        downloadButton('download.sa.lmplot','Download Plot')
      )
    )
  )
)


#############################  Metagenomic Analysis  #################################

panel.ma.data <- tabPanel(
  "Data Upload  ",
  helpText("First row as sample name, first col as species name, duplication is forbidden"),
  fileInput("ma.otu", "Choose OTU File",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  ),
  tags$hr(),
  helpText("First row as sample name, first col as group name, each row represents a group mode, leave empty blank for samples you don't want to plot"),
  fileInput("ma.group", "Choose group File",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  ),
  tags$hr(),
  checkboxInput("ma.header", "First row as header", TRUE),
  checkboxInput("ma.checkname", "Checkname (default:F)", F),
  textInput("ma.sep", "Separator (default:tab)",
    value = "\t",
    placeholder = NULL
  ),
  textInput("ma.quote", "Quote marker (default:Null)",
    value = "",
    placeholder = NULL
  )
)

panel.ma.general <- tabPanel(
  "General   ",
  numericInput("ma.groupcol", "col num. for group, empty for no group", value = ""),
  selectInput(
    "ma.fill.palette", "color palette for fill",
    c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "igv", "uchicago", "simpsons", "rickandmorty", "brewer", "discrete", "continuous", "default")
  ),
  selectInput(
    "ma.color.palette", "color palette for color",
    c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "igv", "uchicago", "simpsons", "rickandmorty", "brewer", "discrete", "continuous", "default")
  ),
  textInput("ma.title", "Plot title", value = ""),
  numericInput("ma.plot.width", "Plot width for download, inch", value = "4"),
  numericInput("ma.plot.height", "Plot height for download, inch", value = "4")
)

panel.ma.pca <- tabPanel(
  "PCA   ",
  checkboxInput("ma.pca.ellipse", "Add ellipse ?", F),
  numericInput("ma.pca.level", "Ellipse level, default = 0.95", value = "0.95"),
  numericInput("ma.pca.size", "Point size, default = 3", value = "3")
)

panel.ma.pcoa <- tabPanel(
  "PCOA   ",
  selectInput("ma.pcoa.method", "Distance method ?",
    c("manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski", "jaccard", "gower", "altGower",  "horn", "mountford",  "binomial", "chao", "cao", "mahalanobis"),
    selected = "bray"
  ),
  checkboxInput("ma.pcoa.ellipse", "Add ellipse ?", F),
  numericInput("ma.pcoa.level", "Ellipse level, default = 0.95", value = "0.95"),
  numericInput("ma.pcoa.size", "Point size, default = 3", value = "3")
)

panel.ma.nmds <- tabPanel(
  "NMDS   ",
  checkboxInput("ma.nmds.ellipse", "Add ellipse ?", F),
  numericInput("ma.nmds.level", "Ellipse level, default = 0.95", value = "0.95"),
  numericInput("ma.nmds.size", "Point size, default = 3", value = "3")
)

panel.ma.cca <- tabPanel(
  "CCA/RDA   ",
  selectInput("ma.cca.method", "Standardization methods ?",
    c("total", "max", "hellinger", "normalize", "range", "standardize"),
    selected = "hellinger"
  ),
  numericInput("ma.cca.textsize", "Text size, default = 3", value = "3"),
  numericInput("ma.cca.size", "Point size, default = 3", value = "3"),
  helpText("First row as sample name, first col as env factor name, duplication is forbidden"),
  fileInput("ma.env", "Choose environment File",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  ),
  tags$hr(),
  checkboxInput("ma.cca.species", "Plot select species", F),
  helpText("Only first one col with a colname , each row represents a select species, duplication is forbidden"),
  fileInput("ma.cca.species.data", "Upload select species File",
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  ),
  tags$hr()
)



panel.ma <- tabPanel(
  "Metagenomic Analysis",
  sidebarPanel(
    tabsetPanel(
      panel.ma.data,
      panel.ma.general,
      panel.ma.pca,
      panel.ma.pcoa,
      panel.ma.nmds,
      panel.ma.cca
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "OTU data",
        tableOutput("ma.otu.contents")
      ),
      tabPanel(
        "PCA plot",
        plotOutput("ma.pcaplot", height = "600px", width = "600px"),
        downloadButton('download.ma.pcaplot','Download Plot')
      ),
      tabPanel(
        "PCOA plot",
        plotOutput("ma.pcoaplot", height = "600px", width = "600px"),
        downloadButton('download.ma.pcoaplot','Download Plot')
      ),
      tabPanel(
        "NMDS plot",
        plotOutput("ma.nmdsplot", height = "600px", width = "600px"),
        downloadButton('download.ma.nmdsaplot','Download Plot')
      ),
      tabPanel(
        "CCA/RDA plot",
        plotOutput("ma.ccaplot", height = "600px", width = "600px"),
        downloadButton('download.ma.ccaplot','Download Plot')
      )
    )
  )
)

#############################           ui        #################################

ui <- tagList(
  navbarPage(
    "iLluminating Joy Xanadu V1.0",
    panel.sa,
    panel.ma,
    mainPanel()
  )
)

#############################       server       #################################

server <- function(input, output) {
  output$sa.contents <- renderTable({
    inFile <- input$sa.file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.table(inFile$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote)
  })

  output$sa.boxplot <- renderPlot({
    inFile <- input$sa.file
    sa.data <- read.table(inFile$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote)
    for (i in 1:ncol(sa.data[, -input$sa.y])) {
      sa.data[, i] <- as.factor(as.character(sa.data[, i]))
    }
    sa.data[, input$sa.y] <- as.numeric(sa.data[, input$sa.y])

    rawplot <- ggplot(sa.data, aes(x = sa.data[, input$sa.x], y = sa.data[, input$sa.y]))
    sa.boxplot <- plot.raw(sa.data, input, rawplot)

    box.fill <- plot.parameter(sa.data, input$sa.box.fill)
    sa.boxplot <- sa.boxplot + geom_boxplot(aes(fill = box.fill), color = "black", outlier.shape = NA) + guides(fill = guide_legend(title = "Groups"))

    if (input$sa.box.jitter == T) {
      sa.boxplot <- sa.boxplot + geom_jitter(aes(fill = box.fill), color = "black", shape = 21, position = position_jitterdodge())
    }

    if (is.na(input$sa.stat.compar) != T) {
      compare.group <- strsplit(input$sa.stat.compar, split = ";")[[1]]
      my_comparisons <- strsplit(compare.group, split = ",")
    }

    if (input$sa.box.stat == T) {
      if (input$sa.stat.type == "Between X-axis group comparison") {
        sa.boxplot <- sa.boxplot + stat_compare_means(
          method = input$sa.stat.method, label = input$sa.stat.label, paired = input$sa.stat.paired,
          comparisons = my_comparisons
        )
      }
      if (input$sa.stat.type == "Reference comparison") {
        sa.boxplot <- sa.boxplot + stat_compare_means(
          method = input$sa.stat.method, label = input$sa.stat.label, paired = input$sa.stat.paired,
          ref.group = input$sa.stat.ref
        )
      }
      if (input$sa.stat.type == "Within X-axis group comparison") {
        stat.group <- plot.parameter(sa.data, input$sa.stat.group)
        sa.boxplot <- sa.boxplot + stat_compare_means(
          method = input$sa.stat.method, label = input$sa.stat.label, paired = input$sa.stat.paired,
          aes(group = stat.group)
        )
      }
    }

    print(sa.boxplot)
  })

  output$sa.violinplot <- renderPlot({
    inFile <- input$sa.file
    sa.data <- read.table(inFile$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote)
    for (i in 1:ncol(sa.data[, -input$sa.y])) {
      sa.data[, i] <- as.factor(as.character(sa.data[, i]))
    }
    sa.data[, input$sa.y] <- as.numeric(sa.data[, input$sa.y])
    rawplot <- ggplot(sa.data, aes(x = sa.data[, input$sa.x], y = sa.data[, input$sa.y]))
    sa.violinplot <- plot.raw(sa.data, input, rawplot)

    violin.fill <- plot.parameter(sa.data, input$sa.violin.fill)
    sa.violinplot <- sa.violinplot + geom_violin(aes(fill = violin.fill), color = "black") + guides(fill = guide_legend(title = "Groups"))

    if (input$sa.violin.jitter == T) {
      sa.violinplot <- sa.violinplot + geom_jitter(aes(fill = violin.fill), color = "black", shape = 21, position = position_jitterdodge())
    }

    if (is.na(input$sa.stat.compar) != T) {
      compare.group <- strsplit(input$sa.stat.compar, split = ";")[[1]]
      my_comparisons <- strsplit(compare.group, split = ",")
    }

    if (input$sa.violin.stat == T) {
      if (input$sa.stat.type == "Between X-axis group comparison") {
        sa.violinplot <- sa.violinplot + stat_compare_means(
          method = input$sa.stat.method, label = input$sa.stat.label, paired = input$sa.stat.paired,
          comparisons = my_comparisons
        )
      }
      if (input$sa.stat.type == "Reference comparison") {
        sa.violinplot <- sa.violinplot + stat_compare_means(
          method = input$sa.stat.method, label = input$sa.stat.label, paired = input$sa.stat.paired,
          ref.group = input$sa.stat.ref
        )
      }
      if (input$sa.stat.type == "Within X-axis group comparison") {
        stat.group <- plot.parameter(sa.data, input$sa.stat.group)
        sa.violinplot <- sa.violinplot + stat_compare_means(
          method = input$sa.stat.method, label = input$sa.stat.label, paired = input$sa.stat.paired,
          aes(group = stat.group)
        )
      }
    }
    if (input$sa.violin.box == T) {
      if (is.na(input$sa.violin.fill) != T) {
        sa.violinplot <- sa.violinplot + geom_boxplot(aes(color = violin.fill), fill = "white", width = input$sa.violin.boxwidth, position = position_dodge(input$sa.violin.boxdodge))
      } else {
        sa.violinplot <- sa.violinplot + geom_boxplot(fill = "white", color = "grey", width = input$sa.violin.boxwidth)
      }
    }



    print(sa.violinplot)
  })

  output$sa.lineplot <- renderPlot({
    inFile <- input$sa.file
    sa.data <- read.table(inFile$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote)
    for (i in 1:ncol(sa.data[, -input$sa.y])) {
      sa.data[, i] <- as.factor(as.character(sa.data[, i]))
    }
    sa.data[, input$sa.y] <- as.numeric(sa.data[, input$sa.y])

    if (is.null(input$sa.line.groups) != T) {
      mean.group <- as.numeric(strsplit(input$sa.line.groups, split = ",")[[1]])
      mean.group.name <- colnames(sa.data)[mean.group]
      sa.summary <- summarySE(data = sa.data, measurevar = colnames(sa.data)[input$sa.y], groupvars = mean.group.name)
      summary.x <- locate.new.col(sa.data, input$sa.x, sa.summary)
      summary.y <- locate.new.col(sa.data, input$sa.y, sa.summary)
      for (i in 1:(ncol(sa.summary) - 5)) {
        sa.summary[, i] <- as.factor(as.character(sa.summary[, i]))
      }
      summary.linecolor <- locate.new.col(sa.data, input$sa.line.linecolor, sa.summary)
      summary.linetype <- locate.new.col(sa.data, input$sa.line.linetype, sa.summary)
    }

    if (input$sa.line.mean == F) {
      rawplot <- ggplot(sa.data, aes(x = sa.data[, input$sa.x], y = sa.data[, input$sa.y]))
      sa.lineplot <- plot.raw(sa.data, input, rawplot)
      line.color <- plot.parameter(sa.data, input$sa.line.linecolor)
      line.linetype <- plot.parameter(sa.data, input$sa.line.linetype)
      sa.lineplot <- sa.lineplot +
        geom_line(aes(group = line.color, color = line.color, linetype = line.linetype), size = input$sa.line.linewidth, position = position_dodge(input$sa.line.dodge)) +
        geom_point(aes(fill = line.color), color = "black", shape = 21, size = input$sa.line.dotsize, position = position_dodge(input$sa.line.pointdodge)) +
        guides(fill = guide_legend(title = "Groups"))
    } else {
      rawplot <- ggplot(sa.summary, aes(x = sa.summary[, summary.x], y = sa.summary[, summary.y]))
      sa.lineplot <- plot.raw(sa.summary, input, rawplot)
      line.color <- plot.parameter(sa.summary, summary.linecolor)
      line.linetype <- plot.parameter(sa.summary, summary.linetype)
      sa.lineplot <- sa.lineplot +
        geom_line(aes(group = line.color, color = line.color, linetype = line.linetype), size = input$sa.line.linewidth, position = position_dodge(input$sa.line.dodge)) +
        geom_point(aes(fill = line.color), color = "black", shape = 21, size = input$sa.line.dotsize, position = position_dodge(input$sa.line.dodge)) +
        guides(fill = guide_legend(title = "Groups"))

      if (input$sa.line.errorbar == T) {
        if (input$sa.line.errorbartype == "se") {
          sa.lineplot <- sa.lineplot +
            geom_errorbar(aes(color = line.color, ymin = sa.summary[, summary.y] - se, ymax = sa.summary[, summary.y] + se), position = position_dodge(input$sa.line.dodge), width = 0.1 * input$sa.line.dotsize)
        } else {
          sa.lineplot <- sa.lineplot +
            geom_errorbar(aes(color = line.color, ymin = sa.summary[, summary.y] - sd, ymax = sa.summary[, summary.y] + sd), position = position_dodge(input$sa.line.dodge), width = 0.1 * input$sa.line.dotsize)
        }
      }
    }

    print(sa.lineplot)
  })

  output$sa.lmplot <- renderPlot({
    inFile <- input$sa.file
    sa.data <- read.table(inFile$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote)
    for (i in 1:ncol(sa.data[, -c(input$sa.y, input$sa.x)])) {
      sa.data[, i] <- as.factor(as.character(sa.data[, i]))
    }
    sa.data[, input$sa.y] <- as.numeric(as.character(sa.data[, input$sa.y]))
    sa.data[, input$sa.x] <- as.numeric(as.character(sa.data[, input$sa.x]))
    rawplot <- ggplot(sa.data, aes(x = sa.data[, input$sa.x], y = sa.data[, input$sa.y]))
    sa.lmplot <- plot.raw(sa.data, input, rawplot)

    lm.color <- plot.parameter(sa.data, input$sa.lm.lmcolor)
    lm.lmtype <- plot.parameter(sa.data, input$sa.lm.lmtype)
    sa.lmplot <- sa.lmplot +
      geom_smooth(aes(color = lm.color, linetype = lm.lmtype), size = input$sa.lm.lmwidth, method = input$sa.lm.method, se = TRUE) +
      geom_point(aes(fill = lm.color), color = "black", shape = 21, size = input$sa.lm.dotsize) +
      guides(fill = guide_legend(title = "Groups"))

    print(sa.lmplot)
  })

  output$sa.barplot <- renderPlot({
    inFile <- input$sa.file
    sa.data <- read.table(inFile$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote)
    for (i in 1:ncol(sa.data[, -input$sa.y])) {
      sa.data[, i] <- as.factor(as.character(sa.data[, i]))
    }
    sa.data[, input$sa.y] <- as.numeric(sa.data[, input$sa.y])

    if (is.null(input$sa.bar.groups) != T) {
      mean.group <- as.numeric(strsplit(input$sa.bar.groups, split = ",")[[1]])
      mean.group.name <- colnames(sa.data)[mean.group]
      sa.summary <- summarySE(data = sa.data, measurevar = colnames(sa.data)[input$sa.y], groupvars = mean.group.name)
      summary.x <- locate.new.col(sa.data, input$sa.x, sa.summary)
      summary.y <- locate.new.col(sa.data, input$sa.y, sa.summary)
      for (i in 1:(ncol(sa.summary) - 5)) {
        sa.summary[, i] <- as.factor(as.character(sa.summary[, i]))
      }
      summary.barfill <- locate.new.col(sa.data, input$sa.bar.barfill, sa.summary)
    }
    if (input$sa.bar.bartype == "stack") {
      bar.bartype <- "stack"
    } else {
      bar.bartype <- position_dodge(input$sa.bar.dodge)
    }

    if (input$sa.bar.mean == F) {
      rawplot <- ggplot(sa.data, aes(x = sa.data[, input$sa.x], y = sa.data[, input$sa.y]))
      sa.barplot <- plot.raw(sa.data, input, rawplot)
      bar.barfill <- plot.parameter(sa.data, input$sa.bar.barfill)
      sa.barplot <- sa.barplot +
        geom_bar(aes(fill = bar.barfill), color = "black", width = input$sa.bar.barwidth, stat = "identity", position = bar.bartype) +
        guides(fill = guide_legend(title = "Groups"))
    } else {
      rawplot <- ggplot(sa.summary, aes(x = sa.summary[, summary.x], y = sa.summary[, summary.y]))
      sa.barplot <- plot.raw(sa.summary, input, rawplot)
      bar.barfill <- plot.parameter(sa.summary, summary.barfill)
      sa.barplot <- sa.barplot +
        geom_bar(aes(fill = bar.barfill), color = "black", width = input$sa.bar.barwidth, stat = "identity", position = bar.bartype) +
        guides(fill = guide_legend(title = "Groups"))

      if (input$sa.bar.errorbar == T) {
        if (input$sa.bar.errorbartype == "se") {
          sa.barplot <- sa.barplot +
            geom_errorbar(aes(group = bar.barfill, ymin = sa.summary[, summary.y] - se, ymax = sa.summary[, summary.y] + se), width = 0.5 * input$sa.bar.barwidth, position = bar.bartype)
        } else {
          sa.barplot <- sa.barplot +
            geom_errorbar(aes(group = bar.barfill, ymin = sa.summary[, summary.y] - sd, ymax = sa.summary[, summary.y] + sd), width = 0.5 * input$sa.bar.barwidth, position = bar.bartype)
        }
      }
    }

    print(sa.barplot)
  })

  output$sa.densityplot <- renderPlot({
    inFile <- input$sa.file
    sa.data <- read.table(inFile$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote)
    for (i in 1:ncol(sa.data[, -input$sa.y])) {
      sa.data[, i] <- as.factor(as.character(sa.data[, i]))
    }
    sa.data[, input$sa.y] <- as.numeric(sa.data[, input$sa.y])

    if (input$sa.density.mean == T) {
      density.mean <- "mean"
    } else {
      density.mean <- "none"
    }

    if (input$sa.density.type == "Histogram") {
      rawplot <- gghistogram(sa.data,
        x = colnames(sa.data)[input$sa.y],
        add = density.mean, rug = input$sa.density.rug,
        color = colnames(sa.data)[input$sa.density.fill],
        fill = colnames(sa.data)[input$sa.density.fill],
        bins = input$sa.density.bin
      )
    } else {
      rawplot <- ggdensity(sa.data,
        x = colnames(sa.data)[input$sa.y],
        add = density.mean, rug = input$sa.density.rug,
        color = colnames(sa.data)[input$sa.density.fill],
        fill = colnames(sa.data)[input$sa.density.fill],
        bins = input$sa.density.bin
      )
    }

    sa.densityplot <- plot.raw(sa.data, input, rawplot)

    print(sa.densityplot)
  })
  
  output$ma.otu.contents <- renderTable({
    ma.File <- input$ma.otu
    if (is.null(ma.File)) {
      return(NULL)
    }
    read.table(ma.File$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote)
  })

  output$ma.pcaplot <- renderPlot({
    ma.File <- input$ma.otu
    ma.data <- read.table(ma.File$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote)
    
    if (is.na(input$ma.groupcol) != F) {
      ma.matrix = dataframe2matrix(ma.data)
      otu.data <- t(ma.matrix)
      otu.pca <- summary(prcomp(otu.data))
      otu.pca.data <- data.frame(
        sample = rownames(otu.pca$x),
        x = as.numeric(otu.pca$x[, 1]),
        y = as.numeric(otu.pca$x[, 2])
      )
      pca.plot <- ggplot(otu.pca.data, aes(x = x, y = y)) +
        geom_point(size = input$ma.pca.size)+
        labs(x=paste("PCA1(",as.numeric(sprintf("%.3f",otu.pca$importance[2,1]))*100,"%)",sep=""),
                                                 y=paste("PCA2(",as.numeric(sprintf("%.3f",otu.pca$importance[2,2]))*100,"%)",sep=""))
    }else{
      ma.group <- input$ma.group
      group.data <- read.table(ma.group$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote,na.strings = "NA",fill = T,stringsAsFactors = T)
      group = group.data[input$ma.groupcol,which(group.data[input$ma.groupcol,] != "")]
      group2 = t(group[,-1])
      group.factor = as.factor(group2[,1])
      ma.group.data = ma.data[,which(group.data[input$ma.groupcol,] != "")]
      ma.matrix = dataframe2matrix(ma.group.data)
      otu.data <- t(ma.matrix)
      otu.pca <- summary(prcomp(otu.data))
      otu.pca.data <- data.frame(
        sample = rownames(otu.pca$x),
        x = as.numeric(otu.pca$x[, 1]),
        y = as.numeric(otu.pca$x[, 2]),
        class = group.factor
      )
      otu.pca.data$class = as.factor(otu.pca.data$class)
      pca.plot <- ggplot(otu.pca.data, aes(x = x, y = y,fill = class,color = class)) +
        geom_point(size = input$ma.pca.size)+
        labs(x=paste("PCA1(",as.numeric(sprintf("%.3f",otu.pca$importance[2,1]))*100,"%)",sep=""),
             y=paste("PCA2(",as.numeric(sprintf("%.3f",otu.pca$importance[2,2]))*100,"%)",sep=""))
    }
    
    if ( input$ma.pca.ellipse == T) {
      pca.plot = pca.plot +
        stat_ellipse(level = input$ma.pca.level)
    } 

    pca.plot = plot.ma.raw(input, pca.plot)

    print(pca.plot)
    
  })

  output$ma.pcoaplot <- renderPlot({
    ma.File <- input$ma.otu
    ma.data <- read.table(ma.File$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote)
    
    if (is.na(input$ma.groupcol) != F) {
      ma.matrix = dataframe2matrix(ma.data)
      otu.data <- t(ma.matrix)
      distance.otu<-vegdist(otu.data,method = input$ma.pcoa.method)
      PCOA <- pcoa(distance.otu, correction="none", rn=NULL)
      PCOA.data = data.frame(
        x=PCOA$vectors[,1],y=PCOA$vectors[,2]
      )
      
      pcoa.plot =ggplot(data=PCOA.data,aes(x,y)) + 
        geom_point(size=input$ma.pcoa.size) +
        labs(x=paste("PCOA1(",as.numeric(sprintf("%.3f",PCOA$values[,"Relative_eig"][1]))*100,"%)",sep=""),
             y=paste("PCOA2(",as.numeric(sprintf("%.3f",PCOA$values[,"Relative_eig"][2]))*100,"%)",sep=""))
    }else{
      ma.group <- input$ma.group
      group.data <- read.table(ma.group$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote,na.strings = "NA",fill = T,stringsAsFactors = T)
      group = group.data[input$ma.groupcol,which(group.data[input$ma.groupcol,] != "")]
      group2 = t(group[,-1])
      group.factor = as.factor(group2[,1])
      ma.group.data = ma.data[,which(group.data[input$ma.groupcol,] != "")]
      ma.matrix = dataframe2matrix(ma.group.data)
      otu.data <- t(ma.matrix)
      distance.otu<-vegdist(otu.data,method = input$ma.pcoa.method)
      PCOA <- pcoa(distance.otu, correction="none", rn=NULL)
      PCOA.data = data.frame(
        x=PCOA$vectors[,1],
        y=PCOA$vectors[,2],
        class = group.factor
      )
      
      pcoa.plot =ggplot(data=PCOA.data,aes(x,y,color = class,fill = class)) + 
        geom_point(size=input$ma.pcoa.size) +
        labs(x=paste("PCOA1(",as.numeric(sprintf("%.3f",PCOA$values[,"Relative_eig"][1]))*100,"%)",sep=""),
             y=paste("PCOA2(",as.numeric(sprintf("%.3f",PCOA$values[,"Relative_eig"][2]))*100,"%)",sep=""))
    }
    
    if ( input$ma.pcoa.ellipse == T) {
      pcoa.plot = pcoa.plot +
        stat_ellipse(level = input$ma.pcoa.level)
    } 
    
    pcoa.plot = plot.ma.raw(input, pcoa.plot)
    
    print(pcoa.plot)
    
  })
  
  output$ma.nmdsplot <- renderPlot({
    ma.File <- input$ma.otu
    ma.data <- read.table(ma.File$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote)
    
    if (is.na(input$ma.groupcol) != F) {
      ma.matrix = dataframe2matrix(ma.data)
      otu.data <- t(ma.matrix)
      NMDS <- metaMDS(otu.data)
      NMDS.data = data.frame(x = NMDS$points[,1], y = NMDS$points[,2])
      
      nmds.plot =ggplot(data=NMDS.data,aes(x,y)) + 
        geom_point(size=input$ma.nmds.size) +
        labs(x="NMDS1",y="NMDS2")
    }else{
      ma.group <- input$ma.group
      group.data <- read.table(ma.group$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote,na.strings = "NA",fill = T,stringsAsFactors = T)
      group = group.data[input$ma.groupcol,which(group.data[input$ma.groupcol,] != "")]
      group2 = t(group[,-1])
      group.factor = as.factor(group2[,1])
      ma.group.data = ma.data[,which(group.data[input$ma.groupcol,] != "")]
      ma.matrix = dataframe2matrix(ma.group.data)
      otu.data <- t(ma.matrix)
      NMDS <- metaMDS(otu.data)
      NMDS.data = data.frame(x = NMDS$points[,1], y = NMDS$points[,2],class = group.factor)
      
      nmds.plot =ggplot(data=NMDS.data,aes(x,y,color = group.factor,fill = group.factor)) + 
        geom_point(size=input$ma.nmds.size) +
        labs(x="NMDS1",y="NMDS2")
    }
    
    if ( input$ma.nmds.ellipse == T) {
      nmds.plot = nmds.plot +
        stat_ellipse(level = input$ma.nmds.level)
    } 
    
    nmds.plot = plot.ma.raw(input, nmds.plot)
    
    print(nmds.plot)
    
  }) 
   
  output$ma.ccaplot <- renderPlot({
    ma.File <- input$ma.otu
    ma.data <- read.table(ma.File$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote)
    env.File <- input$ma.env
    env.input <- read.table(env.File$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote)
    
    if (is.na(input$ma.groupcol) != F) {
      ma.matrix = dataframe2matrix(ma.data)
      otu.data <- t(ma.matrix)
      otu.hell <- decostand(otu.data, method =input$ma.cca.method)
      env.matrix = dataframe2matrix(env.input)
      env.data <- t(env.matrix)
      env.hell <- decostand(env.data, method =input$ma.cca.method)      
      env.hell.data = as.data.frame(env.hell)
      dca.otu <- decorana(otu.hell)
      if(dca.otu[[1]][1] < 4){
        otu.rda <- rda(otu.hell ~ ., env.hell.data)
        type = "RDA"
      }else{
        otu.rda <- cca(otu.hell ~ ., env.hell.data)
        type = "CCA"
      }
      cca.data = summary(otu.rda)
      samples<-data.frame(sample=row.names(cca.data$sites),x=cca.data$sites[,1],y=cca.data$sites[,2])
      species<-data.frame(species=row.names(cca.data$species),x=cca.data$species[,1],y=cca.data$species[,2])
      envi<-data.frame(envi=row.names(cca.data$biplot),x=cca.data$biplot[,1],y=cca.data$biplot[,2])
      envi.data = data.frame(x=rep(envi$x,2),y=rep(envi$y,2),env=rep(envi[,1],2))
      envi.data$env = as.character(envi.data$env)
      for (i in 1:nrow(envi)){
        envi.data$x[2*i-1] = 0
        envi.data$y[2*i-1] = 0
        envi.data$x[2*i] = envi$x[i]
        envi.data$y[2*i] = envi$y[i]
        envi.data$env[2*i-1] = as.character(envi$envi[i])
        envi.data$env[2*i] = as.character(envi$envi[i])
      }
      envi.data$x = as.numeric(envi.data$x)
      envi.data$y = as.numeric(envi.data$y)
      envi.data$env = as.factor(envi.data$env)
      
      cca.plot = ggplot(data=samples,aes(x,y)) + 
        geom_point(size=input$ma.cca.size) +
        geom_text(data=envi,aes(label=envi),color="black",size = input$ma.cca.textsize) +
        geom_hline(yintercept=0) + geom_vline(xintercept=0)+
        geom_path(data=envi.data,aes(x=x,y=y,group=env),color="blue",
                  arrow = arrow(angle = 10, ends = "last",type = "closed"))+
        labs(x=paste(type,"1(",as.numeric(sprintf("%.3f",cca.data$cont$importance[2,1]))*100,"%)",sep=""),
             y=paste(type,"2(",as.numeric(sprintf("%.3f",cca.data$cont$importance[2,2]))*100,"%)",sep=""))
      
      

    }else{
      ma.group <- input$ma.group
      group.data <- read.table(ma.group$datapath, header = input$sa.header, check.names = input$sa.checkname, sep = input$sa.sep, quote = input$sa.quote,na.strings = "NA",fill = T,stringsAsFactors = T)
      group = group.data[input$ma.groupcol,which(group.data[input$ma.groupcol,] != "")]
      group2 = t(group[,-1])
      group.factor = as.factor(group2[,1])
      ma.group.data = ma.data[,which(group.data[input$ma.groupcol,] != "")]
      env.group.data = env.input[,which(group.data[input$ma.groupcol,] != "")]
      
      ma.matrix = dataframe2matrix(ma.group.data)
      otu.data <- t(ma.matrix)
      otu.hell <- decostand(otu.data, method =input$ma.cca.method)
      env.matrix = dataframe2matrix(env.group.data)
      env.data <- t(env.matrix)
      env.hell <- decostand(env.data, method =input$ma.cca.method)      
      env.hell.data = as.data.frame(env.hell)
      dca.otu <- decorana(otu.hell)
      if(dca.otu[[1]][1] < 4){
        otu.rda <- rda(otu.hell ~ ., env.hell.data)
        type = "RDA"
      }else{
        otu.rda <- cca(otu.hell ~ ., env.hell.data)
        type = "CCA"
      }
      cca.data = summary(otu.rda)
      samples<-data.frame(sample=row.names(cca.data$sites),x=cca.data$sites[,1],y=cca.data$sites[,2],class = group.factor)
      species<-data.frame(species=row.names(cca.data$species),x=cca.data$species[,1],y=cca.data$species[,2])
      envi<-data.frame(envi=row.names(cca.data$biplot),x=cca.data$biplot[,1],y=cca.data$biplot[,2])
      envi.data = data.frame(x=rep(envi$x,2),y=rep(envi$y,2),env=rep(envi[,1],2))
      envi.data$env = as.character(envi.data$env)
      for (i in 1:nrow(envi)){
        envi.data$x[2*i-1] = 0
        envi.data$y[2*i-1] = 0
        envi.data$x[2*i] = envi$x[i]
        envi.data$y[2*i] = envi$y[i]
        envi.data$env[2*i-1] = as.character(envi$envi[i])
        envi.data$env[2*i] = as.character(envi$envi[i])
      }
      envi.data$x = as.numeric(envi.data$x)
      envi.data$y = as.numeric(envi.data$y)
      envi.data$env = as.factor(envi.data$env)
      
      cca.plot = ggplot(data=samples,aes(x,y)) + 
        geom_point(size=input$ma.cca.size,aes(color = class,fill = class)) +
        geom_text(data=envi,aes(label=envi),color="black",size = input$ma.cca.textsize) +
        geom_hline(yintercept=0) + geom_vline(xintercept=0)+
        geom_path(data=envi.data,aes(x=x,y=y,group=env),color="blue",
                  arrow = arrow(angle = 15, length = unit(0.15, "inches"), ends = "last",type = "closed"))+
        labs(x=paste(type,"1(",as.numeric(sprintf("%.3f",cca.data$cont$importance[2,1]))*100,"%)",sep=""),
             y=paste(type,"2(",as.numeric(sprintf("%.3f",cca.data$cont$importance[2,2]))*100,"%)",sep=""))
      
    }
    
    if (input$ma.cca.species == T){
      species.File <- input$ma.cca.species.data
      species.input <- read.table(species.File$datapath, header = input$ma.header, check.names = input$ma.checkname, sep = input$ma.sep, quote = input$ma.quote)
      colnames(species.input)[1] = "species"
      species.data.select = merge(species,species.input,"species")
      species.data = data.frame(x=rep(species.data.select$x,2),y=rep(species.data.select$y,2),species=rep(species.data.select[,1],2))
      species.data$species = as.character(species.data$species)
      for (i in 1:nrow(species.data.select)){
        species.data$x[2*i-1] = 0
        species.data$y[2*i-1] = 0
        species.data$x[2*i] = species.data.select$x[i]
        species.data$y[2*i] = species.data.select$y[i]
        species.data$species[2*i-1] = as.character(species.data.select$species[i])
        species.data$species[2*i] = as.character(species.data.select$species[i])
      }
      species.data$x = as.numeric(species.data$x)
      species.data$y = as.numeric(species.data$y)
      
      cca.plot = cca.plot +  
        geom_path(data=species.data,aes(x=x,y=y,group=species),color="red",
                  arrow = arrow(angle = 15, length = unit(0.15, "inches"), ends = "last",type = "closed"))+
        geom_text(data=species.data[seq(2,nrow(species.data),by = 2),],aes(x=x,y=y,label=species),color="red",size = input$ma.cca.textsize)
     
    }
    
    
    
    
    
    cca.plot = plot.ma.raw(input, cca.plot)
    
    print(cca.plot)
    
  }) 
  
  output$download.ma.pcaplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$ma.plot.width,height = input$ma.plot.height,units = "in")
    }
  )
  output$download.ma.pcoaplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$ma.plot.width,height = input$ma.plot.height,units = "in")
    }
  )
  output$download.ma.nmdsplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$ma.plot.width,height = input$ma.plot.height,units = "in")
    }
  )
  output$download.ma.ccaplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$ma.plot.width,height = input$ma.plot.height,units = "in")
    }
  )
  output$download.sa.boxplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$sa.plot.width,height = input$sa.plot.height,units = "in")
    }
  )  
  output$download.sa.violinplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$sa.plot.width,height = input$sa.plot.height,units = "in")
    }
  )  
  output$download.sa.densityplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$sa.plot.width,height = input$sa.plot.height,units = "in")
    }
  )
  output$download.sa.barplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$sa.plot.width,height = input$sa.plot.height,units = "in")
    }
  )  
  output$download.sa.lineplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$sa.plot.width,height = input$sa.plot.height,units = "in")
    }
  )  
  output$download.sa.lmplot <- downloadHandler(
    filename = function(){paste("plot",'.pdf',sep='')},
    content = function(file){
      ggsave(file,plot=last_plot(),"pdf",dpi = 320,width = input$sa.plot.width,height = input$sa.plot.height,units = "in")
    }
  )  
  
  
}


############################     Run the application   ############################
shinyApp(ui = ui, server = server)




