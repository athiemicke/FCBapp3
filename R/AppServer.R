#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
#'@import ggplot2
#'@import data.table
#'@import plyr
#'@import ggridges
#'@import scales
#'@import ggpubr
#'@import shiny
#'@import BiocManager
#'@import flowViz
#'@import flowCore
#'@import flowStats


maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}





FFtoDF<-function(FF){

  if(class(FF) == "flowFrame"){
    return(as.data.frame(flowCore::exprs(FF)))
  }

  if(class(FF) == "list"){
    frameList<-list()
    length(frameList)<-length(FF)
    for(i in 1:length(FF)){
      if(class(FF[[i]]) == "flowFrame"){
        frameList[[i]]<-as.data.frame(flowCore::exprs(FF[[i]]))
        names(frameList)[[i]]<-names(FF)[[i]]
      }
      else{
        warning(paste("Object at index",i,"not of type flowFrame"))
      }
    }
    return(frameList)
  }
  else {
    stop("Object is not of type flowFrame")
  }
}




AppServer <-function(input, output, session) {



  shiny::observeEvent( input$save,{
    SD <- as.numeric(input$SD)
    bw <- as.numeric(input$bw)
    threshold <- as.numeric(input$threshold)
    PBdil  <- as.numeric(input$PBdil)
    POdil  <- as.numeric(input$POdil)


  })

  user_data <-  shiny::reactive({
    inFile <- input$flowfile

    data1 <-flowCore::read.FCS(inFile$datapath)
    return(data1)

  })

  user_key <-  shiny::reactive({
    inkey <- input$key

    key1 <-read.delim(inkey$datapath,header = F)
    return(key1)

  })


  #      mapurl <- session$registerDataObj(
  #
  #        name = 'arrests', # an arbitrary but unique name for the data object
  #        datas <- user_data(),
  #        datasdf <-  FFtoDF(datas),
  #        choices_names <- make.names(colnames(datasdf)),
  #        choices_names <- gsub('[.]', '_', choices_names),
  #        filter = function(choices_names, req) {
  #
  #          query <- parseQueryString(req$QUERY_STRING)
  #          #state <- subset(kbnew, Klasse==as.character(query) ) # state name
  #          state <- query
  #          klasse_name <- sort(unique(state$protein))
  #          # data is USArrests, the `data` argument of registerDataObj()
  #          #urban <- data[state, 'Wert'] # % urban population
  #
  #          # # save a map of the state and a pie of %UrbanPop to a PNG file
  #          # image <- tempfile()
  #          # tryCatch({
  #          # png(image, width = 400, height = 200, bg = 'transparent')
  #          # par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
  #          # map('county', regions = state, mar = c(0, 0, 0, 4))
  #          # pie(c(100 - urban, 100), col = rgb(1, c(1, 0), c(1, 0), .2), labels = NA)
  #          # }, finally = dev.off())
  #          #
  #          # # send the PNG image back in a response
  #          # shiny:::httpResponse(
  #          # 200, 'image/png', readBin(image, 'raw', file.info(image)[, 'size'])
  #          # )
  #
  #        }
  #      )
  #
  #      # update the render function for selectize
  #      updateSelectizeInput(
  #        session, 'state', server = TRUE,
  #        datas <- user_data(),
  #        datasdf <-  FFtoDF(datas),
  #        choices_names <- make.names(colnames(datasdf)),
  #        choices_names <- gsub('[.]', '_', choices_names),
  #
  #        # colnames(histdf) <- columnnames
  #        # channelnames <-  colnames(histdf)
  #
  #        choices = choices_names,
  #        options = list(render = I(sprintf(
  #          "{
  #       option: function(item, escape) {
  #       return '<div><img width=\"10\" height=\"5\" ' +
  #       'src=\"%s&state=' + escape(item.value) + '\" />' +
  #       escape(item.value) + '</div>';
  #       }
  # }",
  #          mapurl
  #        )))
  #      )



  output$parcoord1 <- shiny::renderPlot({
    #par(mar = c(2, 4, 2, .1))

    SD_in <- as.numeric(input$SD)
    #state <- input$state
    gateddata <- NULL
    #   user_data <- input$user_data
    #   user_data_name <- user_data$name
    # user_data_name <- as.character(input$flowfile)
    #data <- read.flowSet(user_data)

    #load data individually
    #  data1 <- read.FCS(user_data_name)
    #  data1 <- read.FCS('combaba.fcs')
    datas <- user_data()
    #  datas <- c(data1)


    for (i in seq_along(datas) ) {
      #select gate
      morphGate <- flowCore::norm2Filter(filterId = "MorphologyGate", "FSC-A", "SSC-A", scale = SD_in)
      gateddata <- cbind(gateddata, Subset(datas, morphGate))
    }

    print(FSplot <- flowViz::xyplot(`SSC-A` ~ `FSC-A` | name, data=datas, filter=morphGate, stat=T, pos = 0.5, abs = TRUE, smooth =F))





  }
  )


  output$parcoord <- shiny::renderPlot({


    SD_in <- as.numeric(input$SD)

    gateddata <- NULL

    datas <- user_data()

    #  datas <- c(data1)
    for (i in seq_along(datas) ) {
      #select gate
      morphGate <- flowCore::norm2Filter(filterId = "MorphologyGate", "FSC-A", "SSC-A", scale = SD_in)
      gateddata <- cbind(gateddata, Subset(datas, morphGate))
    }

    # total_channels <- length(exprs(gateddata[1]))
    total_channels <- ncol(flowCore::exprs(gateddata[[1]]))

    tf1 <- flowCore::transformList(from = colnames(gateddata[[1]])[1:total_channels], tfun = asinh)
    # tf2 <- transformList(from = colnames(gateddata[[2]])[1:25], tfun = asinh)
    # tf3 <- transformList(from = colnames(gateddata[[3]])[1:25], tfun = asinh)
    apolog1 <- tf1 %on% gateddata[[1]]
    # apolog2 <- tf2 %on% gateddata[[2]]
    # apolog3 <- tf3 %on% gateddata[[3]]
    #add all apolog data in one variable
    #  apologs <- c(apolog1  ) #,apolog2,apolog3)


    #par(mar = c(2, 4, 2, .1))
    bw_in <- as.numeric(input$bw)
    #state <- input$state

    bargate <- flowStats::curv2Filter('Pacific Orange-A','Pacific Blue-A',bwFac = bw_in)
    print(flowViz::xyplot( `Pacific Orange-A` ~`Pacific Blue-A` , data=apolog1, smooth=F,filter=bargate, abs = TRUE))
  })

  output$parcoord2 <- shiny::renderPlot({

    SD_in <- as.numeric(input$SD)
    #state <- input$state
    gateddata <- NULL
    datas <- user_data()
    for (i in seq_along(datas) ) {
      #select gate
      morphGate <- flowCore::norm2Filter(filterId = "MorphologyGate", "FSC-A", "SSC-A", scale = SD_in)
      gateddata <- cbind(gateddata, Subset(datas, morphGate))
    }

    # total_channels <- length(exprs(gateddata[1]))
    total_channels <- ncol(flowCore::exprs(gateddata[[1]]))

    tf1 <- flowCore::transformList(from = colnames(gateddata[[1]])[1:total_channels], tfun = asinh)
    # tf2 <- transformList(from = colnames(gateddata[[2]])[1:25], tfun = asinh)
    # tf3 <- transformList(from = colnames(gateddata[[3]])[1:25], tfun = asinh)
    apolog1 <- tf1 %on% gateddata[[1]]
    # apolog2 <- tf2 %on% gateddata[[2]]
    # apolog3 <- tf3 %on% gateddata[[3]]
    #add all apolog data in one variable
    # apologs <- c(apolog1,apolog2,apolog3)


    #par(mar = c(2, 4, 2, .1))
    bw_in <- as.numeric(input$bw)
    PBdil_in  <- as.numeric(input$PBdil)
    POdil_in <- as.numeric(input$POdil)
    #state <- input$state
    areas <- PBdil_in*POdil_in
    #  times=c( 0,2,5,10,15,20,30,60,120,180,240,300)
    times1 <-  user_key()
    names(times1) <- 'condition'
    instances <- nrow(times1$condition)
    times <- times1$condition
    bargate <- flowStats::curv2Filter('Pacific Orange-A','Pacific Blue-A',bwFac = bw_in)

    # AR <- NULL
    # for (i in 1:areas) {
    #
    #   pop_area <- paste('area',i,sep = ' ')
    #   AR[i] <-  split(apolog1, bargate, population='area 1')
    #
    #   #AR1count<- nrow(exprs(AR1[[1]]))
    # }
    AR1 <-  flowCore::split(apolog1, bargate, population='area 1')
    AR1count<- nrow(flowCore::exprs(AR1[[1]]))
    # if (AR1count>1000) {
    #  print(xyplot( `Pacific Orange-A` ~`Pacific Blue-A` ,  AR1[[1]], smooth=F, abs = TRUE))
    AR2 <-  flowCore::split(apolog1, bargate, population='area 2')
    AR2count<- nrow(flowCore::exprs(AR2[[1]]))
    # if (AR2count>1000) {
    AR3 <-  flowCore::split(apolog1, bargate, population='area 3')
    AR3count<- nrow(flowCore::exprs(AR3[[1]]))
    # if (AR3count>1000) {
    AR4 <-  flowCore::split(apolog1, bargate, population='area 4')
    AR4count<- nrow(flowCore::exprs(AR4[[1]]))
    # if (AR4count>1000) {

    AR5 <-  flowCore::split(apolog1, bargate, population='area 5')
    AR5count<- nrow(flowCore::exprs(AR5[[1]]))
    # if (AR5count>1000) {
    AR6 <-  flowCore::split(apolog1, bargate, population='area 6')
    AR6count<- nrow(flowCore::exprs(AR6[[1]]))
    # if (AR6count>1000) {
    AR7 <-  flowCore::split(apolog1, bargate, population='area 7')
    AR7count<- nrow(flowCore::exprs(AR7[[1]]))
    # if (AR7count>1000) {
    AR8 <-  flowCore::split(apolog1, bargate, population='area 8')
    AR8count<- nrow(flowCore::exprs(AR8[[1]]))
    # if (AR8count>1000) {

    AR9 <-  flowCore::split(apolog1, bargate, population='area 9')
    AR9count<- nrow(flowCore::exprs(AR9[[1]]))
    # if (AR9count>1000) {
    # AR10 <-  split(apolog1, bargate, population='area 10')
    # AR10count<- nrow(exprs(AR10[[1]]))
    # # if (AR10count>1000) {
    # AR11 <-  split(apolog1, bargate, population='area 11')
    # AR11count<- nrow(exprs(AR11[[1]]))
    # # if (AR11count>1000) {
    # AR12 <-  split(apolog1, bargate, population='area 12')
    # AR12count<- nrow(exprs(AR12[[1]]))
    # # if (AR12count>1000) {

    ##revert data for histogram
    ntfAR1 <- flowCore::transformList(from = colnames(AR1[[1]])[1:total_channels], tfun = sinh)
    ntfAR2 <- flowCore::transformList(from = colnames(AR2[[1]])[1:total_channels], tfun = sinh)
    ntfAR3 <- flowCore::transformList(from = colnames(AR3[[1]])[1:total_channels], tfun = sinh)
    ntfAR4 <- flowCore::transformList(from = colnames(AR4[[1]])[1:total_channels], tfun = sinh)
    ntfAR5 <- flowCore::transformList(from = colnames(AR5[[1]])[1:total_channels], tfun = sinh)
    ntfAR6 <- flowCore::transformList(from = colnames(AR6[[1]])[1:total_channels], tfun = sinh)
    ntfAR7 <- flowCore::transformList(from = colnames(AR7[[1]])[1:total_channels], tfun = sinh)
    ntfAR8 <- flowCore::transformList(from = colnames(AR8[[1]])[1:total_channels], tfun = sinh)
    ntfAR9 <- flowCore::transformList(from = colnames(AR9[[1]])[1:total_channels], tfun = sinh)
    # ntfAR10 <- transformList(from = colnames(AR10[[1]])[1:total_channels], tfun = sinh)
    # ntfAR11 <- transformList(from = colnames(AR11[[1]])[1:total_channels], tfun = sinh)
    # ntfAR12 <- transformList(from = colnames(AR12[[1]])[1:total_channels], tfun = sinh)


    nsAR1 <- ntfAR1 %on% AR1[[1]]
    nsAR2 <- ntfAR2 %on% AR2[[1]]
    nsAR3 <- ntfAR3 %on% AR3[[1]]
    nsAR4 <- ntfAR4 %on% AR4[[1]]
    nsAR5 <- ntfAR5 %on% AR5[[1]]
    nsAR6 <- ntfAR6 %on% AR6[[1]]
    nsAR7 <- ntfAR7 %on% AR7[[1]]
    nsAR8 <- ntfAR8 %on% AR8[[1]]
    nsAR9 <- ntfAR9 %on% AR9[[1]]
    # nsAR10 <- ntfAR10 %on% AR10[[1]]
    # nsAR11 <- ntfAR11 %on% AR11[[1]]
    # nsAR12 <- ntfAR12 %on% AR12[[1]]



    nsA1df <-  FFtoDF(nsAR1)
    nsA1df$PBmean <- mean(nsA1df$`Pacific Blue-A`)
    nsA1df$POmean <- mean(nsA1df$`Pacific Orange-A`)
    #nsA1df$timepoint <- times[1]
    nsA2df <-  FFtoDF(nsAR2)
    nsA2df$PBmean <- mean(nsA2df$`Pacific Blue-A`)
    nsA2df$POmean <- mean(nsA2df$`Pacific Orange-A`)
    #nsA2df$timepoint <- times[2]
    nsA3df <-  FFtoDF(nsAR3)
    nsA3df$PBmean <- mean(nsA3df$`Pacific Blue-A`)
    nsA3df$POmean <- mean(nsA3df$`Pacific Orange-A`)
    #nsA3df$timepoint <- times[3]
    nsA4df <-  FFtoDF(nsAR4)
    nsA4df$PBmean <- mean(nsA4df$`Pacific Blue-A`)
    nsA4df$POmean <- mean(nsA4df$`Pacific Orange-A`)
    #nsA4df$timepoint <- times[4]
    nsB1df <-  FFtoDF(nsAR5)
    nsB1df$PBmean <- mean(nsB1df$`Pacific Blue-A`)
    nsB1df$POmean <- mean(nsB1df$`Pacific Orange-A`)
    #nsB1df$timepoint <- times[5]
    nsB2df <-  FFtoDF(nsAR6)
    nsB2df$PBmean <- mean(nsB2df$`Pacific Blue-A`)
    nsB2df$POmean <- mean(nsB2df$`Pacific Orange-A`)
    #nsB2df$timepoint <- times[6]
    nsB3df <-  FFtoDF(nsAR7)
    nsB3df$PBmean <- mean(nsB3df$`Pacific Blue-A`)
    nsB3df$POmean <- mean(nsB3df$`Pacific Orange-A`)
    #nsB3df$timepoint <- times[7]
    nsB4df <-  FFtoDF(nsAR8)
    nsB4df$PBmean <- mean(nsB4df$`Pacific Blue-A`)
    nsB4df$POmean <- mean(nsB4df$`Pacific Orange-A`)
    #nsB4df$timepoint <- times[8]
    nsC1df <-  FFtoDF(nsAR9)
    nsC1df$PBmean <- mean(nsC1df$`Pacific Blue-A`)
    nsC1df$POmean <- mean(nsC1df$`Pacific Orange-A`)
    # # nsC1df$timepoint <- times[9]
    # nsC2df <-  FFtoDF(nsAR10)
    # nsC2df$PBmean <- mean(nsC2df$`Pacific Blue-A`)
    # nsC2df$POmean <- mean(nsC2df$`Pacific Orange-A`)
    # #nsC2df$timepoint <- times[9]
    # nsC3df <-  FFtoDF(nsAR11)
    # nsC3df$PBmean <- mean(nsC3df$`Pacific Blue-A`)
    # nsC3df$POmean <- mean(nsC3df$`Pacific Orange-A`)
    # #nsC3df$timepoint <- times[10]
    # nsC4df <-  FFtoDF(nsAR12)
    # nsC4df$PBmean <- mean(nsC4df$`Pacific Blue-A`)
    # nsC4df$POmean <- mean(nsC4df$`Pacific Orange-A`)
    #nsC4df$timepoint <- times[11]



    nsdfs <- rbind.fill(nsA1df,nsA2df,nsA3df,nsA4df,nsB1df,nsB2df,nsB3df,nsB4df,
                        nsC1df
                        #,nsC2df,nsC3df,nsC4df
    )

    ARDF <- data.frame('Areas'= c( 'area1','area2','area3',
                                   'area4','area5','area6',
                                   'area7','area8','area9'
                                   # ,'area10','area11','area12'
    ),
    'PB'= c(mean(nsA1df$`Pacific Blue-A`),mean(nsA2df$`Pacific Blue-A`),mean(nsA3df$`Pacific Blue-A`),mean(nsA4df$`Pacific Blue-A`),
            mean(nsB1df$`Pacific Blue-A`),mean(nsB2df$`Pacific Blue-A`),mean(nsB3df$`Pacific Blue-A`),mean(nsB4df$`Pacific Blue-A`),
            mean(nsC1df$`Pacific Blue-A`)
            # ,mean(nsC2df$`Pacific Blue-A`),mean(nsC3df$`Pacific Blue-A`),mean(nsC4df$`Pacific Blue-A`)

    ),
    'PO'= c(mean(nsA1df$`Pacific Orange-A`),mean(nsA2df$`Pacific Orange-A`),mean(nsA3df$`Pacific Orange-A`),mean(nsA4df$`Pacific Orange-A`),
            mean(nsB1df$`Pacific Orange-A`),mean(nsB2df$`Pacific Orange-A`),mean(nsB3df$`Pacific Orange-A`),mean(nsB4df$`Pacific Orange-A`),
            mean(nsC1df$`Pacific Orange-A`)
            # ,mean(nsC2df$`Pacific Orange-A`),mean(nsC3df$`Pacific Orange-A`),mean(nsC4df$`Pacific Orange-A`)

    ),
    'index'= c('nsA1df','nsA2df','nsA3df','nsA4df',
               'nsB1df','nsB2df','nsB3df','nsB4df',
               'nsC1df'
               #,'nsC2df','nsC3df','nsC4df'
    )
    )



    nsPB1df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 1:3))
    nsA1df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 1))
    nsA1df$timepoint <- times[1]
    nsA2df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 2))
    nsA2df$timepoint <- times[2]
    nsA3df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 3))
    nsA3df$timepoint <- times[3]
    # nsA4df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 4))
    # nsA4df$timepoint <- times[4]

    nsPB2df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 4:6))
    nsB1df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 1))
    nsB1df$timepoint <- times[4]
    nsB2df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 2))
    nsB2df$timepoint <- times[5]
    nsB3df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 3))
    nsB3df$timepoint <- times[6]
    # nsB4df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 4))
    # nsB4df$timepoint <- times[8]

    nsPB3df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 7:9))
    nsC1df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 1))
    nsC1df$timepoint <- times[7]
    nsC2df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 2))
    nsC2df$timepoint <- times[8]
    nsC3df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 3))
    nsC3df$timepoint <- times[9]
    # nsC4df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 4))
    # nsC4df$timepoint <- times[12]

    # histdb <- c(nsA1,nsA2,nsA3 ,nsA4,nsB1,nsB2 ,nsB3,nsB4,nsC1,nsC2,nsC3,nsC4)  #,nsC1,nsD1
    histdf <- plyr::rbind.fill(nsA1df,nsA2df,nsA3df
                               #,nsA4df
                               ,nsB1df,nsB2df ,nsB3df
                               # ,nsB4df
                               ,nsC1df,nsC2df,nsC3df
                               # ,nsC4df
    )
    columnnames <- make.names(colnames(histdf))
    columnnames <- gsub('[.]', '_', columnnames)

    colnames(histdf) <- columnnames
    channelnames <-  colnames(histdf)
    #  channelnames <- channelnames[c(8,11,12,13,14,22)]


    histdf$timepoint1 <- as.character(histdf$timepoint)
    #histdf$timepoint1 <- factor(histdf$timepoint1, levels = sort(times), ordered=TRUE)

    histdf$timepoint1 <-factor( histdf$timepoint1  ,times1$condition)


    channelname <-channelnames[9] #channelnames1[k]
    histdf$channel <- channelnames[9] #channelnames1[k]
    histdf$protein <- 'protein_a' #add input for marker name filename[k]
    # histdf$date <- date1
    # histdf$method <- ANmet
    #  histdf$FSCSSC_gateSD <- STD
    # histdf$costain <- 1
    # histdf$sample <- t[k]
    #histdf$input <- 'step'


    col1 <- 'Pacific_Blue_A' #unique(Casp3$channel)
    col2 <- 'Pacific_Orange_A' #unique(Casp8$channel)
    # prot2d_p <- subset(prot2d, )
    myvars <- c(col1,col2, 'timepoint1')
    #  date1 <- as.data.frame(date1)
    prot2d_p <- histdf[myvars]
    names(prot2d_p) <- c('A','B', 'timepoint')

    FS=12
    debarcscatter <- ggplot2::ggplot( histdf , aes(x = Pacific_Blue_A ,y= Pacific_Orange_A
                                                   # ,alpha = 0.5
                                                   # ,color=cluster
    )) + #, group=timepoint
      #   coord_trans(y="log", x="log") +
      # #  coord_trans(y="sqrt", x="sqrt") +
      ggplot2::scale_y_continuous(
        trans = scales::log10_trans(),
        breaks = scales::trans_breaks("log10", function(x) 10^x)
        ,labels = scales::trans_format("log10", scales::math_format(10^.x))
        #     ,limits = c(exp(1)^powerofx[k],exp(1)^powerof[k])
        ,limits = c(10^1,10^5)
      )+
      ggplot2::scale_x_continuous(trans = scales::log10_trans(),
                                  breaks = scales::trans_breaks("log10", function(x) 10^x)
                                  ,labels = scales::trans_format("log10", scales::math_format(10^.x))
                                  #                    limits = c(exp(1)^powerofx[k],exp(1)^powerof[k]))+
                                  ,limits = c(10^1,10^5))+
      ggplot2::geom_point(size=0.1, aes(color=timepoint1))+ #, alpha=0.6
      # ggtitle(filename[k])+
      # theme(legend.position="none")+
      # theme(
      #   #legend.position=c(0.8,1),
      #   legend.justification=c(0,1), legend.text = element_text(size = 8),legend.title = element_text(size=9),
      #   legend.key.size = unit(8,"point"),
      #   #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      #   panel.background = element_blank(),
      #   axis.ticks.y = element_line(colour = 'black', size = 3, linetype = 'solid'),
      #   axis.ticks.x = element_line(colour = 'black', size = 3, linetype = 'solid'),
      #   axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

    ggplot2::theme(
      #  panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.background = element_blank(),
      #legend.position="none",
      legend.justification=c(0,1), legend.text = element_text(size = 8),legend.title = element_text(size=9),
      legend.key.size = unit(8,"point"),
      plot.title = element_text(size = FS),
      axis.text.y= element_text(size = FS),
      axis.title.x = element_text(size = FS,hjust = 0.5,margin = margin(t = 1, r = 1, b = 1, l = 1)),
      axis.title.y = element_text(size = FS,hjust = 0.5,margin = margin(t = 1, r = 1, b = 1, l = 1)),
      axis.text.x= element_text(size = FS),
      axis.ticks.y = element_line(colour = 'black', size = 1*0.352778, linetype = 'solid'),
      axis.ticks.x = element_line(colour = 'black', size = 1*0.352778, linetype = 'solid'),
      axis.ticks.length = unit(0.1, "cm"),
      axis.line = element_line(colour = "black", size = 1*0.352778, linetype = "solid"),
      plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))+
      guides(color = guide_legend(override.aes = list(size=9)))
    print(debarcscatter)

  })

  output$parcoord3 <- shiny::renderPlot({


    SD_in <- as.numeric(input$SD)
    #state <- input$state
    gateddata <- NULL
    datas <- user_data()
    for (i in seq_along(datas) ) {
      #select gate
      morphGate <- flowCore::norm2Filter(filterId = "MorphologyGate", "FSC-A", "SSC-A", scale = SD_in)
      gateddata <- cbind(gateddata, Subset(datas, morphGate))
    }


    #total_channels <- length(exprs(gateddata[1]))
    total_channels <- ncol(flowCore::exprs(gateddata[[1]]))

    tf1 <- flowCore::transformList(from = colnames(gateddata[[1]])[1:total_channels], tfun = asinh)
    # tf2 <- transformList(from = colnames(gateddata[[2]])[1:25], tfun = asinh)
    # tf3 <- transformList(from = colnames(gateddata[[3]])[1:25], tfun = asinh)
    apolog1 <- tf1 %on% gateddata[[1]]
    # apolog2 <- tf2 %on% gateddata[[2]]
    # apolog3 <- tf3 %on% gateddata[[3]]
    #add all apolog data in one variable
    # apologs <- c(apolog1,apolog2,apolog3)


    #par(mar = c(2, 4, 2, .1))
    bw_in <- as.numeric(input$bw)
    #state <- input$state

    bargate <- flowStats::curv2Filter('Pacific Orange-A','Pacific Blue-A',bwFac = bw_in)
    #  times=c( 0,2,5,10,15,20,30,60,120,180,240,300)
    times1 <-  user_key()
    names(times1) <- 'condition'
    instances <- nrow(times1$condition)
    times <- times1$condition
    AR1 <-  flowCore::split(apolog1, bargate, population='area 1')
    AR1count<- nrow(flowCore::exprs(AR1[[1]]))
    # if (AR1count>1000) {
    #  print(xyplot( `Pacific Orange-A` ~`Pacific Blue-A` ,  AR1[[1]], smooth=F, abs = TRUE))
    AR2 <-  flowCore::split(apolog1, bargate, population='area 2')
    AR2count<- nrow(flowCore::exprs(AR2[[1]]))
    # if (AR2count>1000) {
    AR3 <-  flowCore::split(apolog1, bargate, population='area 3')
    AR3count<- nrow(flowCore::exprs(AR3[[1]]))
    # if (AR3count>1000) {
    AR4 <-  flowCore::split(apolog1, bargate, population='area 4')
    AR4count<- nrow(flowCore::exprs(AR4[[1]]))
    # if (AR4count>1000) {

    AR5 <-  flowCore::split(apolog1, bargate, population='area 5')
    AR5count<- nrow(flowCore::exprs(AR5[[1]]))
    # if (AR5count>1000) {
    AR6 <-  flowCore::split(apolog1, bargate, population='area 6')
    AR6count<- nrow(flowCore::exprs(AR6[[1]]))
    # if (AR6count>1000) {
    AR7 <-  flowCore::split(apolog1, bargate, population='area 7')
    AR7count<- nrow(flowCore::exprs(AR7[[1]]))
    # if (AR7count>1000) {
    AR8 <-  flowCore::split(apolog1, bargate, population='area 8')
    AR8count<- nrow(flowCore::exprs(AR8[[1]]))
    # if (AR8count>1000) {

    AR9 <-  flowCore::split(apolog1, bargate, population='area 9')
    AR9count<- nrow(flowCore::exprs(AR9[[1]]))
    # if (AR9count>1000) {
    # AR10 <-  split(apolog1, bargate, population='area 10')
    # AR10count<- nrow(exprs(AR10[[1]]))
    # # if (AR10count>1000) {
    # AR11 <-  split(apolog1, bargate, population='area 11')
    # AR11count<- nrow(exprs(AR11[[1]]))
    # # if (AR11count>1000) {
    # AR12 <-  split(apolog1, bargate, population='area 12')
    # AR12count<- nrow(exprs(AR12[[1]]))
    # # if (AR12count>1000) {

    ##revert data for histogram
    ntfAR1 <- flowCore::transformList(from = colnames(AR1[[1]])[1:total_channels], tfun = sinh)
    ntfAR2 <- flowCore::transformList(from = colnames(AR2[[1]])[1:total_channels], tfun = sinh)
    ntfAR3 <- flowCore::transformList(from = colnames(AR3[[1]])[1:total_channels], tfun = sinh)
    ntfAR4 <- flowCore::transformList(from = colnames(AR4[[1]])[1:total_channels], tfun = sinh)
    ntfAR5 <- flowCore::transformList(from = colnames(AR5[[1]])[1:total_channels], tfun = sinh)
    ntfAR6 <- flowCore::transformList(from = colnames(AR6[[1]])[1:total_channels], tfun = sinh)
    ntfAR7 <- flowCore::transformList(from = colnames(AR7[[1]])[1:total_channels], tfun = sinh)
    ntfAR8 <- flowCore::transformList(from = colnames(AR8[[1]])[1:total_channels], tfun = sinh)
    ntfAR9 <- flowCore::transformList(from = colnames(AR9[[1]])[1:total_channels], tfun = sinh)
    # ntfAR10 <- transformList(from = colnames(AR10[[1]])[1:total_channels], tfun = sinh)
    # ntfAR11 <- transformList(from = colnames(AR11[[1]])[1:total_channels], tfun = sinh)
    # ntfAR12 <- transformList(from = colnames(AR12[[1]])[1:total_channels], tfun = sinh)


    nsAR1 <- ntfAR1 %on% AR1[[1]]
    nsAR2 <- ntfAR2 %on% AR2[[1]]
    nsAR3 <- ntfAR3 %on% AR3[[1]]
    nsAR4 <- ntfAR4 %on% AR4[[1]]
    nsAR5 <- ntfAR5 %on% AR5[[1]]
    nsAR6 <- ntfAR6 %on% AR6[[1]]
    nsAR7 <- ntfAR7 %on% AR7[[1]]
    nsAR8 <- ntfAR8 %on% AR8[[1]]
    nsAR9 <- ntfAR9 %on% AR9[[1]]
    # nsAR10 <- ntfAR10 %on% AR10[[1]]
    # nsAR11 <- ntfAR11 %on% AR11[[1]]
    # nsAR12 <- ntfAR12 %on% AR12[[1]]



    nsA1df <-  FFtoDF(nsAR1)
    nsA1df$PBmean <- mean(nsA1df$`Pacific Blue-A`)
    nsA1df$POmean <- mean(nsA1df$`Pacific Orange-A`)
    #nsA1df$timepoint <- times[1]
    nsA2df <-  FFtoDF(nsAR2)
    nsA2df$PBmean <- mean(nsA2df$`Pacific Blue-A`)
    nsA2df$POmean <- mean(nsA2df$`Pacific Orange-A`)
    #nsA2df$timepoint <- times[2]
    nsA3df <-  FFtoDF(nsAR3)
    nsA3df$PBmean <- mean(nsA3df$`Pacific Blue-A`)
    nsA3df$POmean <- mean(nsA3df$`Pacific Orange-A`)
    #nsA3df$timepoint <- times[3]
    nsA4df <-  FFtoDF(nsAR4)
    nsA4df$PBmean <- mean(nsA4df$`Pacific Blue-A`)
    nsA4df$POmean <- mean(nsA4df$`Pacific Orange-A`)
    #nsA4df$timepoint <- times[4]
    nsB1df <-  FFtoDF(nsAR5)
    nsB1df$PBmean <- mean(nsB1df$`Pacific Blue-A`)
    nsB1df$POmean <- mean(nsB1df$`Pacific Orange-A`)
    #nsB1df$timepoint <- times[5]
    nsB2df <-  FFtoDF(nsAR6)
    nsB2df$PBmean <- mean(nsB2df$`Pacific Blue-A`)
    nsB2df$POmean <- mean(nsB2df$`Pacific Orange-A`)
    #nsB2df$timepoint <- times[6]
    nsB3df <-  FFtoDF(nsAR7)
    nsB3df$PBmean <- mean(nsB3df$`Pacific Blue-A`)
    nsB3df$POmean <- mean(nsB3df$`Pacific Orange-A`)
    #nsB3df$timepoint <- times[7]
    nsB4df <-  FFtoDF(nsAR8)
    nsB4df$PBmean <- mean(nsB4df$`Pacific Blue-A`)
    nsB4df$POmean <- mean(nsB4df$`Pacific Orange-A`)
    #nsB4df$timepoint <- times[8]
    nsC1df <-  FFtoDF(nsAR9)
    nsC1df$PBmean <- mean(nsC1df$`Pacific Blue-A`)
    nsC1df$POmean <- mean(nsC1df$`Pacific Orange-A`)
    # # nsC1df$timepoint <- times[9]
    # nsC2df <-  FFtoDF(nsAR10)
    # nsC2df$PBmean <- mean(nsC2df$`Pacific Blue-A`)
    # nsC2df$POmean <- mean(nsC2df$`Pacific Orange-A`)
    # #nsC2df$timepoint <- times[9]
    # nsC3df <-  FFtoDF(nsAR11)
    # nsC3df$PBmean <- mean(nsC3df$`Pacific Blue-A`)
    # nsC3df$POmean <- mean(nsC3df$`Pacific Orange-A`)
    # #nsC3df$timepoint <- times[10]
    # nsC4df <-  FFtoDF(nsAR12)
    # nsC4df$PBmean <- mean(nsC4df$`Pacific Blue-A`)
    # nsC4df$POmean <- mean(nsC4df$`Pacific Orange-A`)
    #nsC4df$timepoint <- times[11]



    nsdfs <- plyr::rbind.fill(nsA1df,nsA2df,nsA3df,nsA4df,nsB1df,nsB2df,nsB3df,nsB4df,
                              nsC1df
                              #,nsC2df,nsC3df,nsC4df
    )

    ARDF <- data.frame('Areas'= c( 'area1','area2','area3',
                                   'area4','area5','area6',
                                   'area7','area8','area9'
                                   # ,'area10','area11','area12'
    ),
    'PB'= c(mean(nsA1df$`Pacific Blue-A`),mean(nsA2df$`Pacific Blue-A`),mean(nsA3df$`Pacific Blue-A`),mean(nsA4df$`Pacific Blue-A`),
            mean(nsB1df$`Pacific Blue-A`),mean(nsB2df$`Pacific Blue-A`),mean(nsB3df$`Pacific Blue-A`),mean(nsB4df$`Pacific Blue-A`),
            mean(nsC1df$`Pacific Blue-A`)
            # ,mean(nsC2df$`Pacific Blue-A`),mean(nsC3df$`Pacific Blue-A`),mean(nsC4df$`Pacific Blue-A`)

    ),
    'PO'= c(mean(nsA1df$`Pacific Orange-A`),mean(nsA2df$`Pacific Orange-A`),mean(nsA3df$`Pacific Orange-A`),mean(nsA4df$`Pacific Orange-A`),
            mean(nsB1df$`Pacific Orange-A`),mean(nsB2df$`Pacific Orange-A`),mean(nsB3df$`Pacific Orange-A`),mean(nsB4df$`Pacific Orange-A`),
            mean(nsC1df$`Pacific Orange-A`)
            # ,mean(nsC2df$`Pacific Orange-A`),mean(nsC3df$`Pacific Orange-A`),mean(nsC4df$`Pacific Orange-A`)

    ),
    'index'= c('nsA1df','nsA2df','nsA3df','nsA4df',
               'nsB1df','nsB2df','nsB3df','nsB4df',
               'nsC1df'
               #,'nsC2df','nsC3df','nsC4df'
    )
    )



    nsPB1df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 1:3))
    nsA1df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 1))
    nsA1df$timepoint <- times[1]
    nsA2df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 2))
    nsA2df$timepoint <- times[2]
    nsA3df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 3))
    nsA3df$timepoint <- times[3]
    # nsA4df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 4))
    # nsA4df$timepoint <- times[4]

    nsPB2df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 4:6))
    nsB1df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 1))
    nsB1df$timepoint <- times[4]
    nsB2df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 2))
    nsB2df$timepoint <- times[5]
    nsB3df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 3))
    nsB3df$timepoint <- times[6]
    # nsB4df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 4))
    # nsB4df$timepoint <- times[8]

    nsPB3df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 7:9))
    nsC1df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 1))
    nsC1df$timepoint <- times[7]
    nsC2df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 2))
    nsC2df$timepoint <- times[8]
    nsC3df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 3))
    nsC3df$timepoint <- times[9]
    # nsC4df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 4))
    # nsC4df$timepoint <- times[12]

    # histdb <- c(nsA1,nsA2,nsA3 ,nsA4,nsB1,nsB2 ,nsB3,nsB4,nsC1,nsC2,nsC3,nsC4)  #,nsC1,nsD1
    histdf <- plyr::rbind.fill(nsA1df,nsA2df,nsA3df
                               #,nsA4df
                               ,nsB1df,nsB2df ,nsB3df
                               # ,nsB4df
                               ,nsC1df,nsC2df,nsC3df
                               # ,nsC4df
    )
    columnnames <- make.names(colnames(histdf))
    columnnames <- gsub('[.]', '_', columnnames)

    colnames(histdf) <- columnnames
    channelnames <-  colnames(histdf)
    #  channelnames <- channelnames[c(8,11,12,13,14,22)]


    histdf$timepoint1 <- as.character(histdf$timepoint)
    #histdf$timepoint1 <- factor(histdf$timepoint1, levels = sort(times), ordered=TRUE)

    histdf$timepoint1 <-factor( histdf$timepoint1  ,times1$condition)

    channelname <-channelnames[9] #channelnames1[k]
    histdf$channel <- channelnames[9] #channelnames1[k]
    histdf$protein <-    'protein_a' #add input for marker name filename[k]
    # histdf$date <- date1
    # histdf$method <- ANmet
    #histdf$FSCSSC_gateSD <- STD
    #histdf$costain <- 1
    # histdf$sample <- t[k]
    # histdf$input <- 'step'

    histdf2 <- histdf
    names(histdf2)[names(histdf2) == channelname] <- 'channel1'


    histdf2 <- histdf2[histdf2$'channel1' > 0, ]
    # channelname <- unique(kbnew1$channel)
    # histdf <- histdf[histdf$channel > 0, ]

    kbnew1n <- reshape2::melt(histdf2,id=c('protein','timepoint'))  #,'cumex'
    kbnew1n <- subset(kbnew1n, variable=='channel1') #'channel'
    # kbnew1n <- subset(kbnew1n, variable=='channel') #'channel'
    kbnew1n$tpoint <- factor(kbnew1n$timepoint, levels = sort(unique(kbnew1n$timepoint)), ordered=T)
    # kbnew1n$input <- factor(kbnew1n$input, levels = sort(unique(kbnew1n$input),decreasing = F), ordered=T)
    kbnew1n$value <- as.numeric(kbnew1n$value)

    threshold <-  as.numeric(input$threshold)   #example threshold 1000

    FS=12
    p<-ggplot2::ggplot()+
      # coord_trans( x="log10") +
      ggplot2::scale_color_manual(values = c("#EC008C","black"))+  #2E3192 cparp #2E3192 pp38 #00A651 pS6 #F7941D noxa
      ggplot2::scale_fill_manual(values = c("#EC008C","black"))+

      # ggtitle(paste(prs_name))+
      ggplot2::scale_x_continuous(
        trans = scales::log10_trans()
        ,breaks = scales::trans_breaks("log10", function(x) 10^x,n = 2)
        ,labels = scales::trans_format("log10", scales::math_format(10^.x))
        #,limits = c(exp(1)^powerofx[i],exp(1)^powerof[i])
        # ,limits = c(exp(1)^2,exp(1)^12)
        ,limits = c(10^1,10^5)

      )+
      # xlab(paste('I [',prs_name,']', sep = ''))+ #proteins[i]
      # xlab(prs_name)+ #proteins[i]
      # ylab('time [min]')+
      ggridges::geom_density_ridges(data=kbnew1n, aes(x =  value ,y= tpoint,
                                                      # fill=input,
                                                      #  fill=NA,
                                                      # color=input,size=date
      ) ,alpha = .1
      ,show.legend = FALSE)+  #log(value)  #,size=date
      ggplot2::scale_size_discrete(range = c(0.3, 0.31))+
      ggplot2::geom_vline(xintercept =  threshold, color='red',size=0.5) +
      ggridges::theme_ridges(grid = T)+
      ggplot2::theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.position="none",
        plot.title = element_text(size = FS),
        axis.text.y= element_text(size = FS),
        axis.title.x = element_text(size = FS,hjust = 0.5,margin = margin(t = 1, r = 1, b = 1, l = 1)),
        # axis.title.y = element_text(size = FS,hjust = 0.5,margin = margin(t = 1, r = 1, b = 1, l = 1)),
        axis.title.y = element_blank(),
        axis.text.x= element_text(size = FS),
        axis.ticks.y = element_line(colour = 'black', size = 1*0.352778, linetype = 'solid'),
        axis.ticks.x = element_line(colour = 'black', size = 1*0.352778, linetype = 'solid'),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "black", size = 1*0.352778, linetype = "solid"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))
    # print(p)



    Onfraction <- NULL
    # z_score <- NULL
    times <- unique(kbnew1n$timepoint)

    for (l in seq_along(times)) {
      kbnew1n1 <- subset(kbnew1n, timepoint==times[l])
      #  z_score[l] <- (median(dists_zs_1$channel)-globalmean)/globalsd
      # z_score[l] <- (mean(kbnew1n_cparp1$value)-globalmean)/globalsd
      Onfraction[l] <- 100*nrow(subset(kbnew1n1, value > threshold))/nrow(kbnew1n1)
    }

    kbnew1n2 <- data.frame(
      #'protein'=rep(prot_names[i],length(times)),
      # 'protein'=TOI,
      #'z_score'=z_score,
      #add the on-fraction to the data frame containing the z-score
      'OnFraction'=Onfraction,
      'condition'=times)


    kbnew1n2$condition <- factor(kbnew1n2$condition, levels = rev(kbnew1n2$condition) )#, ordered=T)

    FS=12
    p3 <-ggplot2::ggplot()+
      ggplot2::geom_line(data = kbnew1n2, aes(x=condition, y=OnFraction))+
      ggplot2::geom_bar(data = kbnew1n2, aes(x=condition, y=OnFraction),stat = 'identity')+
      ggplot2::coord_flip()+
      ggplot2::theme(
        panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.position="none",
        plot.title = element_text(size = FS),
        # axis.text.y= element_text(size = FS),
        axis.text.y= element_blank(),
        axis.title.x = element_text(size = FS,hjust = 0.5,margin = margin(t = 1, r = 1, b = 1, l = 1)),
        # axis.title.y = element_text(size = FS,hjust = 0.5,margin = margin(t = 1, r = 1, b = 1, l = 1)),
        axis.title.y = element_blank(),
        axis.text.x= element_text(size = FS), #,angle = 30
        axis.ticks.y = element_line(colour = 'black', size = 1*0.352778, linetype = 'solid'),
        axis.ticks.x = element_line(colour = 'black', size = 1*0.352778, linetype = 'solid'),
        axis.ticks.length = unit(0.1, "cm"),
        axis.line = element_line(colour = "black", size = 1*0.352778, linetype = "solid"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))
    #print(p3)

    p4 <- ggpubr::ggarrange(p,p3, nrow = 1
                            #, labels = c("B", "C")
    )
    print(p4)
  })



  output$distribution <- shiny::downloadHandler(
    filename = function() {
      bw_in <- as.character(input$bw)
      SD_in <- as.character(input$SD)

      paste(bw_in,"_",SD_in, "_db.csv", sep = "")
    },
    content = function(file) {
      SD_in <- as.numeric(input$SD)
      #state <- input$state
      gateddata <- NULL
      datas <- user_data()
      for (i in seq_along(datas) ) {
        #select gate
        morphGate <- flowCore::norm2Filter(filterId = "MorphologyGate", "FSC-A", "SSC-A", scale = SD_in)
        gateddata <- cbind(gateddata, Subset(datas, morphGate))
      }


      total_channels <- ncol(flowCore::exprs(gateddata[[1]]))

      tf1 <- flowCore::transformList(from = colnames(gateddata[[1]])[1:total_channels], tfun = asinh)
      # tf2 <- transformList(from = colnames(gateddata[[2]])[1:25], tfun = asinh)
      # tf3 <- transformList(from = colnames(gateddata[[3]])[1:25], tfun = asinh)
      apolog1 <- tf1 %on% gateddata[[1]]
      # apolog2 <- tf2 %on% gateddata[[2]]
      # apolog3 <- tf3 %on% gateddata[[3]]
      #add all apolog data in one variable
      # apologs <- c(apolog1,apolog2,apolog3)


      #par(mar = c(2, 4, 2, .1))
      bw_in <- as.numeric(input$bw)
      #state <- input$state

      bargate <- flowStats::curv2Filter('Pacific Orange-A','Pacific Blue-A',bwFac = bw_in)
      #  times=c( 0,2,5,10,15,20,30,60,120,180,240,300)
      times1 <-  user_key()
      names(times1) <- 'condition'
      instances <- nrow(times1$condition)
      times <- times1$condition
      AR1 <-  flowCore::split(apolog1, bargate, population='area 1')
      AR1count<- nrow(flowCore::exprs(AR1[[1]]))
      # if (AR1count>1000) {
      #  print(xyplot( `Pacific Orange-A` ~`Pacific Blue-A` ,  AR1[[1]], smooth=F, abs = TRUE))
      AR2 <-  flowCore::split(apolog1, bargate, population='area 2')
      AR2count<- nrow(flowCore::exprs(AR2[[1]]))
      # if (AR2count>1000) {
      AR3 <-  flowCore::split(apolog1, bargate, population='area 3')
      AR3count<- nrow(flowCore::exprs(AR3[[1]]))
      # if (AR3count>1000) {
      AR4 <-  flowCore::split(apolog1, bargate, population='area 4')
      AR4count<- nrow(flowCore::exprs(AR4[[1]]))
      # if (AR4count>1000) {

      AR5 <-  flowCore::split(apolog1, bargate, population='area 5')
      AR5count<- nrow(flowCore::exprs(AR5[[1]]))
      # if (AR5count>1000) {
      AR6 <-  flowCore::split(apolog1, bargate, population='area 6')
      AR6count<- nrow(flowCore::exprs(AR6[[1]]))
      # if (AR6count>1000) {
      AR7 <-  flowCore::split(apolog1, bargate, population='area 7')
      AR7count<- nrow(flowCore::exprs(AR7[[1]]))
      # if (AR7count>1000) {
      AR8 <-  flowCore::split(apolog1, bargate, population='area 8')
      AR8count<- nrow(flowCore::exprs(AR8[[1]]))
      # if (AR8count>1000) {

      AR9 <-  flowCore::split(apolog1, bargate, population='area 9')
      AR9count<- nrow(flowCore::exprs(AR9[[1]]))
      # if (AR9count>1000) {
      # AR10 <-  split(apolog1, bargate, population='area 10')
      # AR10count<- nrow(exprs(AR10[[1]]))
      # # if (AR10count>1000) {
      # AR11 <-  split(apolog1, bargate, population='area 11')
      # AR11count<- nrow(exprs(AR11[[1]]))
      # # if (AR11count>1000) {
      # AR12 <-  split(apolog1, bargate, population='area 12')
      # AR12count<- nrow(exprs(AR12[[1]]))
      # # if (AR12count>1000) {

      ##revert data for histogram
      ntfAR1 <- flowCore::transformList(from = colnames(AR1[[1]])[1:total_channels], tfun = sinh)
      ntfAR2 <- flowCore::transformList(from = colnames(AR2[[1]])[1:total_channels], tfun = sinh)
      ntfAR3 <- flowCore::transformList(from = colnames(AR3[[1]])[1:total_channels], tfun = sinh)
      ntfAR4 <- flowCore::transformList(from = colnames(AR4[[1]])[1:total_channels], tfun = sinh)
      ntfAR5 <- flowCore::transformList(from = colnames(AR5[[1]])[1:total_channels], tfun = sinh)
      ntfAR6 <- flowCore::transformList(from = colnames(AR6[[1]])[1:total_channels], tfun = sinh)
      ntfAR7 <- flowCore::transformList(from = colnames(AR7[[1]])[1:total_channels], tfun = sinh)
      ntfAR8 <- flowCore::transformList(from = colnames(AR8[[1]])[1:total_channels], tfun = sinh)
      ntfAR9 <- flowCore::transformList(from = colnames(AR9[[1]])[1:total_channels], tfun = sinh)
      # ntfAR10 <- transformList(from = colnames(AR10[[1]])[1:total_channels], tfun = sinh)
      # ntfAR11 <- transformList(from = colnames(AR11[[1]])[1:total_channels], tfun = sinh)
      # ntfAR12 <- transformList(from = colnames(AR12[[1]])[1:total_channels], tfun = sinh)


      nsAR1 <- ntfAR1 %on% AR1[[1]]
      nsAR2 <- ntfAR2 %on% AR2[[1]]
      nsAR3 <- ntfAR3 %on% AR3[[1]]
      nsAR4 <- ntfAR4 %on% AR4[[1]]
      nsAR5 <- ntfAR5 %on% AR5[[1]]
      nsAR6 <- ntfAR6 %on% AR6[[1]]
      nsAR7 <- ntfAR7 %on% AR7[[1]]
      nsAR8 <- ntfAR8 %on% AR8[[1]]
      nsAR9 <- ntfAR9 %on% AR9[[1]]
      # nsAR10 <- ntfAR10 %on% AR10[[1]]
      # nsAR11 <- ntfAR11 %on% AR11[[1]]
      # nsAR12 <- ntfAR12 %on% AR12[[1]]



      nsA1df <-  FFtoDF(nsAR1)
      nsA1df$PBmean <- mean(nsA1df$`Pacific Blue-A`)
      nsA1df$POmean <- mean(nsA1df$`Pacific Orange-A`)
      #nsA1df$timepoint <- times[1]
      nsA2df <-  FFtoDF(nsAR2)
      nsA2df$PBmean <- mean(nsA2df$`Pacific Blue-A`)
      nsA2df$POmean <- mean(nsA2df$`Pacific Orange-A`)
      #nsA2df$timepoint <- times[2]
      nsA3df <-  FFtoDF(nsAR3)
      nsA3df$PBmean <- mean(nsA3df$`Pacific Blue-A`)
      nsA3df$POmean <- mean(nsA3df$`Pacific Orange-A`)
      #nsA3df$timepoint <- times[3]
      nsA4df <-  FFtoDF(nsAR4)
      nsA4df$PBmean <- mean(nsA4df$`Pacific Blue-A`)
      nsA4df$POmean <- mean(nsA4df$`Pacific Orange-A`)
      #nsA4df$timepoint <- times[4]
      nsB1df <-  FFtoDF(nsAR5)
      nsB1df$PBmean <- mean(nsB1df$`Pacific Blue-A`)
      nsB1df$POmean <- mean(nsB1df$`Pacific Orange-A`)
      #nsB1df$timepoint <- times[5]
      nsB2df <-  FFtoDF(nsAR6)
      nsB2df$PBmean <- mean(nsB2df$`Pacific Blue-A`)
      nsB2df$POmean <- mean(nsB2df$`Pacific Orange-A`)
      #nsB2df$timepoint <- times[6]
      nsB3df <-  FFtoDF(nsAR7)
      nsB3df$PBmean <- mean(nsB3df$`Pacific Blue-A`)
      nsB3df$POmean <- mean(nsB3df$`Pacific Orange-A`)
      #nsB3df$timepoint <- times[7]
      nsB4df <-  FFtoDF(nsAR8)
      nsB4df$PBmean <- mean(nsB4df$`Pacific Blue-A`)
      nsB4df$POmean <- mean(nsB4df$`Pacific Orange-A`)
      #nsB4df$timepoint <- times[8]
      nsC1df <-  FFtoDF(nsAR9)
      nsC1df$PBmean <- mean(nsC1df$`Pacific Blue-A`)
      nsC1df$POmean <- mean(nsC1df$`Pacific Orange-A`)
      # # nsC1df$timepoint <- times[9]
      # nsC2df <-  FFtoDF(nsAR10)
      # nsC2df$PBmean <- mean(nsC2df$`Pacific Blue-A`)
      # nsC2df$POmean <- mean(nsC2df$`Pacific Orange-A`)
      # #nsC2df$timepoint <- times[9]
      # nsC3df <-  FFtoDF(nsAR11)
      # nsC3df$PBmean <- mean(nsC3df$`Pacific Blue-A`)
      # nsC3df$POmean <- mean(nsC3df$`Pacific Orange-A`)
      # #nsC3df$timepoint <- times[10]
      # nsC4df <-  FFtoDF(nsAR12)
      # nsC4df$PBmean <- mean(nsC4df$`Pacific Blue-A`)
      # nsC4df$POmean <- mean(nsC4df$`Pacific Orange-A`)
      #nsC4df$timepoint <- times[11]



      nsdfs <- rbind.fill(nsA1df,nsA2df,nsA3df,nsA4df,nsB1df,nsB2df,nsB3df,nsB4df,
                          nsC1df
                          #,nsC2df,nsC3df,nsC4df
      )

      ARDF <- data.frame('Areas'= c( 'area1','area2','area3',
                                     'area4','area5','area6',
                                     'area7','area8','area9'
                                     # ,'area10','area11','area12'
      ),
      'PB'= c(mean(nsA1df$`Pacific Blue-A`),mean(nsA2df$`Pacific Blue-A`),mean(nsA3df$`Pacific Blue-A`),mean(nsA4df$`Pacific Blue-A`),
              mean(nsB1df$`Pacific Blue-A`),mean(nsB2df$`Pacific Blue-A`),mean(nsB3df$`Pacific Blue-A`),mean(nsB4df$`Pacific Blue-A`),
              mean(nsC1df$`Pacific Blue-A`)
              # ,mean(nsC2df$`Pacific Blue-A`),mean(nsC3df$`Pacific Blue-A`),mean(nsC4df$`Pacific Blue-A`)

      ),
      'PO'= c(mean(nsA1df$`Pacific Orange-A`),mean(nsA2df$`Pacific Orange-A`),mean(nsA3df$`Pacific Orange-A`),mean(nsA4df$`Pacific Orange-A`),
              mean(nsB1df$`Pacific Orange-A`),mean(nsB2df$`Pacific Orange-A`),mean(nsB3df$`Pacific Orange-A`),mean(nsB4df$`Pacific Orange-A`),
              mean(nsC1df$`Pacific Orange-A`)
              # ,mean(nsC2df$`Pacific Orange-A`),mean(nsC3df$`Pacific Orange-A`),mean(nsC4df$`Pacific Orange-A`)

      ),
      'index'= c('nsA1df','nsA2df','nsA3df','nsA4df',
                 'nsB1df','nsB2df','nsB3df','nsB4df',
                 'nsC1df'
                 #,'nsC2df','nsC3df','nsC4df'
      )
      )



      nsPB1df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 1:3))
      nsA1df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 1))
      nsA1df$timepoint <- times[1]
      nsA2df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 2))
      nsA2df$timepoint <- times[2]
      nsA3df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 3))
      nsA3df$timepoint <- times[3]
      # nsA4df <- subset(nsPB1df, POmean==maxN(unique(nsPB1df$PO), 4))
      # nsA4df$timepoint <- times[4]

      nsPB2df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 4:6))
      nsB1df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 1))
      nsB1df$timepoint <- times[4]
      nsB2df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 2))
      nsB2df$timepoint <- times[5]
      nsB3df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 3))
      nsB3df$timepoint <- times[6]
      # nsB4df <- subset(nsPB2df, POmean==maxN(unique(nsPB2df$PO), 4))
      # nsB4df$timepoint <- times[8]

      nsPB3df <- subset(nsdfs, PBmean==maxN(ARDF$PB, 7:9))
      nsC1df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 1))
      nsC1df$timepoint <- times[7]
      nsC2df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 2))
      nsC2df$timepoint <- times[8]
      nsC3df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 3))
      nsC3df$timepoint <- times[9]
      # nsC4df <- subset(nsPB3df, POmean==maxN(unique(nsPB3df$PO), 4))
      # nsC4df$timepoint <- times[12]

      # histdb <- c(nsA1,nsA2,nsA3 ,nsA4,nsB1,nsB2 ,nsB3,nsB4,nsC1,nsC2,nsC3,nsC4)  #,nsC1,nsD1
      histdf <- plyr::rbind.fill(nsA1df,nsA2df,nsA3df
                                 #,nsA4df
                                 ,nsB1df,nsB2df ,nsB3df
                                 # ,nsB4df
                                 ,nsC1df,nsC2df,nsC3df
                                 # ,nsC4df
      )
      columnnames <- make.names(colnames(histdf))
      columnnames <- gsub('[.]', '_', columnnames)

      colnames(histdf) <- columnnames
      channelnames <-  colnames(histdf)
      #  channelnames <- channelnames[c(8,11,12,13,14,22)]


      histdf$timepoint1 <- as.character(histdf$timepoint)
      #histdf$timepoint1 <- factor(histdf$timepoint1, levels = sort(times), ordered=TRUE)

      histdf$timepoint1 <-factor( histdf$timepoint1  ,times1$condition)


      channelname <-channelnames[9] #channelnames1[k]
      histdf$channel <- channelnames[9] #channelnames1[k]
      histdf$protein <- 'protein_a' #add input for marker name filename[k]
      # histdf$date <- date1
      # histdf$method <- ANmet
      #histdf$FSCSSC_gateSD <- STD
      #histdf$costain <- 1
      # histdf$sample <- t[k]
      #histdf$input <- 'step'
      write.csv(histdf, file, row.names = FALSE)
    }
  )

}
