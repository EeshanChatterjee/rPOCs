# Big Yellow Analysis
require(reshape)
require(ggplot2)
# ========================================================================
# Member functions
# ========================================================================
statcalc = function(spend_df){  
  total_imps = sum(spend_df$imps)
  total_clicks = sum(spend_df$clicks)
  total_convs = sum(spend_df$total_convs)
  ctr = total_clicks*100/total_imps
  cvr = total_convs*100/total_imps
  total_spend = sum(spend_df$media_cost)
  cpi = total_spend/total_imps
  cpc = total_spend/total_clicks
  cpa = total_spend/total_convs
  
  df.stats = list(total_imps = total_imps,total_clicks = total_clicks,
                  total_convs = total_convs,ctr = ctr,cvr = cvr,
                  total_spend = total_spend,cpi = cpi,cpc = cpc,cpa = cpa)
  return(df.stats)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)),width = unit(1, "npc"), height = unit(1, "npc")))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_perf_by_lineitem = function(lineitem_stats){
  stst.df = as.data.frame(t(lineitem_stats))
  rownames(stst.df) <- NULL
  
  names(stst.df$lineitem) <- NULL
  stst.df$lineitem = factor(unlist(stst.df$lineitem))
  
  names(stst.df$total_imps) <- NULL
  stst.df$total_imps = unlist(stst.df$total_imps)
  
  names(stst.df$total_clicks) <- NULL
  stst.df$total_clicks = unlist(stst.df$total_clicks)
  
  names(stst.df$total_convs) <- NULL
  stst.df$total_convs = unlist(stst.df$total_convs)
  
  names(stst.df$total_spend) <- NULL
  stst.df$total_spend = unlist(stst.df$total_spend)
  
  names(stst.df$ctr) <- NULL
  stst.df$ctr = unlist(stst.df$ctr)
  
  names(stst.df$cvr) <- NULL
  stst.df$cvr = unlist(stst.df$cvr)
  
  names(stst.df$cpi) <- NULL
  stst.df$cpi = unlist(stst.df$cpi)
  
  names(stst.df$cpc) <- NULL
  stst.df$cpc = unlist(stst.df$cpc)
  
  names(stst.df$cpa) <- NULL
  stst.df$cpa = unlist(stst.df$cpa)
  
  plot.totimps = ggplot(data = stst.df, aes(x=lineitem, y = total_imps))
  plot.totimps = plot.totimps + geom_bar(stat="identity") + ggtitle("Impressions Served")
  
  plot.totclicks = ggplot(data = stst.df, aes(x=lineitem, y = total_clicks))
  plot.totclicks = plot.totclicks + geom_bar(stat="identity") + ggtitle("Clicks")
  
  plot.totconvs = ggplot(data = stst.df, aes(x=lineitem, y = total_convs))
  plot.totconvs = plot.totconvs + geom_bar(stat="identity") + ggtitle("Converts")
  
  plot.totspend = ggplot(data = stst.df, aes(x=lineitem, y = total_spend))
  plot.totspend = plot.totspend + geom_bar(stat="identity") + ggtitle("Spend")
  
  cvr.ctr = data.frame(lineitem = unlist(stst.df$lineitem),cvr = unlist(stst.df$cvr),ctr = unlist(stst.df$ctr))
  cvr.ctr.m = melt(cvr.ctr,id.vars = "lineitem")
  
  plot.cvrctr = ggplot(cvr.ctr.m, aes(lineitem,value)) + geom_bar(aes(fill = variable), position = "dodge", stat = "identity")
  plot.cvrctr = plot.cvrctr + ggtitle("CVR / CTR")
  
  plot.cpti = ggplot(data = stst.df, aes(x=lineitem, y = cpi*1000))
  plot.cpti = plot.cpti + geom_bar(stat="identity") + ggtitle("Cost per 1000 impressions")
  
  plot.cpc = ggplot(data = stst.df, aes(x=lineitem, y = cpc))
  plot.cpc = plot.cpc + geom_bar(stat="identity") + ggtitle("Cost per Click")
  
  plot.cpa = ggplot(data = stst.df, aes(x=lineitem, y = cpa))
  plot.cpa = plot.cpa + geom_bar(stat="identity") + ggtitle("Cost per Aquisition")
  
  return(list(plot.totimps = plot.totimps, plot.totclicks = plot.totclicks, plot.totconvs = plot.totconvs,
              plot.totspend = plot.totspend, plot.cvrctr = plot.cvrctr, plot.cpti = plot.cpti,
              plot.cpc = plot.cpc, plot.cpa = plot.cpa))
}

plot_perf_by_day = function(day_stats){
  
  pbd.df = data.frame(lapply(as.data.frame(t(day_stats)),function(x){
    x = unlist(x)
    x = sapply(x, function(dp){
      if(is.infinite(dp)|is.na(dp)){dp = 0}
      return(dp)
    })
    return(x)
  } ))
  
  plot.totimps = ggplot(data = pbd.df, aes(x=date, y = total_imps))
  plot.totimps = plot.totimps + geom_bar(stat="identity") + ggtitle("Impressions Served")
  
  cl.cv = melt(pbd.df[,c("date","total_clicks","total_convs")],id.vars = "date")
  
  plot.clcv = ggplot(data = cl.cv, aes(x=date, y = value)) + ggtitle("Clicks and Converts")
  plot.clcv = plot.clcv + geom_bar(aes(fill = variable),stat="identity", position = "dodge")
  
  plot.spend = ggplot(data = pbd.df, aes(x=date, y = total_spend))
  plot.spend = plot.spend + geom_bar(stat="identity") + ggtitle("Total Spend")
  
  ctr.cvr = melt(pbd.df[,c("date","ctr","cvr")],id.vars = "date")
  
  plot.ctrcvr = ggplot(data = ctr.cvr, aes(x=date, y = value)) + ggtitle("CTR and CVR")
  plot.ctrcvr = plot.ctrcvr + geom_bar(aes(fill = variable),stat="identity", position = "dodge")
  
  plot.cpi = ggplot(data = pbd.df, aes(x=date, y = cpi*1000)) + ggtitle("Cost per 1000 Impressions")
  plot.cpi = plot.cpi + geom_bar(stat="identity")
  
  plot.cpc = ggplot(data = pbd.df, aes(x=date, y = cpc)) + ggtitle("Cost per Click")
  plot.cpc = plot.cpc + geom_bar(stat="identity")
  
  plot.cpa = ggplot(data = pbd.df, aes(x=date, y = cpa)) + ggtitle("Cost per Aquisition")
  plot.cpa = plot.cpa + geom_bar(stat="identity")
  
  return(list(plot.totimps = plot.totimps, plot.clcv = plot.clcv, plot.spend = plot.spend,
              plot.ctrcvr = plot.ctrcvr, plot.cpi = plot.cpi, plot.cpc = plot.cpc,
              plot.cpa = plot.cpa))
}

plot_perf_by_hourofday = function(hod_stats){
  
  hod.df = data.frame(lapply(as.data.frame(t(hod_stats)),function(x){
    x = unlist(x)
    x = sapply(x, function(dp){
      if(is.infinite(dp)|is.na(dp)){dp = 0}
      return(dp)
    })
    return(x)
  } ))
  
  plot.totimps = ggplot(data = hod.df, aes(x=hour, y = total_imps))
  plot.totimps = plot.totimps + geom_bar(stat="identity") + ggtitle("Impressions Served")
  
  cl.cv = melt(hod.df[,c("hour","total_clicks","total_convs")],id.vars = "hour")
  
  plot.clcv = ggplot(data = cl.cv, aes(x=hour, y = value)) + ggtitle("Clicks and Converts")
  plot.clcv = plot.clcv + geom_bar(aes(fill = variable),stat="identity", position = "dodge")
  
  plot.spend = ggplot(data = hod.df, aes(x=hour, y = total_spend))
  plot.spend = plot.spend + geom_bar(stat="identity") + ggtitle("Total Spend")
  
  ctr.cvr = melt(hod.df[,c("hour","ctr","cvr")],id.vars = "hour")
  
  plot.ctrcvr = ggplot(data = ctr.cvr, aes(x=hour, y = value)) + ggtitle("CTR and CVR")
  plot.ctrcvr = plot.ctrcvr + geom_bar(aes(fill = variable),stat="identity", position = "dodge")
  
  plot.cpi = ggplot(data = hod.df, aes(x=hour, y = cpi*1000)) + ggtitle("Cost per 1000 Impressions")
  plot.cpi = plot.cpi + geom_bar(stat="identity")
  
  plot.cpc = ggplot(data = hod.df, aes(x=hour, y = cpc)) + ggtitle("Cost per Click")
  plot.cpc = plot.cpc + geom_bar(stat="identity")
  
  plot.cpa = ggplot(data = hod.df, aes(x=hour, y = cpa)) + ggtitle("Cost per Aquisition")
  plot.cpa = plot.cpa + geom_bar(stat="identity")
  
  return(list(plot.totimps = plot.totimps, plot.clcv = plot.clcv, plot.spend = plot.spend,
              plot.ctrcvr = plot.ctrcvr, plot.cpi = plot.cpi, plot.cpc = plot.cpc,
              plot.cpa = plot.cpa))
}

# ========================================================================
# Script
# ========================================================================

analyseApnData = function(bydata){

# Datasets to be rendered
# ------------------------------------------------------------------------

overall_performance = list(
  time_range = character(),
  total_imps = numeric(),
  total_clicks = numeric(),
  total_convs = numeric(),
  overall_ctr = numeric(),
  overall_cvr = numeric(),
  total_spend = numeric(),
  overall_cpi = numeric(),
  overall_cpc = numeric(),
  overall_cpa = numeric()
)

perf_by_io = list(
  perf_by_lineitem = list(),
  perf_by_day = list(),
  perf_by_hour_of_day = list()
)

# lineItemData = list(
#   perf_by_day = list(),
#   perf_by_hour_of_day = list()
#   )

# Read the csv
# ------------------------------------------------------------------------
# bydata = read.csv(text=readLines('~/Downloads/big yellow placemnt report.csv')[-(1:7)],header = T)

# Create additional columns
# ------------------------------------------------------------------------
bydata$time = format(strptime(as.character(bydata[,1]),"%m/%d/%Y %H:%M",tz = "GMT"), "%m/%d/%Y %H:%M %Z")
bydata$io_id = as.numeric(gsub(".+\\(([0-9]+?)\\)","\\1",bydata$insertion_order))
bydata$lineitem_id = as.numeric(gsub(".+\\(([0-9]+?)\\)","\\1",bydata$line_item))
bydata$campaign_id = as.numeric(gsub(".+\\(([0-9]+?)\\)","\\1",bydata$campaign))
bydata$mc_bucket = cut(bydata$media_cost,breaks = (0:ceiling(max(bydata$media_cost))),include.lowest = T)
bydata$hourofday = format(strptime(as.character(bydata[,1]),"%m/%d/%Y %H:%M",tz = "GMT"), "%H")
bydata$date = format(strptime(as.character(bydata[,1]),"%m/%d/%Y %H:%M",tz = "GMT"), "%m/%d/%Y")

# Populate Overall statistics
# ------------------------------------------------------------------------
overall_performance$time_range = paste(range(bydata$time),collapse = ' - ')
overall_performance$total_imps = sum(bydata$imps)
overall_performance$total_clicks = sum(bydata$clicks)
overall_performance$total_convs = sum(bydata$total_convs)
overall_performance$overall_ctr = overall_performance$total_clicks*100/overall_performance$total_imps
overall_performance$overall_cvr = overall_performance$total_convs*100/overall_performance$total_imps
overall_performance$total_spend = sum(bydata$media_cost)
overall_performance$overall_cpi = overall_performance$total_spend/overall_performance$total_imps
overall_performance$overall_cpc = overall_performance$total_spend/overall_performance$total_clicks
overall_performance$overall_cpa = overall_performance$total_spend/overall_performance$total_convs

# Create io_by_day stats
# ------------------------------------------------------------------------
io_split_bydata = split(bydata,bydata$io_id)

io_perf_by_day = lapply(io_split_bydata,function(x){
  x_by_day = split(x,x$date)
  perf = lapply(x_by_day,function(x_day){
    date_stats = statcalc(x_day)
    date_stats$date = unique(x_day$date)[1]
    return(date_stats)
  })
})

# Create io_by_hour_of_day stats
# ------------------------------------------------------------------------
io_perf_by_hour_of_day = lapply(io_split_bydata,function(x){
  x_by_hour_of_day = split(x,x$hourofday)
  perf = lapply(x_by_hour_of_day,function(x_hour){
    hour_stats = statcalc(x_hour)
    hour_stats$hour = unique(x_hour$hourofday)[1]
    return(hour_stats)
  })
})

# Create line_item stats
# ------------------------------------------------------------------------
io_perf_by_lineitem = lapply(io_split_bydata,function(x){
  x_by_lineitem = split(x,x$lineitem_id)
  perf = lapply(x_by_lineitem,function(x_lineitem){
    listats = statcalc(x_lineitem)
    listats$lineitem = unique(x_lineitem$lineitem_id)[1]
    return(listats)
  })
})

names(io_perf_by_lineitem) = names(io_split_bydata)

lineItemData = lapply(io_split_bydata, function(x){
  x_by_lineitem = split(x,x$lineitem_id)
  lineItemAnalysis = lapply(x_by_lineitem, function(li){
    li_by_hour_of_day = split(li,li$hourofday)
    perf_hod = lapply(li_by_hour_of_day,function(x_hour){
      hour_stats = statcalc(x_hour)
      hour_stats$hour = unique(x_hour$hourofday)[1]
      return(hour_stats)
    })

    li_by_day = split(li,li$date)
    perf_day = lapply(li_by_day,function(x_day){
      date_stats = statcalc(x_day)
      date_stats$date = unique(x_day$date)[1]
      return(date_stats)
    })
    return(list(perf_hod = do.call(cbind,perf_hod), perf_day = do.call(cbind,perf_day)))
  })
})

# Create the data structures
# ------------------------------------------------------------------------
io_perf_by_lineitem_df = lapply(io_perf_by_lineitem, function(x){do.call(cbind,x)})
names(io_perf_by_lineitem_df) = names(io_split_bydata)

io_perf_by_hour_of_day_df = lapply(io_perf_by_hour_of_day,function(x){do.call(cbind,x)})
names(io_perf_by_hour_of_day) = names(io_split_bydata)

io_perf_by_day_df = lapply(io_perf_by_day,function(x){do.call(cbind,x)})
names(io_perf_by_day) = names(io_split_bydata)

# Insert the data into the final structure
# ------------------------------------------------------------------------
perf_by_io = lapply(1:length(io_split_bydata), function(x) return(list(perf_by_lineitem = io_perf_by_lineitem_df[[x]],
                                                                  perf_by_day = io_perf_by_day_df[[x]],
                                                                  perf_by_hour_of_day = io_perf_by_hour_of_day_df[[x]])))
names(perf_by_io) = names(io_split_bydata)

# Create the plots for each chart
# ------------------------------------------------------------------------

# Overall Performance


return(list(overall_performance = do.call(rbind,overall_performance), perf_by_io = perf_by_io, lineItemData = lineItemData))

}


# ========================================================================