# Authors: Hosein Hashemi, Simon Driver & Soheil Koushan
SegPlot = function(segimage, segcol, linewd = 4){
  segwid = dim(segimage)
  segIDs = sort(unique(as.vector(mergeSeg_F814W_r)))
  segIDs = segIDs[2:length(segIDs)]
  
  if (missing(segcol)) {
    segcol = c(rainbow(length(segIDs)))
  } else {
    segcol = rep(segcol,length(segIDs))
  }
  
  for (i in 1:length(segIDs)){
    seg = segimage
    seg[] = match(segimage,segIDs[i])
    seg[is.na(seg)]=0
    contour(1:(segwid[1]),1:(segwid[2]),seg, add = T, 
            col = segcol[i], zlim = c(0,1), drawlabels = FALSE, 
            nlevels = 1,lw = linewd)    
  }
}