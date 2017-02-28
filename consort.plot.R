# consort.plot.R
# template for a CONSORT flow chart
# Feb 2017

library(diagram)
# dummy recruitment numbers
consort.assessed = 400
consort.excluded = 200
consort.declined = 75
consort.not.meet = 75
consort.other = 50
consort.randomised = consort.assessed - consort.excluded
allocate.control = allocate.treatment = 100
died.control = died.treatment = 1
lost.fu.control = died.control + 0
lost.fu.treatment = died.treatment + 0
excluded.control = 0
excluded.treatment = 0
analysed.treatment = allocate.treatment - excluded.treatment
analysed.control = allocate.control - excluded.treatment
pp.treatment = allocate.treatment - 5
pp.control = allocate.control - 5
# labels
b = c('Enrollment', 'Allocation', 'Follow-up', 'Analysis')
l1 = paste('Assessed for eligibility (n=', consort.assessed, ')', sep='') # numbers from above
l2 = paste('Excluded (n=', consort.excluded, ')\n',
           '•   Declined to participate (n=', consort.declined, ')\n',
           '•   Not meeting inclusion criteria (n=', consort.not.meet,')\n',
           '•   Other reasons (n=', consort.other, ')', sep='')
l3 = paste('Randomised (n=', consort.randomised,')', sep='')
l4 = paste('Allocated to control (n=', allocate.control ,')\n', sep='')
l5 = paste('Allocated to intervention (n=', allocate.treatment ,')\n',
           '• Received intervention (n=', allocate.treatment ,')\n', sep='')
l6 = paste('Lost to follow-up (n=', lost.fu.control, ')\n', # control lost to fu
           '•   Died (n=', died.control,')', sep='')
l7 = paste('Lost to follow-up (n=', lost.fu.treatment, ')\n', # treatment lost to fu
           '•   Died (n=', died.treatment,')', sep='')
l8 = paste('Analysed (n=', analysed.control, ')\n', 
           '•   Per protocol (n=', pp.control,')', sep='')
l9 = paste('Analysed (n=', analysed.treatment, ')\n', 
           '•   Per protocol (n=', pp.treatment,')', sep='')
labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, b)
n.labels = length(labels)
### make data frame of box chars
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.5	0.94	white	square	0.25	0.16
2	0.76	0.82	white	square	0.28	0.21
3	0.5	0.7	white	square	0.25	0.15
4	0.26	0.5	white	square	0.23	0.2
5	0.76	0.5	white	square	0.23	0.2
6	0.26	0.33	white	square	0.2	0.2
7	0.76	0.33	white	square	0.2	0.2
8	0.26	0.15	white	square	0.2	0.2
9	0.76	0.15	white	square	0.2	0.2
10	0.1	0.95	light blue	round	0.7	0.035
11	0.51	0.6	light blue	round	0.7	0.035
12	0.51	0.411	light blue	round	0.7	0.035
13	0.51	0.235	light blue	round	0.7	0.035')
pos = as.matrix(subset(frame, select=c(x, y)))
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[3, 1] = "' '"
M[4, 3] = "' '"
M[5, 3] = "' '"
M[6, 4] = "' '"
M[7, 5] = "' '"
M[8, 6] = "' '"
M[9, 7] = "' '"
tcol = rep('black', n.labels)
to.blank = c(2,4:9)
tcol[to.blank] = 'transparent' # blank some boxes to add text as right aligned
#postscript('consort.flow.eps', width=7.5, height=7, horiz=F)
tiff('consort.flow.tif', width=7.5, height=7, units='in', res=300, compression = 'lzw')
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
# add left-aligned text; -0.185 controls the horizontal indent
for (i in to.blank){
  text(x=pos[i,1] - 0.185, y=pos[i,2], adj=c(0,0.5), labels=labels[i]) # minus controls text position
}
# extra arrow to excluded
arrows(x0=0.5, x1=0.55, y0=0.82, length=0.12)
dev.off()