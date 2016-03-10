# ===============================================
# prepare

d = read.csv('prices.csv')
d[['month']] = as.integer(
    format(as.Date(paste('01', d[['month']]), '%d %B %Y'), '%Y%m')
)
d = subset(d, month >= 197601 & month <= 201409)
rownames(d) = NULL
base_cpi = d[nrow(d), 'cpi']
d[['oil']] = d[['oil_nominal']] / d[['cpi']] * base_cpi
d[['gas']] = d[['gas_nominal']] / d[['cpi']] * base_cpi
d[['diesel']] = d[['diesel_nominal']] / d[['cpi']] * base_cpi

# ===============================================
# plot

pdf('prices.pdf', family = 'Helvetica-Narrow', width = 15, height = 9)
par(mar = c(3, 2, 2, 0) + .1)
par(oma = c(2, 1, 2, 1))

rgb2 = function(...) rgb(..., maxColorValue = 255)
cex0 = .9
col0 = gray(.2)
cols = c(rgb2(10, 120, 250, 230), rgb2(0, 200, 0, 200))
col_event0 = rgb2(230, 95, 33, 35)
col_event = rgb2(230, 45, 33)

# base
xlim = c(0, nrow(d) + 1)
ylim = c(0, max(c(d[['gas']], d[['diesel']]), na.rm = TRUE) + .1)
plot(0, type = 'n', axes = FALSE, xlim = xlim, ylim = ylim,
     xaxs = 'i', yaxs = 'i', xlab = '', ylab = '')

# events - intervals
events = read.csv('events.csv')
for (i in seq(nrow(events))) {
    start = events[i, 'start']
    end = events[i, 'end']
    if (!is.na(start) && !is.na(end)) {
        x0 = match(start, d[['month']])
        x1 = match(end, d[['month']])
        polygon(c(x0, x0, x1, x1), c(ylim, rev(ylim)), border = FALSE,
                col = col_event0)
    }
}

# x-axis
sel_m1 = which(substr(d[['month']], 5, 6) == '01')
sel_y5 = which(substr(d[['month']], 4, 4) %in% c('0', '5'))
sel = setdiff(sel_m1, sel_y5)
axis(1, sel, substr(d[sel, 'month'], 4, 4),tick = FALSE, line = -1,
     cex.axis = cex0, col.axis = col0)
sel = intersect(sel_m1, sel_y5)
axis(1, sel, substr(d[sel, 'month'], 1, 4), lwd = 0, lwd.ticks = 1,
     cex.axis = cex0, col.axis = col0, col.ticks = col0)
axis(1, seq(xlim[1], xlim[2]), FALSE, lwd.ticks = 0, col = col0)

# y-axis
ticks = seq(1, ylim[2])
abline(h = ticks, col = gray(.8), lwd = 2)
axis(2, ticks, paste0('$', ticks), lwd = 0, las = 1,
     cex.axis = cex0, col.axis = col0)

# lines
lines(d[['diesel']], lwd = 2.2, col = cols[2])
lines(d[['gas']], lwd = 2.2, col = cols[1])

# text
text(match(201206, d[['month']]), 3.2, 'Gasoline', adj = 0, col = cols[1])
text(match(201206, d[['month']]), 4.4, 'Diesel', adj = 0, col = cols[2])

# events - points
x = match(events[['month']], d[['month']])
y = d[x, 'gas']
points(x, y, pch = 16, col = col_event)
text(x, y, events[['event']], pos = 4, col = col_event, font = 4)

# title
mtext('Gasoline Regular Grade and Diesel Monthly Retail Prices',
      side = 3, outer = TRUE, adj = 0, cex = 1.5, font = 2)
mtext('Source: U.S. Energy Information Administration, Short-Term Energy and Winter Fuels Outlook, October 7, 2014',
      side = 1, outer = TRUE, adj = 0, cex = cex0, col = col0)
mtext('* Dollars per gallon adjusted for inflation to September 2014 dollars',
      side = 1, outer = TRUE, adj = 1, cex = cex0, col = col0)

dev.off()
