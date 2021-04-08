# Case1
data = c(3.2, 3.4, 3.7, 3.7, 3.8, 3.9, 4, 4, 4.1, 4.2, 4.7, 4.8, 14, 15)
m = median(data)
ma = mad(data, constant = 1) # Returns the median absolute deviation from the data's median
made = 1.483 * ma

MADe2 = c(m - 2 * made, m + 2 * made)
MADe2

MADe3 = c(m - 3 * made, m + 3 * made)
MADe3