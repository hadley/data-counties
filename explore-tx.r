tx <- subset(raw, substr(id, 0, 3) == "48.")
tx <- add_tol(tx)

qplot(long, lat, data = subset(tx, tol > 0.05), geom="polygon", group=group)

qplot(long, lat, data = subset(tx, tol > 0.05), colour = change)