classesTest <- nightlights_lat_lon[, Classe := ifelse(radiance < 0.65, "low",
                                                      ifelse(radiance < 2.55, "medium",
                                                             "high"))]

classesCount <- nightlights_lat_lon[, .(number = .N), by = Classe]
