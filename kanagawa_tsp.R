library(GA)
library(ggmap)


#Input Data
citynames <- c("Yokohama, Kanagawa, Japan", "Kawasaki, Kanagawa, Japan", 
               "Yokosuka, Kanagawa, Japan", "Kamakura, Kanagawa, Japan", 
               "Zushi, Kanagawa, Japan", "Hayama, Kanagawa, Japan", "Miura, Kanagawa, Japan", 
               "Yamato, Kanagawa, Japan", "Zama, Kanagawa, Japan", "Sagamihara, Kanagawa, Japan", 
               "Ayase, Kanagawa, Japan", "Ebina, Kanagawa, Japan", "Atsugi, Kanagawa, Japan", 
               "Aikawa, Kanagawa, Japan", "Kiyokawa, Kanagawa, Japan", "Fujisawa, Kanagawa, Japan", 
               "Chigasaki, Kanagawa, Japan", "Samukawa, Kanagawa, Japan", "Hiratsuka, Kanagawa, Japan", 
               "Isehara, Kanagawa, Japan", "Hadano, Kanagawa, Japan", "Oiso, Kanagawa, Japan", 
               "Ninomiya, Kanagawa, Japan", "Nakai, Kanagawa, Japan", "Ooi, Kanagawa, Japan", 
               "Kaisei, Kanagawa, Japan", "Matsuda, Kanagawa, Japan", "Yamakita, Kanagawa, Japan", 
               "Odawara, Kanagawa, Japan", "Minamiashigara, Kanagawa, Japan", "Hakone, Kanagawa, Japan", 
               "Yugawara, Kanagawa, Japan", "Manaduru, Kanagawa, Japan")


#Calc Dist
D <- matrix(0, nrow=length(citynames), ncol=length(citynames))
for(i in 1:length(citynames)) {
  for(j in i:length(citynames)) {
    D[i, j] = mapdist(citynames[i], citynames[j])$km
  }
  for(j in 1:i) {
    D[i, j] = D[j, i]
  }
}


#Fitness Function
fitness <- function(x) {
  sum <- 0
  for(i in 2:length(x)) {
    sum <- sum + D[x[i - 1], x[i]]
  }
  sum <- sum + D[x[i], x[1]]
  return(-sum)
}


#GA
min <- 1
max <- nrow(D)
GA <- ga(type="permutation", fitness=fitness, 
         min=min, max=max, popSize=100, maxiter=5000, run=500)
summary(GA)
plot(GA)

solution <- GA@solution[1, ]


#GoogleMap Plot
solution_lonlat <- geocode(citynames[solution])
lon <- append(solution_lonlat$lon, solution_lonlat$lon[1])
lat <- append(solution_lonlat$lat, solution_lonlat$lat[1])
solution_lonlat <- list(lon=lon, lat=lat)


solution_plot <- function(sol) {
  map <- qmap("Atsugi, Kanagawa, Japan", zoom=10)
  
  for(i in 2:length(solution_lonlat$lon)) {
    from <- c(solution_lonlat[["lon"]][i-1], solution_lonlat[["lat"]][i-1])
    to <- c(solution_lonlat[["lon"]][i], solution_lonlat[["lat"]][i])
    r <- route(from, to, structure="route")
    
    map <- map +
      geom_path(
        aes(x=lon, y=lat), color="blue", size=1, 
        data = r, lineend="round"
      )
  }
  
  dataframe <- structure(
    list(
      lon=solution_lonlat[["lon"]], 
      lat=solution_lonlat[["lat"]]
    ), 
    class="data.frame"
  )
  map <- map + 
    geom_point(
      aes(x=lon, y=lat), color="red", size=1.2, 
      data=dataframe
    )
  
  map
}

solution_plot(solution)
