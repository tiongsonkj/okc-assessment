##############################
##  OKC Thunder Assessment  ##
##     Kelvin Tiongson      ##
##############################

# libraries
#install.packages("knitr")
library(knitr)

# load dataset
data = read.csv('shots_data.csv')

# court markers
center_x = 0
center_y = 0
corner_three_limit = 7.8
non_corner_three_distance = 23.75
corner_three_distance = 22
half_circle_radius = non_corner_three_distance - corner_three_limit

### determine shot zones ###
# euclidean distance
distance = function(first, second) {
  x = (second[1] - first[1]) ^ 2
  y = (second[2] - first[2]) ^ 2
  return(sqrt(x + y))
}

# function to determine zone for each row
determine_zone = function(row) {
  x = as.numeric(row['x'])
  y = as.numeric(row['y'])
  
  if (y <= corner_three_limit) {
    # corner 3 or two
    if (x < -corner_three_distance | x > corner_three_distance) {
      return ("C3")
    } else {
      return ("2PT")
    }
  } else {
    # NC3 or 2
    shot_coordinates = c(x, y)
    half_circle_center_coordinates = c(0, 7.8)
    if (distance(shot_coordinates, half_circle_center_coordinates) <= half_circle_radius) {
      return("2PT")
    } else {
      return("NC3")
    }
  }
}


shot_zones = apply(data, 1, determine_zone)
data$shot_zones = shot_zones

### team totals ###
team_a_twos = length(subset(data, shot_zones == "2PT" & team == "Team A")[[1]])
team_a_corner_threes = length(subset(data, shot_zones == "C3" & team == "Team A")[[1]])
team_a_non_corner_threes = length(subset(data, shot_zones == "NC3" & team == "Team A")[[1]])
team_a_total = length(subset(data, team == "Team A")[[1]])

team_b_twos = length(subset(data, shot_zones == "2PT" & team == "Team B")[[1]])
team_b_corner_threes = length(subset(data, shot_zones == "C3" & team == "Team B")[[1]])
team_b_non_corner_threes = length(subset(data, shot_zones == "NC3" & team == "Team B")[[1]])
team_b_total = length(subset(data, team == "Team B")[[1]])


### distribution % ###
team_a__twos_dist = team_a_twos / team_a_total
team_a__non_corner_three_dist = team_a_non_corner_threes / team_a_total
team_a__corner_three_dist = team_a_corner_threes / team_a_total
team_b__twos_dist = team_b_twos / team_b_total
team_b__non_corner_three_dist = team_b_non_corner_threes / team_b_total
team_b__corner_three_dist = team_b_corner_threes / team_b_total

### effective fg% ###
efg = function(fgm, tpm, attempts) {
  return((fgm + (0.5 * tpm)) / attempts)
}

team_a_twos_efg = efg(team_a_twos, 0, team_a_total)
team_a_nc_three_efg = efg(team_a_non_corner_threes, team_a_non_corner_threes, team_a_total)
team_a_cthree_efg = efg(team_a_corner_threes, team_a_corner_threes, team_a_total)
team_b_twos_efg = efg(team_b_twos, 0, team_b_total)
team_b_nc_three_efg = efg(team_b_non_corner_threes, team_b_non_corner_threes, team_b_total)
team_b_cthree_efg = efg(team_b_corner_threes, team_b_corner_threes, team_b_total)


output = data.frame(
  Team = c("A", "A", "A", "B", "B", "B"),
  Zone = c("2PT", "NC3", "C3", "2PT", "NC3", "C3"),
  Shot_Distribution = c(team_a__twos_dist, team_a__non_corner_three_dist, team_a__corner_three_dist,
                        team_b__twos_dist, team_b__non_corner_three_dist, team_b__corner_three_dist),
  EFG = c(team_a_twos_efg, team_a_nc_three_efg, team_a_cthree_efg,
          team_b_twos_efg, team_b_nc_three_efg, team_b_cthree_efg)
)
knitr::kable(output, format = "markdown")
