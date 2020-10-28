# Planar void ratio as a %
target_planar_void_ratio <- 30

# All sizes in metres (m)
origin_of_cartesian_axes <- 0 # This is just for reference

# Width of the rectangle
rectangle_width <- 0.100

#Height of the rectangle
rectangle_height <- 0.100

# Number of particles in the first generation seeded
number_of_particles <- 500

# Maximum size of a particle (m)
maximum_radius <- 0.07

# Initial size of the particles in metres (m)
minimum_radius_seeded <- 0.1e-4

# Initialisation of variables
centre <- cbind(rep(0, number_of_particles), rep(0, number_of_particles))
radius <- c(rep(0, number_of_particles))

# Generation of a vector of radii, one row for each particle
radius <- minimum_radius_seeded*c(rep(1, number_of_particles))

# Generation of the centres with random positions, one row for each particle
centre[,1] <- minimum_radius_seeded+(rectangle_width-2*minimum_radius_seeded)*runif(number_of_particles)
centre[,2] <- minimum_radius_seeded+(rectangle_height-2*minimum_radius_seeded)*runif(number_of_particles)

# Beginning of the growth process
iteration <- 2
planar_void_ratio <- c(rep(100,20000))
planar_void_ratio[1] <- c(101)

while (planar_void_ratio[iteration] != planar_void_ratio[iteration-1] && planar_void_ratio[iteration]>target_planar_void_ratio){
  iteration=iteration+1;
  
  for (k in 1:number_of_particles){
    condition <- rep(13, 6)
    
    condition[1] <- sum(sqrt((centre[,1]-centre[k,1])^2+(centre[,2]-centre[k,2])^2)<(radius[k]+minimum_radius_seeded+radius))
    condition[2] <- (centre[k,1]+radius[k]+minimum_radius_seeded<rectangle_width)
    condition[3] <- (centre[k,1]-radius[k]-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[4] <- (centre[k,2]+radius[k]+minimum_radius_seeded<rectangle_height)
    condition[5] <- (centre[k,2]-radius[k]-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[6] <- (radius[k]+minimum_radius_seeded<=maximum_radius)
    
    # Logical condition: if all the conditions above are met, the particle k is allowed to grow
    can_it_grow <- condition[1]==1 && condition[2]==1 && condition[3]==1 && condition[4]==1 && condition[5]==1 && condition[6]==TRUE  
    
    # The radius of particle k is increased by "minimum_radius_seeded"
    if (can_it_grow==TRUE){     
      radius[k] <- radius[k]+minimum_radius_seeded
    }
    
  }
  
  # The planar void ratio is calculated for the current iteration    
  squared_sum_of_radii <- sum(radius[1:number_of_particles]^2) 
  planar_void_ratio[iteration] <- (rectangle_width*rectangle_height-pi*squared_sum_of_radii)/(rectangle_width*rectangle_height)*100
  
  if (planar_void_ratio[iteration] < target_planar_void_ratio){ 
    #% If the planar void ratio is lower than the target, the calculation must stop
    break
  }
  
  
}

# Generate new acceptable centres

starting_particles_before_new_generation <- dim(centre)[1]

start_generating_new_centres <- 1

while (start_generating_new_centres==1){
  
  if(planar_void_ratio[iteration]>45){  
    new_centres_desired <- 200
  }
  if(planar_void_ratio[iteration]<45){
    new_centres_desired <- 400
  }
  if(planar_void_ratio[iteration]<35){
    new_centres_desired <- 600
  }  
  
  new_centres_accepted <- 0
  
  while (new_centres_accepted<new_centres_desired){
    candidatex <- minimum_radius_seeded+(rectangle_width-2*minimum_radius_seeded)*runif(1)
    candidatey <- minimum_radius_seeded+(rectangle_height-2*minimum_radius_seeded)*runif(1)
    candidaterad <- minimum_radius_seeded
    candidate_centre <- cbind(candidatex, candidatey)
    centre <- rbind(centre, candidate_centre) # We hypothetically add the candidate centre to the "centre" matrix. This allows us to use vector operations in the conditions below
    radius <- c(radius, candidaterad)
    
    condition <- rep(13, 3)
    
    k = dim(centre)[1]
    
    condition[1] <- sum((sqrt((centre[,1]-centre[nrow(centre),1])^2+(centre[,2]-centre[nrow(centre),2])^2))<(candidaterad+minimum_radius_seeded+radius))
    condition[2] <- (candidatex+candidaterad+minimum_radius_seeded<rectangle_width)
    condition[3] <- (candidatex-candidaterad-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[4] <- (candidatey+candidaterad+minimum_radius_seeded<rectangle_height)
    condition[5] <- (candidatey-candidaterad-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[6] <- (candidaterad+minimum_radius_seeded<=maximum_radius)
    
    is_it_acceptable <- condition[1]==1 && condition[2]==1 && condition[3]==1 && condition[4]==1 && condition[5]==1 && condition[6]==TRUE  
    
    if (is_it_acceptable==TRUE){
      new_centres_accepted <- new_centres_accepted + 1
    }else{
      centre<-(centre[-nrow(centre),]) # If the centre is not acceptable based on the conditions set, we remove the last row of the "centre" matrix, where it was stored hypothetically
      radius <- radius[1:(length(radius)-1)] # The same is true for any unwanted radius
    }
    
  }
  
  start_generating_new_centres <- 0 # Stop generating new centres, as we have reached the number desired
  
}

# Grow the new centres

iteration <- iteration+1
planar_void_ratio[iteration] <- 100 #Resetting this for the next calculation

while (planar_void_ratio[iteration] > target_planar_void_ratio && start_generating_new_centres==0){
  
  iteration <- iteration+1 
  
  for (k in (starting_particles_before_new_generation+1):dim(centre)[1]){
    condition <- rep(13, 6)
    condition[1] <- sum(sqrt((centre[,1]-centre[k,1])^2+(centre[,2]-centre[k,2])^2)<(radius[k]+minimum_radius_seeded+radius))
    condition[2] <- (centre[k,1]+radius[k]+minimum_radius_seeded<rectangle_width)
    condition[3] <- (centre[k,1]-radius[k]-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[4] <- (centre[k,2]+radius[k]+minimum_radius_seeded<rectangle_height)
    condition[5] <- (centre[k,2]-radius[k]-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[6] <- (radius[k]+minimum_radius_seeded<=maximum_radius)
    can_it_grow <- condition[1]==1 && condition[2]==1 && condition[3]==1 && condition[4]==1 && condition[5]==1 && condition[6]==TRUE  
    if (can_it_grow==TRUE){     
      radius[k] <- radius[k]+minimum_radius_seeded
    }
  }
  
  squared_sum_of_radii <- sum(radius[1:dim(centre)[1]]^2) 
  planar_void_ratio[iteration] <- (rectangle_width*rectangle_height-pi*squared_sum_of_radii)/(rectangle_width*rectangle_height)*100
  
  if (planar_void_ratio[iteration] < target_planar_void_ratio){
    start_generating_new_centres <- 0
    break
  }
  
  if(planar_void_ratio[iteration] == planar_void_ratio[iteration-1]){
    start_generating_new_centres <- 1
  }
  
} 

# Generate new acceptable centres
starting_particles_before_automated_loop <- dim(centre)[1]

start_generating_new_centres <- 1

while (start_generating_new_centres==1){

if(planar_void_ratio[iteration]>45){  
  new_centres_desired <- 200
}
if(planar_void_ratio[iteration]<45){
  new_centres_desired <- 400
}
if(planar_void_ratio[iteration]<35){
    new_centres_desired <- 600
}  

new_centres_accepted <- 0

while (new_centres_accepted<=new_centres_desired){
  candidatex <- minimum_radius_seeded+(rectangle_width-2*minimum_radius_seeded)*runif(1)
  candidatey <- minimum_radius_seeded+(rectangle_height-2*minimum_radius_seeded)*runif(1)
  candidaterad <- minimum_radius_seeded
  candidate_centre <- cbind(candidatex, candidatey)
  centre <- rbind(centre, candidate_centre) 
  radius <- c(radius, candidaterad)
  
  condition <- rep(13, 3)

  k = dim(centre)[1]
  
  condition[1] <- sum((sqrt((centre[,1]-centre[nrow(centre),1])^2+(centre[,2]-centre[nrow(centre),2])^2))<(candidaterad+minimum_radius_seeded+radius))
  condition[2] <- (candidatex+candidaterad+minimum_radius_seeded<rectangle_width)
  condition[3] <- (candidatex-candidaterad-minimum_radius_seeded>origin_of_cartesian_axes)
  condition[4] <- (candidatey+candidaterad+minimum_radius_seeded<rectangle_height)
  condition[5] <- (candidatey-candidaterad-minimum_radius_seeded>origin_of_cartesian_axes)
  condition[6] <- (candidaterad+minimum_radius_seeded<=maximum_radius)
  
  is_it_acceptable <- condition[1]==1 && condition[2]==1 && condition[3]==1 && condition[4]==1 && condition[5]==1 && condition[6]==TRUE  

  if (is_it_acceptable==TRUE){
    new_centres_accepted <- new_centres_accepted + 1
    }else{
    centre<-(centre[-nrow(centre),]) 
    radius <- radius[1:(length(radius)-1)]
    }
  
  }

start_generating_new_centres <- 0 # 
particles_to_grow <- dim(centre)[1]-starting_particles_before_automated_loop

# Grow the new centres

iteration <- iteration+1
planar_void_ratio[iteration] <- 100 #Resetting this for the next calculation

while (planar_void_ratio[iteration] > target_planar_void_ratio && start_generating_new_centres==0){
  
  iteration <- iteration+1 
  
  for (k in (particles_to_grow+1):dim(centre)[1]){
    condition <- rep(13, 6)
    condition[1] <- sum(sqrt((centre[,1]-centre[k,1])^2+(centre[,2]-centre[k,2])^2)<(radius[k]+minimum_radius_seeded+radius))
    condition[2] <- (centre[k,1]+radius[k]+minimum_radius_seeded<rectangle_width)
    condition[3] <- (centre[k,1]-radius[k]-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[4] <- (centre[k,2]+radius[k]+minimum_radius_seeded<rectangle_height)
    condition[5] <- (centre[k,2]-radius[k]-minimum_radius_seeded>origin_of_cartesian_axes)
    condition[6] <- (radius[k]+minimum_radius_seeded<=maximum_radius)
    can_it_grow <- condition[1]==1 && condition[2]==1 && condition[3]==1 && condition[4]==1 && condition[5]==1 && condition[6]==TRUE  
    if (can_it_grow==TRUE){     
      radius[k] <- radius[k]+minimum_radius_seeded
    }
  }
  
  squared_sum_of_radii <- sum(radius[1:dim(centre)[1]]^2) 
  planar_void_ratio[iteration] <- (rectangle_width*rectangle_height-pi*squared_sum_of_radii)/(rectangle_width*rectangle_height)*100
  
  if (planar_void_ratio[iteration] < target_planar_void_ratio){ 
    start_generating_new_centres <- 0
    break
  }
  
  if(planar_void_ratio[iteration] == planar_void_ratio[iteration-1]){
    start_generating_new_centres <- 1
  }

  }

}

plot(c(origin_of_cartesian_axes, rectangle_width), c(origin_of_cartesian_axes, rectangle_height), type = "n", main="All the generations of circles we have grown", xlab="Width", ylab="Heigth", asp=1)

rect(origin_of_cartesian_axes, origin_of_cartesian_axes, origin_of_cartesian_axes+rectangle_width, origin_of_cartesian_axes+rectangle_height, col = "white", border="red")

library(plotrix)
for(q in 1:starting_particles_before_new_generation){
  draw.circle(centre[q,1], centre[q,2], radius[q], col="blue")
}
for(q in (starting_particles_before_new_generation+1):(starting_particles_before_automated_loop)){
  draw.circle(centre[q,1], centre[q,2], radius[q], col="green")
}
for(q in (starting_particles_before_automated_loop+1):(dim(centre)[1]-1)){
  draw.circle(centre[q,1], centre[q,2], radius[q], col="pink")
}

print(paste("We have created and grown", dim(centre)[1], "circles."))
print(paste("The first generation (blue) included", number_of_particles, "circles."))
print(paste("The second generation (green) included", (starting_particles_before_automated_loop-starting_particles_before_new_generation), "circles."))
print(paste("We have achieved a void % (i.e. the white space) of:", min(planar_void_ratio)))
