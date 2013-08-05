### Conway's Game of Life
### http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
###
### To run the simulation:
### 1. type source("life.r") in R
### 2. click on the plot a few times to create life
### 3. click "Finish" in the cornet of the plot window
### 4. watch as life unfolds.
###
### Note that the grid size and number of iterations can be set at the
### end of the file.
### To test out changes, set the iterations to 1.

create.grid <- function(size) {
  ## Create the environment.
  
  return(list(grid = matrix(0,size,size), size = size))
}

wrap.coordinates <- function(grid, pos) {
  ## Wrap coordinate to cycle within grid.
  
  x.pos = pos$x %% grid$size
  y.pos = pos$y %% grid$size
  if (x.pos == 0) {
    x.pos = grid$size
  }
  if (y.pos == 0) {
    y.pos = grid$size
  }
  return(list(x = x.pos, y = y.pos))
  
}

total.life <- function(grid) {
  return(sum(grid$grid))
}

add <- function(grid, pos) {
  ## Add a point to the grid.
  
  pos = wrap.coordinates(grid, pos)
  grid$grid[pos$x,pos$y] = 1

  return(grid)
}

keep <- function(grid, pos) {
  ## Keep a point (do nothing).
  
  return(grid)
}

remove <- function(grid, pos) {
  # Remove a point from the grid.
  
  pos = wrap.coordinates(grid, pos)
  grid$grid[pos$x,pos$y] = 0

  return(grid)
}

inspect <- function(grid, pos) {
  ## Show the state of a point on the grid.

  return(grid$grid[pos$x,pos$y])
}

show.grid <- function(grid) {
  ## Plot the grid.
  
  image(1:grid$size,1:grid$size,grid$grid)
}


count.neighbours <- function(grid, pos) {
  ## Count the number of living neighbours of a point.
  
  counter = 0
  x = pos$x
  y = pos$y
  
  for (x in pos$x + c(-1,0,1)) {
    for (y in pos$y + c(-1,0,1)) {
      wrap.pos = wrap.coordinates(grid, list(x = x, y = y))
      if (inspect(grid,wrap.pos) == 1) {
        counter = counter + 1
      }
    }
  }
  wrap.pos = wrap.coordinates(grid, pos)
  counter = counter - inspect(grid,wrap.pos)
  return(counter)
}

grow <- function(grid, pos) {
  ## Compute the next state of a point.

  neighbours = count.neighbours(grid, pos)
  
  if (neighbours == 3) {
    return(add)
  }
  else if (neighbours == 2) {
    return(keep)
  }
  else {
    return(remove)
  }
}


time.step <- function(grid) {
  ## Compute the next state of the grid.
  
  next.grid <- grid

  for (x in 1:grid$size) {
    for (y in 1:grid$size) {

      position = list(x = x, y = y)
      change = grow(grid, position)
      next.grid = change(next.grid, position)

    }
  }
  return(next.grid)
}

click.points <- function(grid) {
  ## Allow the user to click to insert life into the grid.

  show.grid(grid)
  cat("Click on the plot to allocate life. When finished, click Finish at the top right corner of the plot.\n")
  repeat {
    click.loc <- locator(1)
    if (!is.null(click.loc)) {
      pos = list(x = round(click.loc$x), y = round(click.loc$y))
      wrap.pos = wrap.coordinates(grid, pos)
      if ((pos$x == wrap.pos$x) & (pos$y == wrap.pos$y)) {
        grid = add(grid, pos)
        show.grid(grid)
      } else {
        break
      }
    } else {
      break
    }
  }
  return(grid)
}

run.life <- function(grid, iterations) {
  ## main life iterator

  ## step though time
  for (a in 1:iterations) {

    cat("\rIteration: ", a)
    
    ## compute the state of life for the next time step
    grid = time.step(grid)
    Sys.sleep(0)
    ## show the state of life
    show.grid(grid)
    if (total.life(grid) == 0) {
      ## if there is no life left, stop the simulation
      break
    }
  }
  cat("\n")
}



## set up varaibles
grid.size = 20
iterations = 50

## create grid
grid <- create.grid(grid.size)

## allocate life
grid = add(grid, list(x = 4, y = 6))
grid = add(grid, list(x = 5, y = 6))
grid = add(grid, list(x = 6, y = 6))
grid = add(grid, list(x = 6, y = 7))
grid = add(grid, list(x = 5, y = 8))

## or manually allocate life
grid = click.points(grid)

## run the simulation
run.life(grid, iterations)



