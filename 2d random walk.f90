* The following program is a simulation of 2-dimensional random walk
* of m number of walkers with n number of steps. In this program we calculate
* the average distance traveled by a walkers in the x and y direction on
* a 2-dimensional grid.
* The main skeleton of the code goes as follows we initialize the necessary 
* variables, calcualte the average distance for each step per walker and then
* store our results. 
     
       program randomwalk
       
       implicit none
       integer walks, steps, i, j
       real x2ave(50000), y2ave(50000), r2ave(50000), x(50000), y(50000)
       call initialize (x2ave, y2ave, walks, steps, x, y)
       call calculate (x2ave, y2ave, walks, steps, x, y, r2ave)
       call store (x2ave, y2ave, walks, steps, x, y, r2ave)
       stop
       end

* Here we initialize an array for average distance in x direction (x2ave) the 
* average distance in y direction (y2ave), the number of walkers, the number of
* steps taken by each walker and the starting point in x and y coordiantes.
* We also initialize that since the average distance moved in the x and y direction
* is zero for each step taken.   

       subroutine initialize (x2ave, y2ave, walks, steps, x, y)
       implicit none
       integer walks, steps, i, j 
       real x2ave(50000), y2ave(50000), x(50000), y(50000)
       print*, 'number of walkers'
       read*, walks
       print*, 'number of steps per walker'
       read*, steps
        do i=1, steps
         x2ave(i)=0
         y2ave(i)=0
        end do
*      x(1)=0
*      y(1)=0
       return
       end

* Here we loop through the number of walkers and the steps each walker takes based
* on the random number generated where the conditional statements determine the next position
* of the walker in the x or y direction.
* We say that if the random number, r, is less that 0.25 the walker moves forward in the
* x direction and if it is less than 0.5 then the walker moves back one. 
* Same condition goes for the y direction. 
* We then normalize the average postions for x and y for each walker.
* After normalization we calculate the average distance travelled by eaach walker (r2ave). 

       subroutine calculate (x2ave, y2ave, walks, steps, x, y, r2ave)
       implicit none
       integer walks, steps, i, j
       real x2ave(steps), y2ave(steps), r2ave(steps), x(steps), y(steps)
     &      , p
* looping through the walker
        do j=1, walks
         do i=1, steps
          p=rand()
           if (p.LT.0.25) then 
              x(i+1)=x(i)+1
            elseif(p.LT.0.5) then
              x(i+1)=x(i)-1
            elseif (p.LT.0.75) then
              y(i+1)=y(i)+1
            else
              y(i+1)=y(i)-1
           end if
          x2ave(i+1)=x2ave(i)+x(i+1)*x(i+1)
          y2ave(i+1)=x2ave(i)+y(i+1)*y(i+1)
         end do
        end do
* normalization
        do i=1, steps
         x2ave(i)=x2ave(i)/walks 
         y2ave(i)=y2ave(i)/walks     
         r2ave(i)=r2ave(i)+((x2ave(i))+(y2ave(i)))
        end do
       return
       end
       
* Here we store the results into a file.
       
       subroutine store (x2ave, y2ave, walks, steps, x, y, r2ave)
       implicit none
       integer walks, steps, i, j, p
       real x2ave(steps), y2ave(steps), r2ave(steps), x(steps), y(steps)
       open (1, file='2drandomwalk.dat')
        do i=1, steps
         write (1,*) r2ave(i)
        end do
       close (1)
       return 
       end
