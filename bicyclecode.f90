* The following program is a simulation of velocity with respect to time
* of a bicyclist experiencing air resistence.
* The main skeleton of the code goes as follows: We declare the
* ncessary variabs and arrays needed for the calcualtion, initialize
* our variables, perform the calcualtion and store our results.
* The line double precision t(5003), velocity(5003) declares that two
* arrays will be used to store velocity  and time.

        program Bicycle
* Declare the arrays we will need
        dimension t(5003), velocity(5003)
        real mass
        call initialize(t, velocity, p, dt, n, mass, c, area, rho)
        call calculate(t, velocity, p, dt, n, mass, c, area, rho)
        call store(t, velocity, n)
        stop
        end

* Here we assign an initial value for initial velocity,
* p(power), dt (time steps), t (total time),mass of bicylist and the bicycle,
* c (drag coefficient), area (cross sectional area of cyclist)
* and rho (density of the medium from which the cyclist is going through).
* These values need to be entered by the user when executing the output file.

       subroutine initialize(t, velocity, p, dt, n, mass, c, area, rho)
       dimension t(1), velocity(1)
       real mass
       print*,'max time'
       read*, time
       t(1) = 0
       print*,'constant power'
       read*, power
       print*,'time step'
       read*, dt
       print*,'initial velocity'
       read*, velocity(1)
       n=min(int(tmax/dt), 5003)
       mass = 70
       c = 0.5
       area = 0.33
       rho = 1.225
       return
       end


* Calcualtion of velocity was done by utilising
* Euler's method because given the value of n_uranium at
* one particualar value of t, we can estimate it's
* value at later times and treat our differential equation
* as an initial value  problem. We can ignore the higher powers
* of our differential equation since out dt is very small. The equation
* is an approximation therefore the error terms that are dropped
* are atleast of second order and by making dt very small
* they should be negligible. Runge-Kutta Method can provide a better
* approximation to the exact solution.
* Essentially for each timestep i, starting with i=1, we are calcualting
* the final velocity of the cyclist and t at step i+1.
* Two equations used to do this are for n-1 steps:
* v(i+1)=v(i)+(p/(mass*v(i)))*dt-0.5*((c*rho*area*v(i)**2)/mass)*dt
* t(i+1)=t(i)+dt

       subroutine calculate(t, v, p, dt, n, mass, c, area, rho)
       real mass
       dimension t(n), v(n)
        do i=1, n-1
         v(i+1)=v(i)+(p/(mass*v(i)))*dt-0.5*
     &          ((c*rho*area*v(i)**2)/mass)*dt
         t(i+1)=t(i)+dt
       end do
       return
       end

* The final subroutine stores the results into a file.

      subroutine store(t, velocity, n)
       dimension t(n), velocity(n)
       open(1,file='bicycle.dat')
        do i=1,n
         write(1,20) t(i), velocity(i)
        end do
       close(1)
20     format(1x,1p,2(e12.5,2x))
       return
       end
