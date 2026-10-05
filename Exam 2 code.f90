* The following program is a simulation of velocity with respect to time
* of a bicyclist experiencing resistence.
* The main skeleton of the code goes as follows: We declare the
* necessary variables and arrays needed for the calcualtion, initialize
* our variables, perform the calcualtion and store our results.
* The line double precision t(5000), velocity(5000) declares that two
* arrays will be used to store velocity  and time.

        program Bicycle
* Declare the arrays we will need
        dimension t(500000), velocity(500000)
        real mass
        call initialize(t, velocity, F, dt, n, mass, c, area, rho, eta, h)
        call calculate(t, velocity, F, dt, n, mass, c, area, rho, eta, h)
        call store(t, velocity, n)
        stop
        end

* Here we assign an initial value for initial velocity in m/s,
* Force(F) in Newtons, time steps(dt) in seconds, total time(t) in seconds,
* mass of bicylist and the bicycle in kg,
* drag coefficient(c), cross sectional area of cyclist in m^2,
* density of the medium from which the cyclist is going through(rho) in kg/m^3,
* viscosity of medium (eta) in Pa*s, and height of cyclist and cycle(h) im m.
* Density of air= 1.225 kg/m^3 and Density of water vapor =0.804 kg/m^3
* For the stokes term, eta is the viscous drag ,viscous drag for air is 2e-5 Pa*s and
* for water is 1.3e-5 Pa*s, the height was picked to be 2m.
* These values need to be entered by the user when executing the output file.

       subroutine initialize(t, velocity, F, dt, n, mass, c, area, rho,
     &                       eta, h)
       dimension t(1), velocity(1)
       real mass
       print*,'max time in seconds'
       read*, time
       t(1) = 0
       print*,'constant force in Newtons'
       read*, F
       print*,'time step in seconds'
       read*, dt
       print*,'initial velocity in m/s'
       read*, velocity(1)
       n=min(int(time/dt), 500000)
       print*, 'density of medium (rho) in kg/m^3'
       read*, rho
       print*, 'viscosity of the medium(eta) in Pa*s'
       read*, eta
       mass = 70
       c = 0.5
       area = 0.33
       h=2
       return
       end


* Calcualtion of velocity was done by utilising
* Euler's method because given the initial value of velocity at
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
* v(i+1)=v(i)+(F/(mass))*dt-0.5*((c*rho*area*v(i)*v(i))/mass)*dt
* t(i+1)=t(i)+dt
*
* For the cyclist on a foggy road we modified the equation to add a viscosity
* drag term also called the stokes term to give a better approximation to
* the terminal velocity of the cyclist. We initialized the viscous drag(eta) as well as
* the height of the cyclist and the cycle since the viscous drag of the medium will
* be effecting the cyclist's velocity. Essentially the viscous drag is how easy it is
* for the cyclist to "slice" through the medium.
* The modified equation for the viscosity term is:
* v(i+1)=v(i)+(F/(mass))*dt-0.5*((c*rho*area*v(i)*v(i))/mass)*dt-(eta*area*(v(i)/(mass*h))*dt
* This equation can also be used to find the terminal velocity for the cyclist is air.

       subroutine calculate(t, v, F, dt, n, mass, c, area, rho,
     &                      eta, h)
       real mass
       dimension t(n), v(n)
        do i=1, n-1
         v(i+1)=v(i)+(F/(mass))*dt
     &          -((c*rho*area*v(i)*v(i))/(2*mass))*dt
     &          -(eta*area*(v(i)/(mass*h)))*dt
         t(i+1)=t(i)+dt
       end do
*      print*, v(i)
*      print*, t(i)
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
20     format(1x,1p,2(e12.3,2x))
       return
       end
