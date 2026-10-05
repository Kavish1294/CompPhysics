* The following program is a simulation of radioactive  decay
* of Uranium  particles over time.
* The main skeleton of the code goes as follows: We declare the
* ncessary variables and arrays needed for the calcualtion, initialize
* our variables, perform the calcualtion and store our results.
* The line double precision n_uranium(100), t(100) declares that two
* arrays will be used to store n_uranium and t.

       program decay
       double precision n_uranium(100),t(100)
       call initialize(n_uranium, t, tau, dt, n)
       call calculate(n_uranium, t, tau, dt, n)
       call store(n_uranium, t, n)
       stop
       end

* Here we assign an initial value for n_uranium(initial number of nuclei,
* tau(time constant), dt (time steps) and t (total time). These values
* need to be entered by the user when executing the output file.

       subroutine initialize(n_uranium, t, tau, dt, n)
       double precision n_uranium(1),t(1)
       print*, 'initial number of nuclei'
       read*, n_uranium(1)
       print*, 'time constant'
       read*, tau
       print*, 'time change'
       read*, dt
       print*, 'total time'
       read*, time
       t(1)=0
       n=min(int(time/dt),100)
       return
       end

* Calcualtion of nuclear decay was done by utilising
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
* n_uranium and t at step i+1.
* Two equations used to do this are for n-1 steps:
* n_uranium(i+1)=n_uranium(i)-(n_uranium(i)/tau)*dt
* t(i+1)=t(i)+dt


       subroutine calculate(n_uranium, t, tau, dt, n)
       double precision n_uranium(n), t(n)
        do i=1, n-1
         n_uranium(i+1)=n_uranium(i)-(n_uranium(i)/tau)*dt
         t(i+1)=t(i)+dt
        end do
       return
       end

* The final subroutine stores the results into a file.

       subroutine store(n_uranium, t, n)
       double precision n_uranium(n), t(n)
       open(1,file='decay.dat')
        do i=1,n
         write(1,20) t(i), n_uranium(i)
        end do
       close(1)
20     format(1x,1p,2(e12.5,2x))
       return
       end
