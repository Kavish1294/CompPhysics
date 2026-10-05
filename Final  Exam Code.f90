* The following program is a simulation of population growth and decline
* for a certain species and bacteria. This program solves the Logistics Equation
* using the Euler Method. The Logistics Equation is a model to determine the
* rate of change of popualtion of a certain species based on their initial
* population and the species growth and decline constants.


       program populationgrowth

* Here we assign memory slots to our varibales so they can used to perform
* the desired calcualtion

       dimension P(1000000),t(1000000)
       call initialize(P, a, b, dt, t, n, method)
       call calculate(P, a, b, dt, t, n, method)
       call store(P, t, n)
       stop
       end

* Here we assign an initial value for P(Initial Number of Population,
* a(growth term constant), b (decline term constant), dt (time steps in year for
* method 1 & 2 and seconds for method 3)and t (total time in years for method 1 and 2
* and seconds for method 3). These values need to be entered by the user when
* executing the output file.
* We also allow the user to select the method they desire to calculate the number
* of popualtion for different species and for different a & b constants.

       subroutine initialize(P, a, b, dt, t, n, method)
       dimension P(1), t(1)
       print*, 'Normal case(1), Abundant resources(2) or Bacteria(3)'
       read*, method
         if(method.ne.1.and.method.ne.2.and.method.ne.3) then
          print *,'must select 1 or 2 or 3'
          stop
         endif
       print*, 'initial number of population'
       read*, P(1)
       print*, 'growth term constant'
       read*, a
       print*, 'death term constant'
       read*, b
       print*, 'time change'
       read*, dt
       print*, 'total time'
       read*, time
       t(1)=0
       n=min(int(time/dt),200000)
       return
       end

* Calcualtion of population curves was done by utilising Euler's method because given
* the value of initial population at one particualar value of t, we can estimate it's value at later
* times and treat our differential equation as an initial value  problem.
* We can ignore the higher powers of our differential equation since out dt is very small.
* The equation is an approximation therefore the error terms that are dropped are atleast of
* second order and by making dt very small they should be negligible.
* Runge-Kutta Method can provide a better approximation to the exact solution.
* Essentially for each timestep i, starting with i=1, we are calcualting population and t at step i+1.
* Equations used for different scenarios are:
* For normal conditions
* P(i+1)=P(i)+(a*P(i)-b*(P(i)**2.0))*dt
* t(i+1)=t(i)+dt
* For abundant resources condition, the power of P tern was changed to 1.5 to better represent
* the conditions because since there are a lot more resources, very few deaths will occur.
* P(i+1)=P(i)+(a*P(i)-b*(P(i)**1.5))*dt
* t(i+1)=t(i)+dt
* For bacterial conditions
* P(i+1)=P(i)+((a*P(i))-b*(P(i)**2.0))*dt
* t(i+1)=t(i)+dt

       subroutine calculate(P, a, b, dt, t, n, method)
       dimension P(n), t(n)
        do i=1, n-1
          if(method.eq.1) then
           P(i+1)=P(i)+((a*P(i))*dt)-((b*(P(i)**2.0))*dt)
           t(i+1)=t(i)+dt
          else if(method.eq.2) then
           P(i+1)=P(i)+(a*P(i)-b*(P(i)**1.5))*dt
           t(i+1)=t(i)+dt
          else if(method.eq.3) then
           P(i+1)=P(i)+((a*P(i))-b*(P(i)**2.0))*dt
           t(i+1)=t(i)+dt
          end if
        end do
       return
       end

* The final subroutine stores the results into a file.

       subroutine store(P, t, n)
       dimension P(n), t(n)
       open(1,file='populationgrowth.dat')
        do i=1,n
         write(1,*) t(i), P(i)
        end do
       close(1)
       return
       end

