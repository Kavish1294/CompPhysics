* The following program is a simulation of a simple harmonic pendulum using
* Euler and Euler-Cromer methods to determine their effects on the oscillations
* of the pendulum.

* The main skeleton of the code goes as follows: We declare the
* necessary variables and arrays needed for the calcualtion, initialize
* our variables, perform the calcualtion and store our results.
* The line dimension theta(5000), omega(5000) and t(5000)  declares that
* arrays will be used to store the displacement angle, angular speed and time.
* We use an if-else loop to allow the user to decide which method he or she
* wants to use to calculate the oscillations of the pendulum

        program Pendulum
        dimension theta(5000), omega(5000), t(5000)
        real g,l
        call initialize(theta, omega, t, dt, n, g, l, method)
        call calculate(theta, omega, t, dt, n, g, l, method)
        call store(t, theta, omega, n)
        stop
        end

* Here we initialize our varibales. We have selected gravity(g) and length of pendulum(l)
* to be real variables. Gravity is set to be 9.8 m/s^2 and length of pendulum is 1 m(length
* of pendulum has to be quite long or our equations of motion will fail, hence it is already
* selected as a constant). We then require the user to select which method they would
* like to use to calulate the oscillations, Euler or Euler-Cromer. We then require the user to
* enter the initial angle in radians, initial angular velocity in radians/sec, time in seconds
* and change in time in seconds.
* Note:- It is better to choose a small angle theta<4 degrees or else at large angles our
* equations will collapse into non-linear regimes. If the angle is selected to be the maximum
* displacement then the angular velocity will be zero. We would recommend this initialization
* because if the displacement angle is picked to be zero one would have to caluculate the
* maximum angular velocity at that point.

        subroutine initialize(theta, omega, t, dt, n, g, l, method)
        dimension theta(1), omega(1), t(1)
        real g,l
        print*, 'Euler(1), Euler-Cromer(2)'
        read*, method
         if(method.ne.1.and.method.ne.2) then
          print*,'must select 1 or 2'
          stop
         endif
        print*, 'initial angle in radians'
        read*, theta
        print*, 'angular velocity in rad/s'
        read*, omega
        print*, 'max time in seconds'
        read*, time
        t(1)=0
        print*, 'time step in seconds'
        read*, dt
        n=min(int(time/dt), 5000)
        g=9.8
        l=1
        print*, time
        print*, dt
        print*, n
        return
        end

* Calcualtion of angular displacement and angular velocity was done by utilising Euler and Euler-Cromer methods
* because given the one particualar value of t, we can estimate it's value at later times and treat our
* differential equation as an initial value  problem. We can ignore the higher powers of our differential equation since
* our dt is very small. The equation is an approximation therefore the error terms that are dropped
* are atleast of second order and by making dt very small they should be negligible. The difference with the
* standard Euler method is that the Euler-Cromer method uses w(n+1) in the equation for, while the Euler method uses v(n).
* As a consequence, the  Euler-Cromer method almost conserves the energy. Often, the energy increases steadily
* when the standard Euler method is applied, making it far less accurate.
* Runge-Kutta Method can provide a better approximation to the exact solution.
* Essentially for each timestep i, starting with i=1, we are calcualting the angular displacement and the angular velocity
* of the pendulum at t at step i+1.

        subroutine calculate(theta, omega, t, dt, n, g, l, method)
        real g,l
        dimension theta(n), omega(n), t(n)
         do i=1, n-1
          if(method.eq.1) then
           omega(i+1)=omega(i)-(g/l)*theta(i)*dt
           theta(i+1)=theta(i)+omega(i)*dt
           t(i+1)=t(i)+dt
          else if(method.eq.2) then
           omega(i+1)=omega(i)-(g/l)*theta(i)*dt
           theta(i+1)=theta(i)+omega(i+1)*dt
           t(i+1)=t(i)+dt
          end if
         end do
        print*, omega(i)
        print*, theta(i)
        print*, t(i)
        return
        end

* This subroutine stores the results in a file

        subroutine store(t, theta, omega, n)
        dimension t(n), theta(n), omega(n)
        open (1,file='pendulum.dat')
         do i=1,n
          write(1,20) t(i), theta(i), omega(i)
         end do
        close(1)
20      format(1x,1p,2(e12.5,2x))
        return
        end
