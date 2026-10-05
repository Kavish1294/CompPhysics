* The following program is a simulation of a planetary motion using
* Euler-Cromer method to determine the orbit of planet earth around
* the sun.

* The main skeleton of the code goes as follows: We declare the
* necessary variables and arrays needed for the calcualtion, initialize
* our variables, perform the calcualtion and store our results.
* The line dimension r(5000), x(5000), y(5000), v_x(5000), v_y(5000) and t(5000)
* declares that arrays will be used to store the orbital radius, horizontal position
* vertical position, horizontal velocity, vertical velocity and time.

        program Planetary Motion
        dimension r(5000), x(5000), y(5000), v_x(5000), v_y(5000),
     $            t(5000)
        real pi, beta, r
        call initialize(r, x, y, v_x, v_y, t, dt, beta, pi, n)
        call calculate(r, x, y, v_x, v_y, t, dt, beta, pi, n)
        call store(t, x, y, n, r)
        stop
        end


* Here we initialize our varibales. We have selected pi, radius of orbit (r) and exponential constant
* beta to be real parameters. We then ask the user to enter the initial values of the horizontal and
* vertical componenets of position of the planet in the orbit in (AU) and the horizontal and vertical
* componenets of velocity of the planet in the orbit in (AU/yr). Since the orbit has a radius of 1
* the user has to be care full when to initalize the positions since the square of positions components'
* sum and root must equal to 1. We recommend to pick the position and velocity values of the planet at the
* axis. The beta allows the user to select the exponent law and see its effects on the planetary orbits.
* Then the user must provide the time in yr and time step in year as well. The rule of thumb is that the
* time step is .01% of the total time to give a good approximation.

        subroutine initialize(r, x, y, v_x, v_y, t, dt, beta, pi, n)
        dimension r(1), x(1), y(1), v_x(1), v_y(1), t(1)
        real pi, beta, r
        print*, 'horizontal component of position(x) in AU'
        read*, x(1)
        print*, 'Vertical component position(y) in AU'
        read*, y(1)
        print*, 'horizontal component of velocity(v_x) in AU/yr'
        read*, v_x(1)
        print*, 'Vertical component of velocity(v_y) in AU/yr'
        read*, v_y(1)
        r(1)=sqrt((x(1)*x(1))+(y(1)*y(1)))
        

*       mu=((M_s*M_p)/(M_s+M_p))
*       L=sqrt((mu*G*M_s*M_p*a*(1-(e*e))))


        print*, 'Power law exponent beta'
        read*, beta
        print*, 'total time in yr'
        read*, time
        print*, 'time step in yr'
        read*, dt
        n=min(int(time/dt), 5000)
        pi=3.14
*       print*, time
*       print*, dt
*       print*, n
*       print*, r
        return
        end

* Calcualtion of horizontal and vertical position was done by utilising the Euler-Cromer method based on the planets
* angular velocity because given the one particualar value of v, we can estimate it's value at later times and treat our
* differential equation as an initial value  problem. The Euler-Cromer method uses v(n+1) in the equation for positions.
* As a consequence, the  Euler-Cromer method almost conserves the energy as well as the angular momentum.
* Runge-Kutta Method can provide a better approximation to the exact solution.
* Essentially for each timestep i, starting with i=1, we are calcualting the postions and the angular velocity
* of the planet at t at step i+1. We use the inital values of postions to initialize r and use r at i to find the
* angualr velocity of the planet at the timestep with the inital angular velocities.
* The equations utilized for this calculation are:
* r(i)=sqrt((x(i)**2)+(y(i)**2))
* v_x(i+1)=v_x(i)-((4*(pi**2)*x(i))/(r(i)**(beta)))*dt
* v_y(i+1)=v_y(i)-((4*(pi**2)*y(i))/(r(i)**(beta)))*dt
* x(i+1)=x(i)+v_x(i+1)*dt
* y(i+1)=y(i)+v_y(i+1)*dt
* t(i+1)=t(i)+dt

        subroutine calculate(r, x, y, v_x, v_y, t, dt, beta, pi, n)
        real pi, beta, r
        dimension r(n), x(n), y(n), v_x(n), v_y(n), t(n)
         do i=1, n-1
          r(i)=sqrt((x(i)*x(i))+(y(i)*y(i)))


*	  r(i)=((L*L)/(mu*G*M_s*M_p))(1/(1-ecos(theta)))

          v_x(i+1)=v_x(i)-((4*(pi*pi)*x(i)*dt)/(r(i)**(beta)))
*         print*, v_x(i)

          V_max(i+1)=sqrt(G*M_s)*sqrt(((1+e)/(a(1-e)))*(1+(M_p/M_s)))
          v_min(i+1)=sqrt(G*M_s)*sqrt(((1-e)/(a(1+e)))*(1+(M_p/M_s)))

          v_y(i+1)=v_y(i)-((4*(pi*pi)*y(i)*dt)/(r(i)**(beta)))
*         print*, v_y(i)


          x(i+1)=x(i)+v_x(i+1)*dt
*         print*, x(i)
          y(i+1)=y(i)+v_y(i+1)*dt
*         print*, y(i)
          t(i+1)=t(i)+dt
         end do
        return
        end

*The final subroutine stores the value into a file

        subroutine store(t, x, y, n, r)
        dimension t(n), x(n), y(n), r(n)
        open (1,file='planetarymotion.dat')
         do i=1,n
          write(1,20) x(i), y(i)
         end do
        close(1)
20      format(1x,1p,2(e12.5,2x))
        return
        end
