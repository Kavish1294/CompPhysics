        program Pendulum
        dimension theta(5000), omega(5000), t(5000)
        real g,l
        call initialize(theta, omega, t, dt, n, g, l)
        call calculate(theta, omega, t, dt, n, g, l)
        call store(t, theta, omega, n)
        stop
        end

        subroutine initialize(theta, omega, t, dt, n, g, l)
        dimension theta(1), omega(1), t(1)
        real g,l
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

*euler-cromer method

        subroutine calculate(theta, omega, t, dt, n, g, l)
        real g,l
        dimension theta(n), omega(n), t(n)
         do i=1, n-1
          omega(i+1)=omega(i)-(g/l)*theta(i)*dt
          theta(i+1)=theta(i)+omega(i+1)*dt
          t(i+1)=t(i)+dt
         end do
        print*, omega(i)
        print*, theta(i)
        print*, t(i)
        return
        end


*euler method

*       subroutine calculate(theta, omega, t, dt, n, g, l)
*       real g,l
*       dimension theta(n), omega(n), t(n)
*        do i=1, n-1
*         omega(i+1)=omega(i)-(g/l)*theta(i)*dt
*         theta(i+1)=theta(i)+omega(i)*dt
*         t(i+1)=t(i)+dt
*        end do
*       return
*       end




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