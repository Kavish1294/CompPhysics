* The following program is a psuedo random number generator which
* computes the remainder of the division of two numbers. The skeleton
* of the code goes as follows: we initialize the numbers we want to
* compute the remainder of, calculate the raminader using the initial
* values and then the program gives out a set of random numbers.

       Program Psuedo Random Number Generator

* Here we establish the number of arrays needed to perform the calcualtion
* for this code.

       dimension r(5000)
       Call initialize (r, a, c, M)
       Call calculate  (r, a, c, M)
       stop
       end

* In this subroutine we initialize our a, c, M and give out program an initial
* random value as a seed value to start up the calcualtion.

       Subroutine initialize (r, a, c, M)
       dimension r(1)
       print*, 'Enter a seed number'
       read*, r(1)
       print*, 'a'
       read*, a
       print*, 'c'
       read*, c
       print*, 'M'
       read*, M
       return
       end

* The calcualte subroutine is where we determine the remainder of ar(i)+c and M
* using the mod command which means to find the remainder of some A divided by P value.
* We then print the random numbers to obtain a set of random values.

       Subroutine calculate (r, a, c, M)
       dimension r(M)
        do i=2,M
         r(i)=mod(a*(r(i-1))+c,M)
         print*, r(i)
        end do
       return
       end
