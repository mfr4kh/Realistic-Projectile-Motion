program Realistic_projectile 

    implicit none 

    real:: m, c_d, r, rho, delta_t, t0, area, x0, v0, v1, angle 

    real:: x1, vx0, vx1, ax0, ax1, y0, y1, vy0, vy1, ay0, ay1 

    real, parameter:: pi=4*atan(1.0), g=9.81 

 

    !putting data 

    m = 0.05  !mass of object is 0.05 kg 

    c_d = 0.47 !Coefficient of air drag for sphare is 0.47 

    r = 0.01 !radius of object is 0.01 m 

    rho =1.225 !density of air is 1.225 kg/m^3 

 

    !initial value 

    v0 = 60  !initial velocity of the oject is 60 m/s 

    angle =30 !angle of projection is 30 degree 

    t0 =0.0  !initial time 

 

    angle = angle*pi/180 !converting angle angle into radian 

    delta_t = 0.001 !increment of time at each step 

    area = pi*r*r !area of object 

 

    !for x direction 

    x0 = 0.0

    vx0 = v0*cos(angle) 

    ax0 = -(0.5)*rho*v0*vx0*c_d*area/m 

 

    !for y direction 

    y0 = 0.0 

    vy0 = v0*sin(angle) 

    ay0 = (0.5)*rho*v0*vy0*c_d*area/m - g 

 

    do  

        !for x direction 

        v1 = sqrt(vx1*vx1 + vy1*vy1) 

        vx1 = vx0 + ax0*delta_t 

        x1 = x0 + vx0*delta_t+(0.5)*ax0*delta_t*delta_t 

        ax1 = -(0.5)*rho*v1*vx1*c_d*area/m 

 

        !for y direction 

        vy1 = vy0 + ay0*delta_t 

        y1 = y0 + vy0*delta_t+(0.5)*ay0*delta_t*delta_t 

        ay1 = (0.5)*rho*v1*vy1*c_d*area/m -g 

        v1 = sqrt(vx1*vx1 + vy1*vy1) 
 

        if(y1 .lt. 0.0) exit 

 
        open(15, file = '30 deg') 

        write(15,*) x0 , y0 

         

        !updating values 

        t0 = t0+ delta_t 

        vx0 = vx1 

        x0 = x1 

        ax0 = ax1 

 

        vy0 = vy1 

        y0 = y1 

        ay0 = ay1    

    end do   

end program Realistic_projectile 