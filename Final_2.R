DATA2_177

A <- DATA2_177$A
hist(A)
t.test(A,mu= 1100 , alt="less", conf= 0.99)

B <- DATA2_177$B
hist(B)
t.test(B,mu=1200, alt="less", conf= 0.99)

C <- DATA2_177$C
hist(C)
t.test(C,mu=1400, alt="less", conf= 0.99)

D <- DATA2_177$D
hist(D)
t.test(D,mu=2100, alt="less", conf= 0.99)

E <- DATA2_177$E
hist(E)
t.test(E,mu=2300, alt="less", conf= 0.99)