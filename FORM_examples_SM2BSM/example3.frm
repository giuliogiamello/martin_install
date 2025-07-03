#-
Off Statistics;

Symbol D;
Index i1=D,i2=D,i3=D;
Index n1=3,n2=3,n3=3,n4=3,n5=3,n6=3;
Vector p1,p2,p3,p4;

Local F = p1(i1)*(p2(i1)+p3(i3))*(p1(i2)+p2(i3));
Local G = d_(i1,i2) * d_(i2,i1);
Local H1 = e_(n1,n2,n3)*e_(n3,n4,n5);
Local H2 = e_(n1,n2,n3)*e_(n2,n3,n4);

Contract;

Print;
.end
