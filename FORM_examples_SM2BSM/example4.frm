#-
Off Statistics;

Index mu1,mu2,mu3,mu4,mu5;
Vector p1,p2,p3,p4;
Local F1 = g_(1,mu1)*g_(1,mu2)*g_(1,mu3)*g_(1,mu4);
Local F2 = g_(2,p1)*g_(2,p2)*g_(2,p3)*g_(2,p4);

Tracen,1;
Trace4 2;

Print;
.end
