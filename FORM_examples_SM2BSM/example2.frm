#-
Off Statistics;

Symbol x,y,z,a,b;

CFunction f, g, h;

Local F = f(x)+f(y)+f(4)+f(a+b);
Local G = g(2, x^2 +2*y +z);
Local H = h(x,y,z,a)*h(x,z,y,b,a,x,y);

id f(z?) = z^2;
id g(x?,y?) = x*f(y);

repeat;
id h(x?,y?,?a) = h(x)*h(y,?a);
endrepeat;

Print;
.end
