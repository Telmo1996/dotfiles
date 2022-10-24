##TGR2 Diseño de lenguajes de programación
#####01/10/2019
###Ejemplo

int factorial(int n){  
    //{n>0}
    //{1=1! and 1<=n}
    int f = 1  
    //{f=1! and 1<=n}
    int i = 1
    //f=i! and i<n

    while(i<n){  
      //f=i! and (i=<n) and i<n
      //f*(i+1) = (i+1) and i+1<=n  
        i=i+1
      //f*i = i! and i<=n
        f=f*i
    } 

    //f=i! and (i>=n) and i<n
    //{f = n!}  
    return f
}

	i 	f 
	1	1	1!
	2	2 	2!
	3	6	3! 	La invariante se convierte en f=i!
	4	24	4!
	5	120	5! 

Ejemplo de fibonacci:

int fibonacci (int i){
	//{0=fib(0) and 1 =Fib(1) and 0<=n}={0<=n}
	int fib0 = 0
	//{fib0 =Fib(0) and 1 = Fib(1) and 0<=n}
	int fib1 = 1

	//{fib0 = Fib(n-m) and fib1 = Fib(n-n+1) and 0<=n<=n}
	//={fib0 = Fib(0) and fib1=Fib(1) and 0<=n}
	int k = n
	//Inv
	while(k>0){
		//{Inv and (k>0)}
		//{fib1 = Fib(n-k+1) and fib1+fib0=Fib(n-k+2) and (0<=k-1<=n)}
		int temp = fib0
		//{fib1 = Fib(n-k+1) and fib1+temp =Fib(n-k+2) and 0<=k-1<=n}
		fib0 = fib1
		//{fib0=Fib(n-k+1) and fib0 + temp = Fib(n-k+2) and 0<=k-1<=n}
		fib1=fib0+temp
		//{fib0=Fib(n-k+1) and fib1 = Fib(n-k+2) and 0<=k+1<=n
		k=k-1}
		//{Inv}
	}//Inv and k<=0
	//fib0 = Fib(n)
	return fib0
}

	k 	fib0	fib1
	n	0		1
	----------------
	n-1	1		0+1
	n-2	1		1+1			x = n-k
	n-3	2		2+1
	n-4	3		3+2
	n-5	5		5+3

	Inv |fib0 = Fib(n-k)	fib1 = Fib(n-k+1)
		|		0<=n<=n
