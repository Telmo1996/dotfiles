//{n>=0}
int factorial(int n){
	int f = 1
	int i = 1

	//{I}
	while(i<n){
		//{I and i<n}
		i=i+1
		f=f*i
	}
	//{I and i>=n}

	//{f=n!}
	return f
}
