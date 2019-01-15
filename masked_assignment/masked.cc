#include <cstdio>
#include <cstdlib>

int main(){
    
  const int n=10000;
  double a[n], b[n], c[n];
  int counter = 0;

  for (int i = 0; i < n; i++)
    {
      a[i]= (double) i + 2.0;
      b[i]= (double)i;
    }

  for(int i = 0; i < n; i++)
    {
      if(a[i] >= b[i])
	c[i] = a[i];
      else
	c[i] = (double)0.0;
    }

  for(int i = 0; i < n; i++)
    {
      if(c[i] >= 0.0)
	counter++;
    }
  printf("%d\n",counter);
  
}
