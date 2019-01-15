#include <cstdio>
#include <cstdlib>

int main(){
    
  const int n=100000;
  double a[n], b[n], redux;

  for (int i = 0; i < n; i++)
    {
      a[i]= (double) i + 315.1415;
      b[i]= (double)1/(1+100000*i);
    }

  redux = 0.0;

  for(int i = 0; i < n; i++)
    redux += a[i]+b[i]; 

  printf("%lf\n",redux);
  
}
