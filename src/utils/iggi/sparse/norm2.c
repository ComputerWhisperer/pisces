#include <math.h>

double norm2 (nv, v)
    int nv;
    double *v;
{
    double sum, *t;
    for (t = v+1; t <= v+nv; t++)
	sum += *t * *t;
    return (sqrt (sum));
}
	
