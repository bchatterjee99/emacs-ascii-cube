#include <stdio.h>
#include <math.h>

void cross_product(double* a, double* b, double* c)
{
    c[0] = a[1]*b[2] - a[2]*b[1];
    c[1] = a[2]*b[0] - a[0]*b[2];
    c[2] = a[0]*b[1] - a[1]*b[0];
    double len = sqrt(c[0] * c[0] + c[1] * c[1] + c[2] * c[2]);
    c[0] = c[0] / len;
    c[1] = c[1] / len;
    c[2] = c[2] / len;
}

void normal(double* a, double* b, double* c, double* d)
{
  double vec1[3];
  double vec2[3];

  vec1[0] = b[0] - a[0];
  vec1[1] = b[1] - a[1];
  vec1[2] = b[2] - a[2];

  vec2[0] = c[0] - a[0];
  vec2[1] = c[1] - a[1];
  vec2[2] = c[2] - a[2];

  printf("vec1 = %lf,%lf,%lf\n", vec1[0], vec1[1], vec1[2]);
  printf("vec2 = %lf,%lf,%lf\n", vec2[0], vec2[1], vec2[2]);

  cross_product(vec1, vec2, d);
}

int main()
{
    double a[] = {1, 2, 3};
    double b[] = {7, 9, 8};
    double c[] = {6, 7, 6};
    double d[3];

    normal(a, b, c, d);

    printf("d = %lf, %lf, %lf\n\n", d[0], d[1], d[2]);

    return 0;
}
