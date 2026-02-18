/*-------------------------------
  File: functions_example.c
-------------------------------*/
#include <stdio.h>

/* Function prototype */
int square(int x);

int main() {
    int n = 7;
    int result;

    /* Call the function */
    result = square(n);

    printf("The square of %d is %d\n", n, result);

    return 0;
}

/* Function to compute square */
int square(int x) {
    return x * x;
}
