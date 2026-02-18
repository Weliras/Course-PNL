#include <stdio.h>
#include <stdbool.h>
#include <complex.h>
#include <math.h>

int main() {
    // --- Arithmetic operations (integer and real) ---
    int a_int = 7, b_int = 2;
    int sum = a_int + b_int;
    int diff = a_int - b_int;
    int prod = a_int * b_int;
    int int_div = a_int / b_int;       // Integer division

    double x_real = 3.0, y_real = 2.0;
    double exp_result = pow(x_real, y_real); // Exponentiation

    printf("Sum: %d\n", sum);
    printf("Difference: %d\n", diff);
    printf("Product: %d\n", prod);
    printf("Integer Division: %d\n", int_div);
    printf("Exponentiation: %.2f\n", exp_result);

    // --- Relational operations ---
    int m = 5, n = 3;
    bool rel_result;

    rel_result = (m == n);
    printf("m == n: %s\n", rel_result ? "true" : "false");
    rel_result = (m != n);
    printf("m != n: %s\n", rel_result ? "true" : "false");
    rel_result = (m > n);
    printf("m > n: %s\n", rel_result ? "true" : "false");

    // --- Logical operations ---
    bool l1 = true, l2 = false;
    bool log_result;

    log_result = l1 && l2;
    printf("l1 AND l2: %s\n", log_result ? "true" : "false");
    log_result = l1 || l2;
    printf("l1 OR l2: %s\n", log_result ? "true" : "false");
    log_result = !l1;
    printf("NOT l1: %s\n", log_result ? "true" : "false");

    // --- Type conversions ---
    int i = 7;
    float r = (float)i;            // integer to float
    double d = (double)r;          // float to double
    double complex c = r + 0.0*I;  // float to complex

    printf("Integer i: %d\n", i);
    printf("Converted to float: %.2f\n", r);
    printf("Converted to double: %.2f\n", d);
    printf("Converted to complex: %.2f + %.2fi\n", creal(c), cimag(c));

    // --- Combined logical and arithmetic operation ---
    int x_val = 10, y_val = 5;
    bool combined_check;

    combined_check = ((x_val + y_val) > 12) && (y_val != 0) && ((double)x_val / (double)y_val >= 4.3);
    printf("Combined operation result: %s\n", combined_check ? "true" : "false");

    return 0;
}
