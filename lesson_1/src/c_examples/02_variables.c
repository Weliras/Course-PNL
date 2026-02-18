#include <stdio.h>
#include <complex.h>  // For complex numbers
#include <stdbool.h>  // For boolean type

int main() {
    // Declaration and initialization
    int age = 25;
    float height = 1.75;
    double complex z = 3.0 + 4.0*I;  // Complex number: 3 + 4i
    char name[10] = "Alice";         // String of max length 10
    bool isStudent = true;           // Boolean value

    // Printing
    printf("Age: %d\n", age);
    printf("Height: %.2f\n", height);
    printf("The complex number z is: %.2f + %.2fi\n", creal(z), cimag(z));
    printf("Name: %s\n", name);
    printf("Is Student: %s\n", isStudent ? "true" : "false");

    return 0;
}
