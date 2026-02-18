#include <stdio.h>

int main() {
    int arr[5] = {1, 2, 3, 4, 5};
    int sum = 0;

    // Compute sum
    for (int i = 0; i < 5; i++) {  // C arrays are 0-indexed
        sum += arr[i];
    }
    printf("Sum of array elements: %d\n", sum);

    // Multiply each element by 2
    for (int i = 0; i < 5; i++) {
        arr[i] *= 2;
    }

    printf("Array after multiplying by 2: ");
    for (int i = 0; i < 5; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");

    return 0;
}
