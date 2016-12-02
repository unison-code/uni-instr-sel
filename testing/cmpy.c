typedef struct SComplex {
    short real;
    short img;
} Complex;

Complex cmpy(Complex a, Complex b) {
    Complex r;
    r.real = a.real*b.real - a.img*b.img;
    r.img = a.real*b.img + a.img*b.real;
    return r;
}
