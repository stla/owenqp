# owenqp

Evaluation of the Owen Q-function in quad precision.

The two Owen Q-functions are

![equation](http://latex.codecogs.com/gif.latex?Q_1%28%5Cnu%2C%20t%2C%20%5Cdelta%2C%20R%29%20%3D%20%5Cfrac%7B1%7D%7B%5CGamma%5Cleft%28%5Cfrac%7B%5Cnu%7D%7B2%7D%5Cright%292%5E%7B%5Cfrac12%28%5Cnu-2%29%7D%7D%5Cint_0%5ER%20%5CPhi%5Cleft%28%5Cfrac%7Btx%7D%7B%5Csqrt%7B%5Cnu%7D%7D-%5Cdelta%5Cright%29x%5E%7B%5Cnu-1%7D%20e%5E%7B-%5Cfrac%7Bx%5E2%7D%7B2%7D%7D%20%5Cmathrm%7Bd%7Dx%2C)

![equation](http://latex.codecogs.com/gif.latex?Q_2%28%5Cnu%2C%20t%2C%20%5Cdelta%2C%20R%29%20%3D%20%5Cfrac%7B1%7D%7B%5CGamma%5Cleft%28%5Cfrac%7B%5Cnu%7D%7B2%7D%5Cright%292%5E%7B%5Cfrac12%28%5Cnu-2%29%7D%7D%5Cint_R%5E%5Cinfty%20%5CPhi%5Cleft%28%5Cfrac%7Btx%7D%7B%5Csqrt%7B%5Cnu%7D%7D-%5Cdelta%5Cright%29x%5E%7B%5Cnu-1%7D%20e%5E%7B-%5Cfrac%7Bx%5E2%7D%7B2%7D%7D%20%5Cmathrm%7Bd%7Dx.)

These functions are available in the library and they are respectively called
`OwenQ1` and `OwenQ2`.

The algorithm is implemented in C++. In fact, you can choose among three C++
files: `owendouble.cpp`, `owenlong.cpp` and `owen.cpp`. They respectively
evaluate the functions in double precision, long precision and quadruple precision.
Just change this line of the cabal file to make your choice:

```
C-sources:           cppfiles/owenlong.cpp
```

IMHO the long precision is enough. The quadruple precision is quite slow.

The library depends on `boost`, the well-known collection of C++ libraries.
It is used to evaluate the Owen T-function, and it is also used in `owen.cpp` to
have the quadruple precision. The Owen T-function is available in the library,
it is called `OwenT`.

There are other functions available in the library: the four Owen cumulative
distribution functions studied in Owen's paper
*A special case of a bivariate noncentral t-distribution* (1965). They are
called `OwenCDF1`, `OwenCDF2`, `OwenCDF3`, `OwenCDF4`.
