#include <boost/math/constants/constants.hpp>
#include <boost/math/special_functions/owens_t.hpp>
#include <cmath>

extern "C"
{

namespace m = boost::math;

const double one_div_root_two = m::constants::one_div_root_two<double>();
// const double root_two_pi = m::constants::root_two_pi<double>();
const long double root_two_pi_long = m::constants::root_two_pi<long double>();
const long double one_div_root_two_pi_long = m::constants::one_div_root_two_pi<long double>();
const long double one_div_root_two_long = m::constants::one_div_root_two<long double>();
const long double log_root_two_pi_long = m::constants::log_root_two_pi<long double>();
// const long double log_two_long =
//   0.69314718055994530941723212145817656807550013436025525412068000949339362196969471560586332699641868754200148102Q;

long double xdnormx(long double x){
  return exp(log(x) - 0.5L*x*x - log_root_two_pi_long);
}

double pnorm(double q){
  if(std::isnan(q)){
    return nan("");
  }
  return 0.5*erfc(-q * one_div_root_two);
}

long double dnorm_long(long double x){
  return exp(-0.5L*x*x) * one_div_root_two_pi_long;
}

long double pnorm_long(long double q){
  return 0.5L * erfc(-q * one_div_root_two_long);
}

int sign(double x){
  return (std::signbit(x) ? -1 : 1);
}

//********* Owen T-function **************************************************//
//****** http://people.sc.fsu.edu/~jburkardt/cpp_src/owens/owens.html ********//
// double znorm1(double x){
//   if(std::isnan(x)){
//     return nan("");
//   }
//   return 0.5 * erf ( x * one_div_root_two );
// }
//
// double znorm2(double x){
//   if(std::isnan(x)){
//     return nan("");
//   }
//   return 0.5 * erfc ( x * one_div_root_two );
// }
//
// double tfun ( double h, double a, double ah ){
//   double as, hs, value;
//   double arange[7] = {0.025, 0.09, 0.15, 0.36, 0.5, 0.9, 0.99999};
//   double c2[21] = {0.99999999999999987510,
//                    -0.99999999999988796462,      0.99999999998290743652,
//                    -0.99999999896282500134,      0.99999996660459362918,
//                    -0.99999933986272476760,      0.99999125611136965852,
//                    -0.99991777624463387686,      0.99942835555870132569,
//                    -0.99697311720723000295,      0.98751448037275303682,
//                    -0.95915857980572882813,      0.89246305511006708555,
//                    -0.76893425990463999675,      0.58893528468484693250,
//                    -0.38380345160440256652,      0.20317601701045299653,
//                    -0.82813631607004984866E-01,  0.24167984735759576523E-01,
//                    -0.44676566663971825242E-02,  0.39141169402373836468E-03};
//   double hrange[14] = {
//     0.02, 0.06, 0.09, 0.125, 0.26,
//     0.4,  0.6,  1.6,  1.7,   2.33,
//     2.4,  3.36, 3.4,  4.8 };
//   int i, iaint, icode, ihint, m;
//   int meth[18] = {1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 4, 4, 4, 4, 5, 6 };
//   int ord[18] =  {2, 3, 4, 5, 7,10,12,18,10,20,30,20, 4, 7, 8,20,13, 0 };
//   double pts[13] = {0.35082039676451715489E-02,
//                     0.31279042338030753740E-01,  0.85266826283219451090E-01,
//                     0.16245071730812277011,      0.25851196049125434828,
//                     0.36807553840697533536,      0.48501092905604697475,
//                     0.60277514152618576821,      0.71477884217753226516,
//                     0.81475510988760098605,      0.89711029755948965867,
//                     0.95723808085944261843,      0.99178832974629703586};
//   int select[15*8] = {
//     1, 1, 2,13,13,13,13,13,13,13,13,16,16,16, 9,
//     1, 2, 2, 3, 3, 5, 5,14,14,15,15,16,16,16, 9,
//     2, 2, 3, 3, 3, 5, 5,15,15,15,15,16,16,16,10,
//     2, 2, 3, 5, 5, 5, 5, 7, 7,16,16,16,16,16,10,
//     2, 3, 3, 5, 5, 6, 6, 8, 8,17,17,17,12,12,11,
//     2, 3, 5, 5, 5, 6, 6, 8, 8,17,17,17,12,12,12,
//     2, 3, 4, 4, 6, 6, 8, 8,17,17,17,17,17,12,12,
//     2, 3, 4, 4, 6, 6,18,18,18,18,17,17,17,12,12 };
//   double wts[13] = {0.18831438115323502887E-01,
//                     0.18567086243977649478E-01,  0.18042093461223385584E-01,
//                     0.17263829606398753364E-01,  0.16243219975989856730E-01,
//                     0.14994592034116704829E-01,  0.13535474469662088392E-01,
//                     0.11886351605820165233E-01,  0.10070377242777431897E-01,
//                     0.81130545742299586629E-02,  0.60419009528470238773E-02,
//                     0.38862217010742057883E-02,  0.16793031084546090448E-02};
//   //
//   //  Determine appropriate method from t1...t6
//   //
//   ihint = 15;
//   for ( i = 1; i <= 14; i++ )
//   {
//     if ( h <= hrange[i-1] )
//     {
//       ihint = i;
//       break;
//     }
//   }
//   iaint = 8;
//   for ( i = 1; i <= 7; i++ )
//   {
//     if ( a <= arange[i-1] )
//     {
//       iaint = i;
//       break;
//     }
//   }
//   icode = select[ihint-1+(iaint-1)*15];
//   m = ord[icode-1];
//
//   if ( meth[icode-1] == 1 )
//   {
//     hs = - 0.5 * h * h;
//     double dhs = exp ( hs );
//     as = a * a;
//     int j = 1;
//     int jj = 1;
//     double aj = one_div_two_pi * a;
//     value = one_div_two_pi * atan ( a );
//     double dj = dhs - 1.0;
//     double gj = hs * dhs;
//     for ( ; ; )
//     {
//       value = value + dj * aj / ( double ) ( jj );
//       if ( m <= j )
//       {
//         return value;
//       }
//       j = j + 1;
//       jj = jj + 2;
//       aj = aj * as;
//       dj = gj - dj;
//       gj = gj * hs / ( double ) ( j );
//     }
//   }
//
//   else if ( meth[icode-1] == 2 )
//   {
//     int maxii = m + m + 1;
//     int ii = 1;
//     value = 0.0;
//     hs = h * h;
//     as = - a * a;
//     double vi = one_div_root_two_pi * a * exp ( - 0.5 * ah * ah );
//     double z = znorm1 ( ah ) / h;
//     double y = 1.0 / hs;
//     for ( ; ; )
//     {
//       value = value + z;
//       if ( maxii <= ii )
//       {
//         value = value * one_div_root_two_pi * exp ( - 0.5 * hs );
//         return value;
//       }
//       z = y * ( vi - ( double ) ( ii ) * z );
//       vi = as * vi;
//       ii = ii + 2;
//     }
//   }
//
//   else if ( meth[icode-1] == 3 )
//   {
//     i = 1;
//     int ii = 1;
//     value = 0.0;
//     hs = h * h;
//     as = a * a;
//     double vi = one_div_root_two_pi * a * exp ( - 0.5 * ah * ah );
//     double zi = znorm1 ( ah ) / h;
//     double y = 1.0 / hs;
//     for ( ; ; )
//     {
//       value = value + zi * c2[i-1];
//       if ( m < i )
//       {
//         value = value * one_div_root_two_pi * exp ( - 0.5 * hs );
//         return value;
//       }
//       zi = y  * ( ( double ) ( ii ) * zi - vi );
//       vi = as * vi;
//       i = i + 1;
//       ii = ii + 2;
//     }
//   }
//
//   else if ( meth[icode-1] == 4 )
//   {
//     int maxii = m + m + 1;
//     int ii = 1;
//     hs = h * h;
//     as = - a * a;
//     value = 0.0;
//     double ai = one_div_two_pi * a * exp ( - 0.5 * hs * ( 1.0 - as ) );
//     double yi = 1.0;
//     for ( ; ; )
//     {
//       value = value + ai * yi;
//       if ( maxii <= ii )
//       {
//         return value;
//       }
//       ii = ii + 2;
//       yi = ( 1.0 - hs * yi ) / ( double ) ( ii );
//       ai = ai * as;
//     }
//   }
//
//   else if ( meth[icode-1] == 5 )
//   {
//     value = 0.0;
//     as = a * a;
//     hs = - 0.5 * h * h;
//     double r;
//     for ( i = 1; i <= m; i++ )
//     {
//       r = 1.0 + as * pts[i-1];
//       value = value + wts[i-1] * exp ( hs * r ) / r;
//     }
//     value = a * value;
//   }
//
//   else if ( meth[icode-1] == 6 )
//   {
//     double normh = znorm2 ( h );
//     value = 0.5 * normh * ( 1.0 - normh );
//     double y = 1.0 - a;
//     double r = atan ( y / ( 1.0 + a ) );
//     if ( r != 0.0 )
//     {
//       value = value - one_div_two_pi * r * exp ( - 0.5 * y * h * h / r );
//     }
//   }
//   return value;
// }
//
double owent(double h, double a){
  return m::owens_t(h,a);
}
// double owent(double h, double a){
//   double absh = fabs ( h );
//   if(absh > DBL_MAX){
//     return 0.0;
//   }
//   double absa = fabs ( a );
//   if(absa > DBL_MAX){
//     return sign(a) * pnorm(-absh) / 2;
//   }
//
//   double cut = 0.67;
//   double normah, normh, value;
//   double ah = absa * absh;
//
//   if ( absa <= 1.0 )
//   {
//     value = tfun ( absh, absa, ah );
//   }
//   else if ( absh <= cut )
//   {
//     value = 0.25 - znorm1 ( absh ) * znorm1 ( ah )
//       - tfun ( ah, 1.0 / absa, absh );
//   }
//   else
//   {
//     normh = znorm2 ( absh );
//     normah = znorm2 ( ah );
//     value = 0.5 * ( normh + normah ) - normh * normah
//     - tfun ( ah, 1.0 / absa, absh );
//   }
//
//   if ( a < 0.0 )
//   {
//     value = - value;
//   }
//
//   return value;
// }
//****************************************************************************//

// ------ Student CDF ------------------------------------------------------- //
double* studentCDF_C(double q, int nu, double* delta, size_t J){
  const double a = sign(q)*sqrt(q*q/nu);
  const double sb = sqrt(nu/(nu+q*q));
  double* C = new double[J];
  for(size_t j=0; j<J; j++){
    double dsb = delta[j] * sb;
    C[j] = 2.0*owent(dsb, a) + pnorm(-dsb);
  }
  return C;
}

double* studentCDF(double q, size_t nu, double* delta, size_t J, double* out){
  size_t j;
  // if(nu > INT_MAX){
  //   for(int j=0; j<J; j++){
  //     out[j] = pnorm(q - delta[j]);
  //   }
  //   return out;
  // }
  // if(fabs(q) > DBL_MAX){
  //   for(int j=0; j<J; j++){
  //     out[j] = fabs(delta[j]) > DBL_MAX ?
  //                 (std::signbit(q) == std::signbit(delta[j]) ?
  //                   nan("") :
  //                   (std::signbit(q) ? 0 : 1)) :
  //                 (std::signbit(q) ? 0 : 1);
  //   }
  //   return out;
  // }
  if(nu == 1){
    double* C = studentCDF_C(q, nu, delta, J);
    for(j=0; j<J; j++){
      out[j] = C[j];
    }
    delete[] C;
    return out;
  }
  const long double qq = (long double)(q*q);
  const long double a = sqrt(qq/((long double)(nu))); //sign(q)*sqrt(qq/nu);
  const long double b = ((long double)(nu))/(((long double)(nu))+qq);
  const long double sb = sqrt(b);
  long double M[nu-1][J];
  for(j=0; j<J ; j++){
    long double dsb = ((long double)(delta[j])) * sb;
    M[0][j] = a * sb * dnorm_long(dsb) * pnorm_long(a*dsb);
  }
  if(nu>2){
    for(j=0; j<J; j++){
      M[1][j] = b * (delta[j] * a * M[0][j] +
                      a * dnorm_long(delta[j]) * one_div_root_two_pi_long);
    }
    if(nu>3){
      long double A[nu-3]; A[0] = 1.0L;
      size_t k;
      if(nu>4){
        for(k=1; k<nu-3; k++){
          A[k] = 1.0L/(k*A[k-1]);
        }
      }
      for(k=2; k<nu-1; k++){
        for(j=0; j<J; j++){
          M[k][j] = (k-1) * b *
                      (A[k-2] * delta[j] * a * M[k-1][j] + M[k-2][j]) / k;
        }
      }
    }
  }
  if(nu%2==1){
    double* C = studentCDF_C(q, nu, delta, J);
    for(j=0; j<J; j++){
      long double sum = 0.0L;
      for(size_t i=1; i<nu-1; i+=2){
        sum += M[i][j];
      }
      out[j] = C[j] + 2.0*((double)sum);
    }
    delete[] C;
  }else{
    for(j=0; j<J; j++){
      long double sum=0.0L; long double out_long;
      for(size_t i=0; i<nu-1; i+=2){
        sum += M[i][j];
      }
      out_long = pnorm_long(-delta[j]) + root_two_pi_long*sum;
      out[j] = (double)out_long;
    }
  }
  return out;
}

// ------- Owen Q-function -------------------------------------------------- //
double* OwenQ1_C(int nu, double t, double* delta, double* R, size_t J){
  const double a = sign(t)*sqrt(t*t/nu);
  const double b = nu/(nu+t*t);
  const double sb = sqrt(b);
  const double ab = sqrt(nu)/(nu/t+t);
  double* C = new double[J];
  for(size_t i=0; i<J; i++){
    double C1 = owent(delta[i]*sb, a);
    double C2 = owent(R[i], a-delta[i]/R[i]);
    double C3 = owent(delta[i]*sb, (ab-R[i]/delta[i])/b);
    C[i] = pnorm(R[i]) - (delta[i] >= 0) + 2.0*(C1 - C2 - C3);
  }
  return C;
}

double* OwenQ1(int algo, size_t nu, double t, double* delta, double* R,
                                                         size_t J, double* out){
  // if(t > DBL_MAX){
  //   for(int j=0; j<J; j++){
  //     out[j] = m::gamma_p(0.5*nu, 0.5*R[j]*R[j]);
  //   }
  // return out;
  // }
  // if(t < DBL_MIN || nu > INT_MAX){
  //   for(int j=0; j<J; j++){
  //     out[j] = 0.0;
  //   }
  // return out;
  // }
  size_t j;
  if(nu == 1){
    double* C = OwenQ1_C(nu, t, delta, R, J);
    for(j=0; j<J; j++){
      out[j] = C[j];
    }
    delete[] C;
    return out;
  }
  const long double tt = (long double)(t*t);
  const long double a = sign(t)*sqrt(tt/nu);
  const long double b = nu/(nu+tt);
  const long double sb = sqrt(b);
  // long double ab;
  // long double asb;
  // if(fabs(t) > DBL_MAX){
  //   ab = 0;
  //   asb = sign(t);
  // }else{
  const long double ab = (long double)(sqrt(nu)/(nu/t+t));
  const long double asb = sign(t)/sqrt(nu/tt+1.0L);
  long double dnormdsb[J];
  long double dabminusRoversb[J];
  long double dab[J];
  const size_t n = nu-1;
  long double H[n][J]; long double M[n][J];
  long double Lfactor[J];
  for(j=0; j<J; j++){
    dnormdsb[j] = dnorm_long(delta[j] * sb);
    dab[j] = delta[j]*ab;
    dabminusRoversb[j] = (dab[j] - R[j])/sb;
    Lfactor[j] = ab * dnorm_long(a*R[j]-delta[j]);
    H[0][j] = dnorm_long(R[j]);
    M[0][j] = asb * dnormdsb[j] *
                (pnorm_long(delta[j]*asb) - pnorm_long(dabminusRoversb[j]));
  }
  if(nu >= 3){
    for(j=0; j<J; j++){
      H[1][j] = xdnormx(R[j]);
      M[1][j] = dab[j]*M[0][j] + ab * dnormdsb[j] *
                  (dnorm_long(delta[j]*asb) - dnorm_long(dabminusRoversb[j]));
    }
    if(nu >= 4){
      if(algo == 1){
        long double A[n]; A[0] = 1.0L; A[1] = 1.0L;
        long double L[J];
        for(j=0; j<J; j++){
          L[j] = H[0][j];
        }
        for(size_t k=2; k<n; k++){
          A[k] = 1.0L / (k*A[k-1]);
          long double r = (long double)(k-1) / (long double)(k);
          for(j=0; j<J; j++){
            long double AkRj = A[k]*R[j];
            L[j] *= AkRj;
            H[k][j] = AkRj * H[k-1][j];
            M[k][j] = r * (A[k-2] * dab[j] * M[k-1][j] + b*M[k-2][j]) -
                        Lfactor[j]*L[j];
          }
        }
      }else{ // algo 2
        long double W[J]; long double logR[J];
        for(j=0; j<J; j++){
          long double Rj = (long double)(R[j]);
          W[j] = -(0.5L * Rj*Rj + log_root_two_pi_long);
          logR[j] = log(Rj);
        }
        long double A = 1.0L;
        long double u = 0.0L; long double v = 0.0L; long double ldf;
        for(size_t k=0; k<n-2; k++){
          long double kp1 = (long double)(k+1);
          long double kp2 = (long double)(k+2);
          if(k % 2 == 0){
            u += log(kp2);
            ldf = u;
          }else{
            v += log(kp2);
            ldf = v;
          }
          long double r = kp1 / kp2;
          for(j=0; j<J; j++){
            long double K = exp(-ldf + kp1*logR[j] + W[j]);
            H[k+2][j] = K*R[j];
            M[k+2][j] = r * (A*dab[j]*M[k+1][j] + b*M[k][j]) -
                          K*Lfactor[j];
          }
          A = 1.0L / (kp1*A);
        }
      }
    }
  }
  if(nu % 2 == 0){
    for(j=0; j<J; j++){
      long double sumH = 0.0L; long double sumM = 0.0L;
      for(size_t i=0; i<n; i+=2){
        sumH += H[i][j]; sumM += M[i][j];
      }
      long double out_long = pnorm_long(-delta[j]) + root_two_pi_long *
                              (sumM - pnorm_long(a*R[j]-delta[j])*sumH);
      out[j] = (double)out_long;
    }
    return out;
  }else{
    double* C = OwenQ1_C(nu, t, delta, R, J);
    for(j=0; j<J; j++){
      long double sumH = 0.0L; long double sumM = 0.0L;
      for(size_t i=1; i<n; i+=2){
        sumH += H[i][j]; sumM += M[i][j];
      }
      long double out_long = 2.0L*(sumM - pnorm_long(a*R[j]-delta[j])*sumH);
      out[j] = C[j] + (double)out_long;
    }
    delete[] C;
    return out;
  }
}

// --- Owen second Q-function ------------------------------------------- //
double* OwenQ2_C(int nu, double t, double* delta, double* R, size_t J){
  const double a = sign(t)*sqrt(t*t/nu);
  const double b = nu/(nu+t*t);
  const double ab = sqrt(nu)/(nu/t+t);
  const double sb = sqrt(nu/(nu+t*t));
  double* C = new double[J];
  for(size_t j=0; j<J; j++){
    double C2 = owent(R[j], a-delta[j]/R[j]);
    double C3 = owent(delta[j]*sb, (ab-R[j]/delta[j])/b);
    C[j] = 2.0*(C2 + C3) + (delta[j] >= 0) +
            pnorm(-delta[j]*sb) - pnorm(R[j]);
  }
  return C;
}

double* OwenQ2(int algo, size_t nu, double t, double* delta, double* R,
                size_t J, double* out){
  size_t j;
  if(nu == 1){
    double* C = OwenQ2_C(nu, t, delta, R, J);
    for(j=0; j<J; j++){
      out[j] = C[j];
    }
    delete[] C;
    return out;
  }
  const long double tt = (long double)(t*t);
  const long double a = sign(t)*sqrt(tt/nu);
  const long double b = nu/(nu+tt);
  const long double sb = sqrt(b);
  const long double ab = (long double)(sqrt(nu)/(nu/t+t));
  const long double asb = sign(t)/sqrt(nu/tt+1);
  long double dnormdsb[J];
  long double dab[J];
  long double dabminusRoversb[J];
  const size_t n = nu-1;
  long double H[n][J]; long double M[n][J];
  long double Lfactor[J];
  for(j=0; j<J; j++){
    dnormdsb[j] = dnorm_long(delta[j] * sb);
    dab[j] = delta[j]*ab;
    dabminusRoversb[j] = (dab[j] - R[j])/sb;
    H[0][j] = dnorm_long(R[j]);
    M[0][j] = asb * dnormdsb[j] * pnorm_long(dabminusRoversb[j]);
    Lfactor[j] = ab * dnorm_long(a*R[j]-delta[j]);
  }
  if(nu >= 3){
    for(j=0; j<J; j++){
      H[1][j] = xdnormx(R[j]);
      M[1][j] = dab[j]*M[0][j] + ab*dnormdsb[j]*dnorm_long(dabminusRoversb[j]);
    }
    if(nu >= 4){
      if(algo == 1){
        long double A[n]; A[0] = 1.0L; A[1] = 1.0L;
        long double L[J];
        for(j=0; j<J; j++){
          L[j] = H[0][j];
        }
        for(size_t k=2; k<n; k++){
          A[k] = 1.0L / (k*A[k-1]);
          long double r = (long double)(k-1) / (long double)(k);
          for(j=0; j<J; j++){
            long double AkRj = A[k]*R[j];
            L[j] *= AkRj;
            H[k][j] = AkRj * H[k-1][j];
            M[k][j] = r * (A[k-2]*dab[j]*M[k-1][j] + b*M[k-2][j]) +
                        Lfactor[j]*L[j];
          }
        }
      }else{ // algo 2
        long double W[J]; long double logR[J];
        for(j=0; j<J; j++){
          long double Rj = (long double)(R[j]);
          W[j] = -(0.5L * Rj*Rj + log_root_two_pi_long);
          logR[j] = log(Rj);
        }
        long double A = 1.0L;
        long double u = 0.0L; long double v = 0.0L; long double ldf;
        for(size_t k=0; k<n-2; k++){
          long double kp1 = (long double)(k+1);
          long double kp2 = (long double)(k+2);
          if(k % 2 == 0){
            u += log(kp2); ldf = u;
          }else{
            v += log(kp2); ldf = v;
          }
          long double r = kp1 / kp2;
          for(j=0; j<J; j++){
            long double K = exp(-ldf + kp1*logR[j] + W[j]);
            H[k+2][j] = K*R[j];
            M[k+2][j] = r * (A*dab[j]*M[k+1][j] + b*M[k][j]) + K*Lfactor[j];
          }
          A = 1.0L / (kp1*A);
        }
      }
    }
  }
  if(nu % 2 == 0){
    for(j=0; j<J; j++){
      long double sumH = 0.0L; long double sumM = 0.0L;
      for(size_t i=0; i<n; i+=2){
        sumH += H[i][j]; sumM += M[i][j];
      }
      long double out_long =
        root_two_pi_long*(sumM + pnorm_long(a*R[j]-delta[j])*sumH);
      out[j] = (double)out_long;
    }
    return out;
  }else{
    double* C = OwenQ2_C(nu, t, delta, R, J);
    for(j=0; j<J; j++){
      long double sumH = 0.0L; long double sumM = 0.0L;
      for(size_t i=1; i<n; i+=2){
        sumH += H[i][j]; sumM += M[i][j];
      }
      long double out_long = 2.0L*(sumM + pnorm_long(a*R[j]-delta[j])*sumH);
      out[j] = C[j] + (double)out_long;
    }
    delete[] C;
    return out;
  }
}

// --- Owen cumulative function 4 ------------------------------------------- //
double* OwenCDF4_C
       (int nu, double t1, double t2, double* delta1, double* delta2, size_t J){
  const double a1 = sign(t1)*sqrt(t1*t1/nu);
  const double sb1 = sqrt(nu/(nu+t1*t1));
  const double a2 = sign(t2)*sqrt(t2*t2/nu);
  const double sb2 = sqrt(nu/(nu+t2*t2));
  double* C = new double[J];
  for(size_t j=0; j<J; j++){
    double R = sqrt(nu)*(delta1[j] - delta2[j])/(t1-t2);
    double C1 = owent(delta2[j]*sb2, a2) - owent(delta1[j]*sb1, a1);
    double H21 = fabs(t1) < 1 ?
                  (t1-(t1-t2)/(1-delta2[j]/delta1[j]))/sqrt(nu) :
                  t1/sqrt(nu)*(1-(1-t2/t1)/(1-delta2[j]/delta1[j]));
    double H22 = fabs(t2) < 1 ?
                  (t2-(t1-t2)/(delta1[j]/delta2[j]-1))/sqrt(nu) :
                  t2/sqrt(nu)*(1-(t1/t2-1)/(delta1[j]/delta2[j]-1));
    double C2 = owent(R, H22) - owent(R, H21);
        //owent(R[j], a2-delta2[j]/R[j) - owent(R[j], a1-delta1[j]/R[j]);
    double H32 = fabs(t2) < 1 ?
                t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
                  (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/(t1-t2) :
                t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
                  (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/t2/(t1/t2-1);
    double H31 = fabs(t1) < 1 ?
                t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
                  (1-delta2[j]/delta1[j])/sqrt(nu)*nu/(t1-t2) :
                t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
                  (1-delta2[j]/delta1[j])/sqrt(nu)*nu/t1/(1-t2/t1);
    double C3 =
      owent(delta2[j]*sb2, H32) - //(delta2[j]*ab2-R[j])/b2/delta2[j]) -
          owent(delta1[j]*sb1, H31);
    C[j] = 2.0*(C1 - C2 - C3) + (delta1[j] >= 0) - (delta2[j] >= 0);
  }
  return C;
}

double* OwenCDF4(int algo, size_t nu, double t1, double t2, double* delta1,
                                         double* delta2, size_t J, double* out){
  // if(nu < 1){
  //   for(int j=0; j<J; j++){
  //     out[j] = nan("");
  //   }
  //   return out;
  // }
  // if(t1 <= t2){
  //   double* S1 = new double[J];
  //   S1 = studentCDF(t1, nu, delta1, J, S1);
  //   double* S2 = new double[J];
  //   S2 = studentCDF(t2, nu, delta2, J, S2);
  //   for(int j=0; j<J; j++){
  //     out[j] = S2[j] - S1[j];
  //   }
  //   delete[] S1;
  //   delete[] S2;
  //   return out;
  // }
  // if(t1 > DBL_MAX){
  //   for(int j=0; j<J; j++){
  //     out[j] = delta1[j] > DBL_MAX ? nan("") : 0;
  //   }
  //   return out;
  // }
  // // if(t1 < DBL_MIN){ // cela implique t1 <= t2 => inutile
  // //   int K=0;
  // //   int j;
  // //   for(j=0; j<J; j++){
  // //     if(delta1[j] < DBL_MIN){
  // //       out[j] = nan("");
  // //     }else{
  // //       K += 1;
  // //     }
  // //   }
  // //   if(K > 0){
  // //     int k=0;
  // //     double* d2 = new double[K];
  // //     int* indices = new int[K];
  // //     for(j=0; j<J; j++){
  // //       if(delta1[j] >= DBL_MIN){
  // //         d2[k] = delta2[k];
  // //         indices[k] = j;
  // //         k += 1;
  // //       }
  // //     }
  // //     double* S2 = studentCDF(t2, nu, d2, K, d2);
  // //     for(k=0; k<K; k++){
  // //       out[indices[k]] = S2[k];
  // //     }
  // //     delete[] d2;
  // //     delete[] indices;
  // //     delete[] S2;
  // //   }
  // // return out;
  // // }
  // // if(t2 > DBL_MAX){ // cela implique t1 <= t2 => inutile
  // //   for(int j=0; j<J; j++){
  // //     if(delta2[j] > DBL_MAX){
  // //       out[j] =  nan("");
  // //     }else{
  // //       double* d = new double[1];
  // //       d[0] = delta1[j];
  // //       double* S1 = studentCDF(t1, nu, d, 1, d);
  // //       out[j] = 1-S1[0];
  // //       delete[] d;
  // //       delete[] S1;
  // //     }
  // //   }
  // // return out;
  // // }
  // if(t2 < DBL_MIN){
  //   for(int j=0; j<J; j++){
  //     out[j] = delta2[j] < DBL_MIN ? nan("") : 0;
  //   }
  //   return out;
  // }
  // if(nu > INT_MAX){ // peut-être mieux dans Haskell si maxBound pas pareil
  //   for(int j=0; j<J; j++){
  //     out[j] = fmax(0, pnorm(t2-delta2[j]) - pnorm(t1-delta1[j]));
  //   }
  //   return out;
  // }
  size_t j;
  if(nu == 1){
    double* C = OwenCDF4_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      out[j] = C[j];
    }
    delete[] C;
    return out;
  }
  const long double t1t1(t1*t1);
//  const long double a1 = sign(t1)*sqrt(t1t1/nu);
  const long double b1 = nu/(nu+t1t1);
  const long double sb1 = sqrt(b1);
  // long double ab1, asb1;
  // if(fabs(t1) > DBL_MAX){ // est-ce utile ?..
  //   ab1 = (long double)(0);
  //   asb1 = (long double)(sign(t1));
  // }else{
  const long double ab1 = (long double)(sqrt(nu)/(nu/t1+t1));
  const long double asb1 = sign(t1)/sqrt(nu/t1t1+1.0L);
  const long double t2t2(t2*t2);
//  const long double a2 = sign(t2)*sqrt(t2t2/nu);
  const long double b2 = nu/(nu+t2t2);
  const long double sb2 = sqrt(b2);
  // const long double ab2 = fabs(t2) > DBL_MAX ?
  //                            (long double)(0) :
  //                            (long double)(sqrt(nu) * 1/(nu/t2+t2));
  // const long double asb2 = fabs(t2) > DBL_MAX ?
  //                             (long double)(sign(t2)) :
  //                             sign(t2) * sqrt(1/(nu/t2t2+1.0L));
  const long double ab2 = (long double)(sqrt(nu)/(nu/t2+t2));
  const long double asb2 = sign(t2)/sqrt(nu/t2t2+1.0L);
  long double R[J];
  long double dnormdsb1[J]; long double dnormdsb2[J];
  long double aRminusdelta1[J]; long double aRminusdelta2[J];
  long double dabminusRoversb1[J]; long double dabminusRoversb2[J];
  const size_t n = nu-1;
  long double M1[n][J]; long double M2[n][J]; long double H[n][J];
  long double Lfactor1[J]; long double Lfactor2[J];
  for(j=0; j<J; j++){
    R[j] = (long double)(sqrt(nu)*(delta1[j]-delta2[j])/(t1-t2));
    dnormdsb1[j] = dnorm_long(delta1[j] * sb1);
    dnormdsb2[j] = dnorm_long(delta2[j] * sb2);
    long double Roversb1 = fabs(t1) < 1 ?
      R[j]/sb1 :
      sign(t1)*(delta1[j]-delta2[j])*sqrt(nu/t1t1+1.0L)/(1-t2/t1);
    dabminusRoversb1[j] = delta1[j]*asb1 - Roversb1;
    long double Roversb2 = fabs(t2) < 1 ?
      R[j]/sb2 :
      sign(t2)*(delta1[j]-delta2[j])*sqrt(nu/t2t2+1.0L)/(t1/t2-1);
    dabminusRoversb2[j] = delta2[j]*asb2 - Roversb2;
    H[0][j] = dnorm_long(R[j]);
    M1[0][j] = asb1 * dnormdsb1[j] *
      (pnorm_long(delta1[j]*asb1) - pnorm_long(dabminusRoversb1[j]));
    M2[0][j] = asb2 * dnormdsb2[j] *
      (pnorm_long(delta2[j]*asb2) - pnorm_long(dabminusRoversb2[j]));
    aRminusdelta1[j] = asb1*Roversb1-delta1[j];
    aRminusdelta2[j] = asb2*Roversb2-delta2[j];
    Lfactor1[j] = ab1 * dnorm_long(aRminusdelta1[j]);
    Lfactor2[j] = ab2 * dnorm_long(aRminusdelta2[j]);
  }
  if(nu >= 3){
    long double dab1[J]; long double dab2[J];
    for(j=0; j<J; j++){
      // H[1][j] = R[j] > DBL_MAX ? 0 : R[j] * H[0][j]; // pas besoin car je vais traiter delta1=Inf
      // M1[1][j] = delta1[j] > DBL_MAX ? // idem, pas besoin
      //              0 :
      //              delta1[j]*ab1*M1[0][j] + ab1 * dnormdsb1[j] *
      //                (dnorm_long(delta1[j]*asb1) - dnorm_long(dabminusRoversb1[j]));
      dab1[j] = delta1[j]*ab1; dab2[j] = delta2[j]*ab2;
      H[1][j] = xdnormx(R[j]);
      M1[1][j] = dab1[j]*M1[0][j] + ab1 * dnormdsb1[j] *
                  (dnorm_long(delta1[j]*asb1) - dnorm_long(dabminusRoversb1[j]));
      M2[1][j] = dab2[j]*M2[0][j] + ab2 * dnormdsb2[j] *
                  (dnorm_long(delta2[j]*asb2) - dnorm_long(dabminusRoversb2[j]));
    }
    if(nu >= 4){
      if(algo == 1){
        long double A[n]; A[0] = 1.0L; A[1] = 1.0L;
        long double L[J];
        for(j=0; j<J; j++){
          L[j] = H[0][j];
        }
        for(size_t k=2; k<n; k++){
          A[k] = 1.0L / (k*A[k-1]);
          long double r = (long double)(k-1) / (long double)(k);
          for(j=0; j<J; j++){
            long double AkRj = A[k]*R[j];
            L[j] *= AkRj;
            H[k][j] = AkRj * H[k-1][j];
            M1[k][j] = r * (A[k-2] * dab1[j]*M1[k-1][j] + b1*M1[k-2][j]) -
                        Lfactor1[j]*L[j];
            M2[k][j] = r * (A[k-2] * dab2[j]*M2[k-1][j] + b2*M2[k-2][j]) -
                        Lfactor2[j]*L[j];
          }
        }
      }else{ // algo2
        long double W[J]; long double logR[J];
        for(j=0; j<J; j++){
          long double Rj = (long double)(R[j]);
          W[j] = -(0.5L * Rj*Rj + log_root_two_pi_long);
          logR[j] = log(Rj);
        }
        long double A = 1.0L;
        long double u = 0.0L; long double v = 0.0L; long double ldf;
        for(size_t k=0; k<n-2; k++){
          long double kp1 = (long double)(k+1);
          long double kp2 = (long double)(k+2);
          if(k % 2 == 0){
            u += log(kp2); ldf = u;
          }else{
            v += log(kp2); ldf = v;
          }
          long double r = kp1 / kp2;
          for(j=0; j<J; j++){
            long double K = exp(-ldf + kp1*logR[j] + W[j]);
            H[k+2][j] = K*R[j];
            M1[k+2][j] = r*(A*dab1[j]*M1[k+1][j] + b1*M1[k][j]) - K*Lfactor1[j];
            M2[k+2][j] = r*(A*dab2[j]*M2[k+1][j] + b2*M2[k][j]) - K*Lfactor2[j];
          }
          A = 1.0L / (kp1*A);
        }
      }
    }
  }
  if(nu % 2 == 0){
    for(j=0; j<J; j++){
      long double sumH=0.0L; long double sumM=0.0L; long double out_long;
      for(size_t i=0; i<n; i+=2){
        sumH += H[i][j]; sumM += M2[i][j] - M1[i][j];
      }
      out_long = root_two_pi_long*(sumM + sumH *
                (pnorm_long(aRminusdelta1[j]) - pnorm_long(aRminusdelta2[j]))) +
                pnorm_long(-delta2[j]) - pnorm_long(-delta1[j]);
      out[j] = (double)out_long;
    }
    return out;
  }else{
    double* C = OwenCDF4_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      long double sumH=0.0L; long double sumM=0.0L; long double out_long;
      for(size_t i=1; i<n; i+=2){
        sumH += H[i][j]; sumM += M2[i][j] - M1[i][j];
      }
      out_long = 2.0*(sumM + sumH *
                  (pnorm_long(aRminusdelta1[j]) - pnorm_long(aRminusdelta2[j])));
      out[j] = C[j] + (double)out_long;
    }
    delete[] C;
    return out;
  }
}

// --- Owen cumulative function 2 ------------------------------------------- //
double* OwenCDF2_C
       (int nu, double t1, double t2, double* delta1, double* delta2, size_t J){
  const double sb1 = sqrt(nu/(nu+t1*t1));
  const double sb2 = sqrt(nu/(nu+t2*t2));
  double* C = new double[J];
  for(size_t j=0; j<J; j++){
    double R = sqrt(nu)*(delta1[j] - delta2[j])/(t1-t2);
    double H21 = fabs(t1) < 1 ?
      (t1-(t1-t2)/(1-delta2[j]/delta1[j]))/sqrt(nu) :
      t1/sqrt(nu)*(1-(1-t2/t1)/(1-delta2[j]/delta1[j]));
    double H22 = fabs(t2) < 1 ?
      (t2-(t1-t2)/(delta1[j]/delta2[j]-1))/sqrt(nu) :
      t2/sqrt(nu)*(1-(t1/t2-1)/(delta1[j]/delta2[j]-1));
    double C2 = owent(R, H22) - owent(R, H21);
    double H32 = fabs(t2) < 1 ?
      t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
        (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/(t1-t2) :
      t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
        (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/t2/(t1/t2-1);
    double H31 = fabs(t1) < 1 ?
      t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
        (1-delta2[j]/delta1[j])/sqrt(nu)*nu/(t1-t2) :
      t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
        (1-delta2[j]/delta1[j])/sqrt(nu)*nu/t1/(1-t2/t1);
    double C3 =
      owent(delta2[j]*sb2, H32) - owent(delta1[j]*sb1, H31);
    C[j] = -2.0*(C2 + C3) + (delta1[j] >= 0) - (delta2[j] >= 0) +
            pnorm(-delta1[j]*sb1) - pnorm(-delta2[j]*sb2);
  }
  return C;
}

double* OwenCDF2(int algo, size_t nu, double t1, double t2, double* delta1,
                                         double* delta2, size_t J, double* out){
  size_t j;
  if(nu == 1){
    double* C = OwenCDF2_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      out[j] = C[j];
    }
    delete[] C;
    return out;
  }
  const long double t1t1(t1*t1);
  const long double b1 = nu/(nu+t1t1);
  const long double sb1 = sqrt(b1);
  const long double ab1 = (long double)(sqrt(nu)/(nu/t1+t1));
  const long double asb1 = sign(t1)/sqrt(nu/t1t1+1.0L);
  const long double t2t2(t2*t2);
  const long double b2 = nu/(nu+t2t2);
  const long double sb2 = sqrt(b2);
  const long double ab2 = (long double)(sqrt(nu)/(nu/t2+t2));
  const long double asb2 = sign(t2)/sqrt(nu/t2t2+1.0L);
  long double R[J];
  long double dnormdsb1[J]; long double dnormdsb2[J];
  long double aRminusdelta1[J]; long double aRminusdelta2[J];
  long double dabminusRoversb1[J]; long double dabminusRoversb2[J];
  const size_t n = nu-1;
  long double M1[n][J]; long double M2[n][J]; long double H[n][J];
  long double Lfactor1[J]; long double Lfactor2[J];
  for(j=0; j<J; j++){
    R[j] = (long double)(sqrt(nu)*(delta1[j]-delta2[j])/(t1-t2));
    dnormdsb1[j] = dnorm_long(delta1[j] * sb1);
    dnormdsb2[j] = dnorm_long(delta2[j] * sb2);
    long double Roversb1 = fabs(t1) < 1 ?
      R[j]/sb1 :
      sign(t1)*(delta1[j]-delta2[j])*sqrt(nu/t1t1+1.0L)/(1-t2/t1);
    dabminusRoversb1[j] = delta1[j]*asb1 - Roversb1;
    long double Roversb2 = fabs(t2) < 1 ?
      R[j]/sb2 :
      sign(t2)*(delta1[j]-delta2[j])*sqrt(nu/t2t2+1.0L)/(t1/t2-1);
    dabminusRoversb2[j] = delta2[j]*asb2 - Roversb2;
    H[0][j] = dnorm_long(R[j]);
    M1[0][j] = asb1 * dnormdsb1[j] * pnorm_long(dabminusRoversb1[j]);
    M2[0][j] = asb2 * dnormdsb2[j] * pnorm_long(dabminusRoversb2[j]);
    aRminusdelta1[j] = asb1*Roversb1-delta1[j];
    aRminusdelta2[j] = asb2*Roversb2-delta2[j];
    Lfactor1[j] = ab1 * dnorm_long(aRminusdelta1[j]);
    Lfactor2[j] = ab2 * dnorm_long(aRminusdelta2[j]);
  }
  if(nu >= 3){
    long double dab1[J]; long double dab2[J];
    for(j=0; j<J; j++){
      dab1[j] = delta1[j]*ab1; dab2[j] = delta2[j]*ab2;
      H[1][j] = xdnormx(R[j]);
      M1[1][j] = dab1[j]*M1[0][j] + ab1 * dnormdsb1[j] *
                  dnorm_long(dabminusRoversb1[j]);
      M2[1][j] = dab2[j]*M2[0][j] + ab2 * dnormdsb2[j] *
                  dnorm_long(dabminusRoversb2[j]);
    }
    if(nu >= 4){
      if(algo == 1){
        long double A[n]; A[0] = 1.0L; A[1] = 1.0L;
        long double L[J];
        for(j=0; j<J; j++){
          L[j] = H[0][j];
        }
        for(size_t k=2; k<n; k++){
          A[k] = 1.0L / (k*A[k-1]);
          long double r = (long double)(k-1) / (long double)(k);
          for(j=0; j<J; j++){
            long double AkRj = A[k]*R[j];
            L[j] *= AkRj;
            H[k][j] = AkRj * H[k-1][j];
            M1[k][j] = r * (A[k-2]*dab1[j]*M1[k-1][j] + b1*M1[k-2][j]) +
                        Lfactor1[j]*L[j];
            M2[k][j] = r * (A[k-2]*dab2[j]*M2[k-1][j] + b2*M2[k-2][j]) +
                        Lfactor2[j]*L[j];
          }
        }
      }else{ // algo2
        long double W[J]; long double logR[J];
        for(j=0; j<J; j++){
          long double Rj = (long double)(R[j]);
          W[j] = -(0.5L * Rj*Rj + log_root_two_pi_long);
          logR[j] = log(Rj);
        }
        long double A = 1.0L;
        long double u = 0.0L; long double v = 0.0L; long double ldf;
        for(size_t k=0; k<n-2; k++){
          long double kp1 = (long double)(k+1);
          long double kp2 = (long double)(k+2);
          if(k % 2 == 0){
            u += log(kp2); ldf = u;
          }else{
            v += log(kp2); ldf = v;
          }
          long double r = kp1 / kp2;
          for(j=0; j<J; j++){
            long double K = exp(-ldf + kp1*logR[j] + W[j]);
            H[k+2][j] = K*R[j];
            M1[k+2][j] = r*(A*dab1[j]*M1[k+1][j] + b1*M1[k][j]) + K*Lfactor1[j];
            M2[k+2][j] = r*(A*dab2[j]*M2[k+1][j] + b2*M2[k][j]) + K*Lfactor2[j];
          }
          A = 1.0L / (kp1*A);
        }
      }
    }
  }
  if(nu % 2 == 0){
    for(j=0; j<J; j++){
      long double sumH=0.0L; long double sumM=0.0L; long double out_long;
      for(size_t i=0; i<n; i+=2){
        sumH += H[i][j];
        sumM += M1[i][j] - M2[i][j];
      }
      out_long = root_two_pi_long*(sumM - sumH *
                (pnorm_long(aRminusdelta1[j]) - pnorm_long(aRminusdelta2[j])));
      out[j] = (double)out_long;
    }
    return out;
  }else{
    double* C = OwenCDF2_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      long double sumH=0.0L; long double sumM=0.0L; long double out_long;
      for(size_t i=1; i<n; i+=2){
        sumH += H[i][j]; sumM += M1[i][j] - M2[i][j];
      }
      out_long = 2.0*(sumM - sumH *
                  (pnorm_long(aRminusdelta1[j]) - pnorm_long(aRminusdelta2[j])));
      out[j] = C[j] + (double)out_long;
    }
    delete[] C;
    return out;
  }
}

// --- Owen cumulative function 1 ------------------------------------------- //
double* OwenCDF1_C
       (int nu, double t1, double t2, double* delta1, double* delta2, size_t J){
  const double a1 = sign(t1)*sqrt(t1*t1/nu);
  const double sb1 = sqrt(nu/(nu+t1*t1));
  const double sb2 = sqrt(nu/(nu+t2*t2));
  double* C = new double[J];
  for(size_t j=0; j<J; j++){
    double R = sqrt(nu)*(delta1[j] - delta2[j])/(t1-t2);
    double C1 = owent(delta1[j]*sb1, a1);
    double H21 = fabs(t1) < 1 ?
      (t1-(t1-t2)/(1-delta2[j]/delta1[j]))/sqrt(nu) :
      t1/sqrt(nu)*(1-(1-t2/t1)/(1-delta2[j]/delta1[j]));
    double H22 = fabs(t2) < 1 ?
      (t2-(t1-t2)/(delta1[j]/delta2[j]-1))/sqrt(nu) :
      t2/sqrt(nu)*(1-(t1/t2-1)/(delta1[j]/delta2[j]-1));
    double C2 = owent(R, H22) - owent(R, H21);
    double H32 = fabs(t2) < 1 ?
      t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
        (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/(t1-t2) :
      t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
        (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/t2/(t1/t2-1);
    double H31 = fabs(t1) < 1 ?
      t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
        (1-delta2[j]/delta1[j])/sqrt(nu)*nu/(t1-t2) :
      t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
        (1-delta2[j]/delta1[j])/sqrt(nu)*nu/t1/(1-t2/t1);
    double C3 =
      owent(delta2[j]*sb2, H32) - owent(delta1[j]*sb1, H31);
    C[j] = 2.0*(C1 + C2 + C3) - (delta1[j] >= 0) + (delta2[j] >= 0) +
            pnorm(-delta2[j]*sb2);
  }
  return C;
}

double* OwenCDF1
  (int algo, size_t nu, double t1, double t2, double* delta1,
                                         double* delta2, size_t J, double* out){
  size_t j;
  if(nu == 1){
    double* C = OwenCDF1_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      out[j] = C[j];
    }
    delete[] C;
    return out;
  }
  const long double t1t1(t1*t1);
  const long double b1 = nu/(nu+t1t1);
  const long double sb1 = sqrt(b1);
  const long double ab1 = (long double)(sqrt(nu)/(nu/t1+t1));
  const long double asb1 = sign(t1)/sqrt(nu/t1t1+1.0L);
  const long double t2t2(t2*t2);
  const long double b2 = nu/(nu+t2t2);
  const long double sb2 = sqrt(b2);
  const long double ab2 = (long double)(sqrt(nu)/(nu/t2+t2));
  const long double asb2 = sign(t2)/sqrt(nu/t2t2+1.0L);
  long double R[J];
  long double dnormdsb1[J]; long double dnormdsb2[J];
  long double dabminusRoversb1[J]; long double dabminusRoversb2[J];
  const size_t n = nu-1;
  long double M1[n][J]; long double M2[n][J];
  long double Lfactor1[J]; long double Lfactor2[J];
  for(j=0; j<J; j++){
    R[j] = (long double)(sqrt(nu)*(delta1[j]-delta2[j])/(t1-t2));
    dnormdsb1[j] = dnorm_long(delta1[j] * sb1);
    dnormdsb2[j] = dnorm_long(delta2[j] * sb2);
    long double Roversb1 = fabs(t1) < 1 ?
      R[j]/sb1 :
      sign(t1)*(delta1[j]-delta2[j])*sqrt(nu/t1t1+1.0L)/(1-t2/t1);
    dabminusRoversb1[j] = delta1[j]*asb1 - Roversb1;
    long double Roversb2 = fabs(t2) < 1 ?
      R[j]/sb2 :
      sign(t2)*(delta1[j]-delta2[j])*sqrt(nu/t2t2+1.0L)/(t1/t2-1);
    dabminusRoversb2[j] = delta2[j]*asb2 - Roversb2;
    M1[0][j] = asb1 * dnormdsb1[j] *
      (pnorm_long(delta1[j]*asb1) - pnorm_long(dabminusRoversb1[j]));
    M2[0][j] = asb2 * dnormdsb2[j] * pnorm_long(dabminusRoversb2[j]);
    Lfactor1[j] = ab1 * dnorm_long(asb1*Roversb1-delta1[j]);
    Lfactor2[j] = ab2 * dnorm_long(asb2*Roversb2-delta2[j]);
  }
  if(nu >= 3){
    long double dab1[J]; long double dab2[J];
    for(j=0; j<J; j++){
      dab1[j] = delta1[j]*ab1; dab2[j] = delta2[j]*ab2;
      M1[1][j] = dab1[j]*M1[0][j] + ab1 * dnormdsb1[j] *
          (dnorm_long(delta1[j]*asb1) - dnorm_long(dabminusRoversb1[j]));
      M2[1][j] = dab2[j]*M2[0][j] + ab2 * dnormdsb2[j] *
                  dnorm_long(dabminusRoversb2[j]);
    }
    if(nu >= 4){
      if(algo == 1){
        long double A[n]; A[0] = 1.0L; A[1] = 1.0L;
        long double L[J];
        for(j=0; j<J; j++){
          L[j] = dnorm_long(R[j]);
        }
        for(size_t k=2; k<n; k++){
          A[k] = 1.0L / (k*A[k-1]);
          long double r = (long double)(k-1) / (long double)(k);
          for(j=0; j<J; j++){
            long double AkRj = A[k]*R[j];
            L[j] *= AkRj;
            M1[k][j] = r * (A[k-2]*dab1[j]*M1[k-1][j] + b1*M1[k-2][j]) -
                        Lfactor1[j]*L[j];
            M2[k][j] = r * (A[k-2]*dab2[j]*M2[k-1][j] + b2*M2[k-2][j]) +
                        Lfactor2[j]*L[j];
          }
        }
      }else{ // algo2
        long double W[J]; long double logR[J];
        for(j=0; j<J; j++){
          long double Rj = (long double)(R[j]);
          W[j] = -(0.5L * Rj*Rj + log_root_two_pi_long);
          logR[j] = log(Rj);
        }
        long double A = 1.0L;
        long double u = 0.0L; long double v = 0.0L; long double ldf;
        for(size_t k=0; k<n-2; k++){
          long double kp1 = (long double)(k+1);
          long double kp2 = (long double)(k+2);
          if(k % 2 == 0){
            u += log(kp2); ldf = u;
          }else{
            v += log(kp2); ldf = v;
          }
          long double r = kp1 / kp2;
          for(j=0; j<J; j++){
            long double K = exp(-ldf + kp1*logR[j] + W[j]);
            M1[k+2][j] = r*(A*dab1[j]*M1[k+1][j] + b1*M1[k][j]) - K*Lfactor1[j];
            M2[k+2][j] = r*(A*dab2[j]*M2[k+1][j] + b2*M2[k][j]) + K*Lfactor2[j];
          }
          A = 1.0L / (kp1*A);
        }
      }
    }
  }
  if(nu % 2 == 0){
    for(j=0; j<J; j++){
      long double sumM=0; long double out_long;
      for(size_t i=0; i<n; i+=2){
        sumM += M2[i][j] + M1[i][j];
      }
      out_long = root_two_pi_long*sumM + pnorm_long(-delta1[j]);
      out[j] = (double)out_long;
    }
    return out;
  }else{
    double* C = OwenCDF1_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      long double sumM = 0.0L;
      for(size_t i=1; i<n; i+=2){
        sumM += M2[i][j] + M1[i][j];
      }
      out[j] = C[j] + (double)(2.0L*sumM);
    }
    delete[] C;
    return out;
  }
}

// --- Owen cumulative function 3 ------------------------------------------- //
double* OwenCDF3_C
       (int nu, double t1, double t2, double* delta1, double* delta2, size_t J){
  const double sb1 = sqrt(nu/(nu+t1*t1));
  const double a2 = sign(t2)*sqrt(t2*t2/nu);
  const double sb2 = sqrt(nu/(nu+t2*t2));
  double* C = new double[J];
  for(size_t j=0; j<J; j++){
    double R = sqrt(nu)*(delta1[j] - delta2[j])/(t1-t2);
    double C1 = -owent(delta2[j]*sb2, a2);
    double H21 = fabs(t1) < 1 ?
    (t1-(t1-t2)/(1-delta2[j]/delta1[j]))/sqrt(nu) :
      t1/sqrt(nu)*(1-(1-t2/t1)/(1-delta2[j]/delta1[j]));
    double H22 = fabs(t2) < 1 ?
    (t2-(t1-t2)/(delta1[j]/delta2[j]-1))/sqrt(nu) :
      t2/sqrt(nu)*(1-(t1/t2-1)/(delta1[j]/delta2[j]-1));
    double C2 = owent(R, H22) - owent(R, H21);
    double H32 = fabs(t2) < 1 ?
    t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
    (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/(t1-t2) :
      t2/sqrt(nu)*(1- (delta1[j]/delta2[j]-1)/(t1/t2-1)) -
        (delta1[j]/delta2[j]-1)/sqrt(nu)*nu/t2/(t1/t2-1);
    double H31 = fabs(t1) < 1 ?
    t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
    (1-delta2[j]/delta1[j])/sqrt(nu)*nu/(t1-t2) :
      t1/sqrt(nu)*(1- (1-delta2[j]/delta1[j])/(1-t2/t1)) -
        (1-delta2[j]/delta1[j])/sqrt(nu)*nu/t1/(1-t2/t1);
    double C3 =
      owent(delta2[j]*sb2, H32) - owent(delta1[j]*sb1, H31);
    C[j] = 2.0*(C1 + C2 + C3) - (delta1[j] >= 0) + (delta2[j] >= 0) -
            pnorm(-delta1[j]*sb1) + 1.0;
  }
  return C;
}

double* OwenCDF3(int algo, size_t nu, double t1, double t2, double* delta1,
                                        double* delta2, size_t J, double* out){
  size_t j;
  if(nu == 1){
    double* C = OwenCDF3_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      out[j] = C[j];
    }
    delete[] C;
    return out;
  }
  const long double t1t1(t1*t1);
  const long double b1 = nu/(nu+t1t1);
  const long double sb1 = sqrt(b1);
  const long double ab1 = (long double)(sqrt(nu)/(nu/t1+t1));
  const long double asb1 = sign(t1) / sqrt(nu/t1t1+1.0L);
  const long double t2t2(t2*t2);
  const long double b2 = nu/(nu+t2t2);
  const long double sb2 = sqrt(b2);
  const long double ab2 = (long double)(sqrt(nu)/(nu/t2+t2));
  const long double asb2 = sign(t2) / sqrt(nu/t2t2+1.0L);
  long double R[J];
  long double dnormdsb1[J]; long double dnormdsb2[J];
  long double aRminusdelta1[J]; long double aRminusdelta2[J];
  long double dabminusRoversb1[J]; long double dabminusRoversb2[J];
  const size_t n = nu-1;
  long double M1[n][J]; long double M2[n][J]; long double H[n][J];
  long double Lfactor1[J]; long double Lfactor2[J];
  for(j=0; j<J; j++){
    R[j] = (long double)(sqrt(nu)*(delta1[j]-delta2[j])/(t1-t2));
    dnormdsb1[j] = dnorm_long(delta1[j] * sb1);
    dnormdsb2[j] = dnorm_long(delta2[j] * sb2);
    long double Roversb1 = fabs(t1) < 1 ?
      R[j]/sb1 :
      sign(t1)*(delta1[j]-delta2[j])*sqrt(nu/t1t1+1.0L)/(1-t2/t1);
    dabminusRoversb1[j] = delta1[j]*asb1 - Roversb1;
    long double Roversb2 = fabs(t2) < 1 ?
      R[j]/sb2 :
      sign(t2)*(delta1[j]-delta2[j])*sqrt(nu/t2t2+1.0L)/(t1/t2-1);
    dabminusRoversb2[j] = delta2[j]*asb2 - Roversb2;
    H[0][j] = dnorm_long(R[j]);
    M1[0][j] = asb1 * dnormdsb1[j] * pnorm_long(dabminusRoversb1[j]);
    M2[0][j] = asb2 * dnormdsb2[j] *
      (pnorm_long(delta2[j]*asb2) - pnorm_long(dabminusRoversb2[j]));
    aRminusdelta1[j] = asb1*Roversb1-delta1[j];
    aRminusdelta2[j] = asb2*Roversb2-delta2[j];
    Lfactor1[j] = ab1 * dnorm_long(aRminusdelta1[j]);
    Lfactor2[j] = ab2 * dnorm_long(aRminusdelta2[j]);
  }
  if(nu >= 3){
    long double dab1[J]; long double dab2[J];
    for(j=0; j<J; j++){
      dab1[j] = delta1[j]*ab1; dab2[j] = delta2[j]*ab2;
      H[1][j] = xdnormx(R[j]);
      M1[1][j] = dab1[j]*M1[0][j] + ab1 * dnormdsb1[j] *
                  dnorm_long(dabminusRoversb1[j]);
      M2[1][j] = dab2[j]*M2[0][j] + ab2 * dnormdsb2[j] *
                  (dnorm_long(delta2[j]*asb2) - dnorm_long(dabminusRoversb2[j]));
    }
    if(nu >= 4){
      if(algo == 1){
        long double A[n]; A[0] = 1.0L; A[1] = 1.0L;
        long double L[J];
        for(j=0; j<J; j++){
          L[j] = H[0][j];
        }
        for(size_t k=2; k<n; k++){
          A[k] = 1.0L / (k*A[k-1]);
          long double r = (long double)(k-1) / (long double)(k);
          for(j=0; j<J; j++){
            long double AkRj = A[k]*R[j];
            L[j] *= AkRj;
            H[k][j] = AkRj * H[k-1][j];
            M1[k][j] = r * (A[k-2]*dab1[j]*M1[k-1][j] + b1*M1[k-2][j]) +
                        Lfactor1[j]*L[j];
            M2[k][j] = r * (A[k-2]*dab2[j]*M2[k-1][j] + b2*M2[k-2][j]) -
                        Lfactor2[j]*L[j];
          }
        }
      }else{ // algo2
        long double W[J]; long double logR[J];
        for(j=0; j<J; j++){
          long double Rj = (long double)(R[j]);
          W[j] = -(0.5L * Rj*Rj + log_root_two_pi_long);
          logR[j] = log(Rj);
        }
        long double A = 1.0L;
        long double u = 0.0L; long double v = 0.0L; long double ldf;
        for(size_t k=0; k<n-2; k++){
          long double kp1 = (long double)(k+1);
          long double kp2 = (long double)(k+2);
          if(k % 2 == 0){
            u += log(kp2); ldf = u;
          }else{
            v += log(kp2); ldf = v;
          }
          long double r = kp1 / kp2;
          for(j=0; j<J; j++){
            long double K = exp(-ldf + kp1*logR[j] + W[j]);
            H[k+2][j] = K*R[j];
            M1[k+2][j] = r*(A*dab1[j]*M1[k+1][j] + b1*M1[k][j]) + K*Lfactor1[j];
            M2[k+2][j] = r*(A*dab2[j]*M2[k+1][j] + b2*M2[k][j]) - K*Lfactor2[j];
          }
          A = 1.0L / (kp1*A);
        }
      }
    }
  }
  if(nu % 2 == 0){
    for(j=0; j<J; j++){
      long double sumH=0.0L; long double sumM=0.0L; long double out_long;
      for(size_t i=0; i<n; i+=2){
        sumH += H[i][j]; sumM += -M1[i][j] - M2[i][j];
      }
      out_long = root_two_pi_long*(sumM + sumH *
                (pnorm_long(aRminusdelta1[j]) - pnorm_long(aRminusdelta2[j]))) +
                1.0L - pnorm_long(-delta2[j]);
      out[j] = (double)out_long;
    }
    return out;
  }else{
    double* C = OwenCDF3_C(nu, t1, t2, delta1, delta2, J);
    for(j=0; j<J; j++){
      long double sumH=0.0L; long double sumM=0.0L; long double out_long;
      for(size_t i=1; i<n; i+=2){
        sumH += H[i][j]; sumM += -M1[i][j] - M2[i][j];
      }
      out_long = 2.0L*(sumM + sumH *
                (pnorm_long(aRminusdelta1[j]) - pnorm_long(aRminusdelta2[j])));
      out[j] = C[j] + (double)out_long;
    }
    delete[] C;
    return out;
  }
}


}
