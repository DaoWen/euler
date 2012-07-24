package project_euler;

public class Crunch {

  public static long crunch(long n, long t, long x, long y, long i) {
    while (true) {
      if (t == 0) return y; // Success!
      else if (i < -n) return 0; // Failed
      else {
        // Count-on-bits
        long k = 0, tmp = t;
        {
          while (tmp > 0) {
            tmp &= tmp-1;
            ++k;
          }
        }
        if (i < 0) { y=x; i=n-k; }
        else { i-=k; }
        --t;
        x-=k;
      }
    }
  }

}
