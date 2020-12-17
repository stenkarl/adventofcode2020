package day13;

// A Java program to demonstrate
// working of Chinise remainder
// Theorem
import java.io.*;
import java.math.BigInteger;

public class GFG {

    // Returns modulo inverse of a
    // with respect to m using extended
    // Euclid Algorithm. Refer below post for details:
    // https://www.geeksforgeeks.org/multiplicative-inverse-under-modulo-m/
    static BigInteger inv(BigInteger a, BigInteger m)
    {
        BigInteger m0 = m, t, q;
        BigInteger x0 = new BigInteger("0"), x1 = new BigInteger("1");

        if (m.equals(BigInteger.ONE))
            return BigInteger.ZERO;

        // Apply extended Euclid Algorithm
        while (a.compareTo(BigInteger.ONE) > 0)
        {
            // q is quotient
            q = a.divide(m);

            t = m;

            // m is remainder now, process
            // same as euclid's algo
            m = a.mod(m);

            a = t;

            t = x0;

            BigInteger mult = q.multiply(x0);
            x0 = x1.subtract(mult);

            x1 = t;
        }

        // Make x1 positive
        if (x1.compareTo(BigInteger.ZERO) < 0) {
            x1 = x1.add(m0);
        }

        return x1;
    }

    // k is size of num[] and rem[].
    // Returns the smallest number
    // x such that:
    // x % num[0] = rem[0],
    // x % num[1] = rem[1],
    // ..................
    // x % num[k-2] = rem[k-1]
    // Assumption: Numbers in num[] are pairwise
    // coprime (gcd for every pair is 1)
    static BigInteger findMinX(BigInteger[] num, BigInteger[] rem, int k)
    {
        // Compute product of all numbers
        BigInteger prod = BigInteger.ONE;
        for (int i = 0; i < k; i++) {
            prod = prod.multiply(num[i]);
        }

        // Initialize result
        BigInteger result = BigInteger.ZERO;

        // Apply above formula
        for (int i = 0; i < k; i++)
        {
            BigInteger pp = prod.divide(num[i]);
            BigInteger product = rem[i].multiply(inv(pp, num[i])).multiply(pp);
            result = result.add(product);
        }

        return result.mod(prod);
    }

    // Driver method
    public static void main(String args[])
    {
        BigInteger[] num = {new BigInteger("1789"),
                new BigInteger("37"), new BigInteger("47"),
                new BigInteger("1889")};
        BigInteger[] rem = {BigInteger.ZERO, new BigInteger("36"),
                            new BigInteger("45"),
                            new BigInteger("1886")};
        int k = num.length;
        System.out.println("x is " +findMinX(num, rem, k));
    }
}

// This code is contributed by nikita Tiwari.

