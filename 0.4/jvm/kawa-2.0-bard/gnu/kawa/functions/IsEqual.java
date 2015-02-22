package gnu.kawa.functions;
import gnu.expr.Language;
import gnu.lists.*;
import java.lang.reflect.Array;
import java.util.Arrays;
import gnu.mapping.Promise;

/** Implement the standard Scheme procedure <tt>equal?</tt>
 * and the Lisp <tt>equal</tt>. */

public class IsEqual extends gnu.mapping.Procedure2
{
  Language language;

  public IsEqual(Language language, String name)
  {
    this.language = language;
    setName(name);
  }

  public static boolean numberEquals (Number num1, Number num2)
  {
    boolean exact1 = Arithmetic.isExact(num1);
    boolean exact2 = Arithmetic.isExact(num2);
    if (exact1 && exact2)
      return NumberCompare.$Eq(num1, num2);
    return exact1 == exact2 && num1.equals(num2);
  }

  public static boolean arrayEquals (Object arg1, Object arg2)
  {
    int len1 = Array.getLength(arg1);
    int len2 = Array.getLength(arg2);
    if (len1 != len2)
      return false;

    String tname1 = arg1.getClass().getName();
    String tname2 = arg2.getClass().getName();
    if (!tname1.equals(tname2))
      return false;

    if (tname1.length() == 2)   // matches primitive types
    {
      switch (tname1.charAt(1))
      {
        case 'Z':               // "[Z" ==> boolean[]
          return Arrays.equals((boolean[]) arg1, (boolean[]) arg2);
        case 'B':               // "[B" ==> byte[]
          return Arrays.equals((byte[]) arg1, (byte[]) arg2);
        case 'C':               // "[C" ==> char[]
          return Arrays.equals((char[]) arg1, (char[]) arg2);
        case 'D':               // "[D" ==> double[]
          return Arrays.equals((double[]) arg1, (double[]) arg2);
        case 'F':               // "[F" ==> float[]
          return Arrays.equals((float[]) arg1, (float[]) arg2);
        case 'I':               // "[I" ==> int[]
          return Arrays.equals((int[]) arg1, (int[]) arg2);
        case 'J':               // "[J" ==> long[]
          return Arrays.equals((long[]) arg1, (long[]) arg2);
        case 'S':               // "[S" ==> short[]
          return Arrays.equals((short[]) arg1, (short[]) arg2);
      }
    }
    for (int i = len1; --i >= 0; )
    {
      // FIXME handle cycles
      if (! apply(Array.get(arg1,i), Array.get(arg2,i)))
        return false;
    }
    return true;
  }

  public static boolean apply (Object arg1, Object arg2)
  {
    arg1 = Promise.force(arg1);
    arg2 = Promise.force(arg2);
    if (arg1 == arg2)
      return true;
    if (arg1 == null || arg2 == null)
      return false;
    if (arg1 instanceof Number && arg2 instanceof Number)
      return IsEqual.numberEquals((Number) arg1, (Number) arg2);
    // The complication is that we want a String and an FString to
    // compare as equal (if they are), because both a Scheme strings.
    /* #ifdef use:java.lang.CharSequence */
    if (arg1 instanceof CharSequence)
      {
        if (! (arg2 instanceof CharSequence))
          return false;
        CharSequence seq1 = (CharSequence) arg1;
        CharSequence seq2 = (CharSequence) arg2;
        int len1 = seq1.length();
        int len2 = seq2.length();
        if (len1 != len2)
          return false;
        for (int i = len1;  --i >= 0; )
          {
            if (seq1.charAt(i) != seq2.charAt(i))
              return false;
          }
        return true;
      }
    /* #else */
    // if (arg1 instanceof String && arg2 instanceof CharSeq)
    //   {
    //     String str1 = (String) arg1;
    //     CharSeq str2 = (CharSeq) arg2;
    //     int len1 = str1.length();
    //     int len2 = str2.length();
    //     if (len1 != len2)
    //       return false;
    //     for (int i = len1;  --i >= 0; )
    //       {
    //         if (str1.charAt(i) != str2.charAt(i))
    //           return false;
    //       }
    //     return true;
    //   }
    // if (arg1 instanceof CharSeq && arg2 instanceof String)
    //   {
    //     CharSeq str1 = (CharSeq) arg1;
    //     String str2 = (String) arg2;
    //     int len1 = str1.length();
    //     int len2 = str2.length();
    //     if (len1 != len2)
    //       return false;
    //     for (int i = len1;  --i >= 0; )
    //       {
    //         if (str1.charAt(i) != str2.charAt(i))
    //           return false;
    //       }
    //     return true;
    //   }
    /* #endif */
    if (arg1 instanceof FVector)
      {
        if (! (arg2 instanceof FVector))
          return false;
        FVector vec1 = (FVector) arg1;
        FVector vec2 = (FVector) arg2;
        int n = vec1.size();
        if (vec2.data == null || vec2.size() != n)
          return false;
        Object[] data1 = vec1.data;
        Object[] data2 = vec2.data;
        for (int i = n;  --i >= 0; )
          {
            // FIXME handle cycles
            if (! apply(data1[i], data2[i]))
              return false;
          }
        return true;
      }
    if (arg1 instanceof LList)
      {
        if (! (arg1 instanceof Pair) || ! (arg2 instanceof Pair))
          return false;
        Pair pair1 = (Pair) arg1;
        Pair pair2 = (Pair) arg2;
        for (;;)
          {
            Object x1 = pair1.getCar();
            Object x2 = pair2.getCar();
            if (! apply(x1, x2)) // FIXME handle cycles
              return false;
            x1 = pair1.getCdr();
            x2 = pair2.getCdr();
            if (x1 == x2)
              return true;
            if (x1 == null || x2 == null)
              return false;
            if (! (x1 instanceof Pair) || !(x2 instanceof Pair))
              return apply(x1, x2); // FIXME handle cycles
            pair1 = (Pair) x1;
            pair2 = (Pair) x2;
          }
      }
    if (arg1.getClass().isArray())
    {
      if (! (arg2.getClass().isArray()))
        return false;
      return IsEqual.arrayEquals(arg1, arg2);
    }
    return arg1.equals (arg2);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return language.booleanObject(apply(arg1, arg2));
  }

}
