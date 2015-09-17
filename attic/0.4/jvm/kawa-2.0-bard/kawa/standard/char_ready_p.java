package kawa.standard;

public class char_ready_p {
  public static boolean ready (Object arg1)
  {
    try
      {
	if (arg1 instanceof java.io.InputStream)
	  return ((java.io.InputStream) arg1).available () > 0;
	else if (arg1 instanceof java.io.Reader)
	  return ((java.io.Reader)arg1).ready();
	else
	  throw new ClassCastException("invalid argument to char-ready?");
      }
    catch (java.io.IOException ex)
      {
	return false;
      }
  }
}
