// Copyright (c) 2003, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import java.util.*;

/** Mananges a table of named options,
 * Can inherit from another table of "default" options. */

public class Options
{
  /** Bit indicating option value is a boolean. */
  public static final int BOOLEAN_OPTION = 1;

  public static final int STRING_OPTION = 2;

  /** The option table contain defaults, that we "inherit" from. */
  Options previous;

  OptionInfo first;
  OptionInfo last;

  public Options ()
  {
  }

  public Options (Options previous)
  {
    this.previous = previous;
  }

  /** Maps property keys to options values. */
  HashMap<String,Object> valueTable;

  /** Maps property keys to OptionInfo. */
  HashMap<String,OptionInfo> infoTable;

  /** Create a new option and enters it in this table.
   * A duplicate option throws a RuntimeException.
   * @param key the options name (key).
   * @param kind type and other flag bits of the option.
   * @param documentation a String describing what the option does. */
  public OptionInfo add(String key, int kind, String documentation)
  {
    return add(key, kind, null, documentation);
  }

  public OptionInfo add(String key, int kind, Object defaultValue,
                        String documentation)
  {
    if (infoTable == null)
      infoTable = new HashMap<String,OptionInfo>();
    else if (infoTable.get(key) != null)
      throw new RuntimeException("duplicate option key: "+key);
    OptionInfo info = new OptionInfo();
    info.key = key;
    info.kind = kind;
    info.defaultValue = defaultValue;
    info.documentation = documentation;
    if (first == null)
      first = info;
    else
      last.next = info;
    last = info;
    infoTable.put(key, info);
    return info;
  }

  static Object valueOf (OptionInfo info, String argument)
  {
    if ((info.kind & BOOLEAN_OPTION) != 0)
      {
    	if (argument == null
	    || argument.equals("1")
	    || argument.equals("on")
	    || argument.equals("yes")
	    || argument.equals("true"))
	  return Boolean.TRUE;
	if (argument.equals("0")
	    || argument.equals("off")
	    || argument.equals("no")
	    || argument.equals("false"))
	  return Boolean.FALSE;
	return null;
      }
    return argument;
  }

  private void error(String message, SourceMessages messages)
  {
    if (messages == null)
      throw new RuntimeException(message);
    else
      messages.error('e', message);
  }

  /** Set the value of a named option. */
  public void set (String key, Object value)
  {
    set(key, value, null);
  }

  /** Set the value of a named option.
   * Return old value, in form suitable for reset.  */
  public Object set(String key, Object value, SourceMessages messages)
  {
    if (valueTable == null)
      valueTable = new HashMap<String,Object>();
    Object oldValue = valueTable.get(key);
    OptionInfo info = getInfo(key);
    if (info == null)
      {
	error("invalid option key: "+key, messages);
	return oldValue;
      }
    if ((info.kind & BOOLEAN_OPTION) != 0)
      {
	if (value instanceof String)
	  value = valueOf(info, (String) value);
	if (! (value instanceof Boolean))
	  {
	    error("value for option "+key
		  +" must be boolean or yes/no/true/false/on/off/1/0",
		  messages);
	    return oldValue;
	  }
      }
    else if (value == null)
      value = "";
    valueTable.put(key, value);
    return oldValue;
  }

  /** Reset the value of a named option. */
  public void reset (String key, Object oldValue)
  {
    if (valueTable == null)
      valueTable = new HashMap<String,Object>();
    if (oldValue == null)
      valueTable.remove(key);
    else
      valueTable.put(key, oldValue);
  }

  public static final String UNKNOWN = "unknown option name";

  /** Set the value of the key to the argument, appropriate parsed.
   * return null on success or a String error message.
   * If the option key is invalid, return UNKNOWN. */
  public String set (String key, String argument)
  {
    OptionInfo info = getInfo(key);
    if (info == null)
      return UNKNOWN;
    Object value = valueOf(info, argument);
    if (value == null)
      {
	if ((info.kind & BOOLEAN_OPTION) != 0)
	  return "value of option "+key+" must be yes/no/true/false/on/off/1/0";
      }
    if (valueTable == null)
      valueTable = new HashMap<String,Object>();
    valueTable.put(key, value);
    return null;
  }

  public OptionInfo getInfo (String key)
  {
    Object info = infoTable == null ? null : infoTable.get(key);
    if (info == null && previous != null)
      info = previous.getInfo(key);
    return (OptionInfo) info;
  }

  /** Get the value for the option.
   * Throws an except if there is no option by that name,
   * Returns defaultValue if there is such an option, but it
   * hasn't been set. */
  public Object get (String key, Object defaultValue)
  {
    OptionInfo info = getInfo(key);
    if (info == null)
      throw new RuntimeException("invalid option key: "+key);
    return get(info, defaultValue);
  }

  public Object get (OptionInfo key, Object defaultValue)
  {
    Options options = this;
    while (options != null)
      {
        for (OptionInfo info = key;  ; )
          {
            Object val = options.valueTable == null ? null
              : options.valueTable.get(info.key);
            if (val != null)
              return val;
            if (info.defaultValue instanceof OptionInfo)
              info = (OptionInfo) info.defaultValue;
            else
              {
                if (info.defaultValue != null)
                  defaultValue = info.defaultValue;
                break;
              }
          }
        options = options.previous;
      }
    return defaultValue;
  }

  public Object get (OptionInfo key)
  {
    return get(key, null);
  }

  /** Get current option value.
   * Only look in local table, not in inherited Options.
   * Return null if there is no binding (even when get would
   * throw an exception on an unknown option).
   */
  public Object getLocal (String key)
  {
    return valueTable == null ? null : valueTable.get(key);
  }

  public boolean getBoolean (String key)
  {
    return ((Boolean) get (key, Boolean.FALSE)).booleanValue();
  }

  public boolean getBoolean (String key, boolean defaultValue)
  {
    Boolean defaultObject = defaultValue ? Boolean.TRUE : Boolean.FALSE;
    return ((Boolean) get (key, defaultObject)).booleanValue();
  }

  public boolean getBoolean (OptionInfo key, boolean defaultValue)
  {
    Boolean defaultObject = defaultValue ? Boolean.TRUE : Boolean.FALSE;
    return ((Boolean) get (key, defaultObject)).booleanValue();
  }

  public boolean getBoolean (OptionInfo key)
  {
    Object value = get (key, null);
    return value == null ? false : ((Boolean) value).booleanValue();
  }

  /** Set a list of options, remember the old value.
   * @param options is vector of triples, echo of which is consisting of:
   * a String option key;
   * an entry whose value is ignored and is used to store the old value; and
   * a new value for the option.
   */
  public void pushOptionValues (Vector options)
  {
    int len = options.size();
    for (int i = 0;  i < len;  i=i+3)
      {
	String key = (String) options.elementAt(i);
	Object oldValue = set(key, options.elementAt(i+2), null);
	options.setElementAt(oldValue, i+1);
      }
  }

  /** Restore a list of options, as set by pushOptionValues
   */
  public void popOptionValues (Vector options)
  {
    for (int i = options.size();  (i -= 3) >= 0;  )
      {
	String key = (String) options.elementAt(i);
	Object oldValue = options.elementAt(i+1);
	options.setElementAt(null, i+1);
	reset(key, oldValue);
      }
  }

  /** Return the list of option keys.
   */
  public ArrayList<String> keys ()
  {
    ArrayList<String> allKeys = new  ArrayList<String>();
    for (Options options = this;  options != null;  options = options.previous)
      {
	if (options.infoTable != null)
	  {
	    for (String k : options.infoTable.keySet())
              {
                if (! allKeys.contains(k))
		  allKeys.add(k);
	      }
	  }
      }
    return allKeys;
  }

  public String getDoc(String key)
  {
    OptionInfo info = getInfo(key);
    if (key == null)
      return null;
    return info.documentation;
  }

  public static final class OptionInfo
  {
    OptionInfo next;
    String key;
    int kind;
    String documentation;
    Object defaultValue;
  }
}
