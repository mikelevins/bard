// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.jemacs.buffer;
import gnu.lists.*;
import gnu.math.IntNum;
import gnu.text.Char;
import gnu.mapping.*;
import java.awt.event.KeyEvent;

public class EKeymap extends gnu.kawa.util.RangeTable
implements gnu.mapping.Named
{
  public static final EKeymap[] empty = new EKeymap[0];
  EKeymap[] parents = empty;

  public static int PRESSED = 0x100;
  public static int RELEASED = 0x200;

  Object defaultBinding;

  String name;

  /** The magic key that indicates a (Emacs) meta prefix.
   * I.e. we saw either an Escape or a meta modifier. */
  public static final int  metaKey = '\033';

  /** The Emacs global map. */
  public static EKeymap globalKeymap = new EKeymap();

  /** The Emacs global escape (meta) map. */
  public static EKeymap metaKeymap = new EKeymap("ESC-map");

  static
  {
    globalKeymap.setAction(metaKey, metaKeymap);
  }

  public static final int CTRL_MASK = java.awt.event.InputEvent.CTRL_MASK;
  public static final int SHIFT_MASK = java.awt.event.InputEvent.SHIFT_MASK;
  // Note ALT_MASK and META_MASK are shifted!
  public static final int META_MASK = java.awt.event.InputEvent.ALT_MASK;
  public static final int ALT_MASK = java.awt.event.InputEvent.META_MASK;

  public EKeymap (String name)
  {
    this.name = name;
  }

  public EKeymap ()
  {
  }

  public String getName()
  {
    return name;
  }

  public Object getSymbol ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public final Object getDefaultBinding ()
  {
    return defaultBinding;
  }

  public void setDefaultBinding (Object value)
  {
    defaultBinding = value;
  }

  public static int getModifiers (int code)
  {
    return (code >> 16) & 0xFF;
  }

  public EKeymap[] getParents ()
  {
    return parents;
  }

  public void setParents (EKeymap[] parents)
  {
    this.parents = parents;
  }

  public void setParent (EKeymap parent)
  {
    if (parent == null)
      this.parents = empty;
    else
      this.parents = new EKeymap[] { parent };
  }

  public EKeymap getParent ()
  {
    int num = parents.length;
    if (num == 0)
      return null;
    if (num == 1)
      return parents[0];
    throw new Error("multiple parents - set getParents, not getParent");
  }

  public void setAction(int key, Object command)
  {
    set(key, command);
  }

  public Object get (int key, int modifiers, boolean acceptDefaults)
  {
    return get (key | (modifiers << 16), acceptDefaults);
  }

  protected Object get (int key, boolean acceptDefaults)
  {
    Object value = super.lookup(key, null);
    if (value != null)
      return value;
    if (acceptDefaults && defaultBinding != null)
      return defaultBinding;
    int plen = parents.length;
    for (int i = 0;  i <plen;  i++)
      {
	value = parents[i].get(key, acceptDefaults);
	if (value != null)
	  return value;
      }
    return null;
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer(40);
    sbuf.append("#<keymap ");
    if (name != null)
      {
	sbuf.append(name);
	sbuf.append(' ');
      }
    /*
    sbuf.append("size ");
    sbuf.append('?');
    */
    sbuf.append("0x");
    sbuf.append(Integer.toHexString(System.identityHashCode(this)));
    sbuf.append('>');
    return sbuf.toString();
  }

  /** Get or create keymap associate with a prefix key in a given keymap. */
  public EKeymap definePrefix(int key)
  {
    Object command = get(key, false);
    Object cc = command;
    Object x;
    if (command == null)
      {
        EKeymap next = new EKeymap(null);
        set(key, next);
        return next;
      }
    command = Command.resolveSymbol(command);
    if (command instanceof EKeymap)
      return (EKeymap) command;
    else
      {
        throw new Error("prefix command cannot override exiting action: "
			+ cc+ " : "+cc.getClass()+" -> "+command+" key "+key+"="+toString(key)+" in "+this);
      }
  }

  public void defineKey(Object keySpec, Object binding)
  {
    EKeymap keymap = this;
    if ((keySpec instanceof Sequence
         /* #ifdef use:java.lang.CharSequence */
         || keySpec instanceof CharSequence
         /* #else */
         // || keySpec instanceof String
         /* #endif */
         ) && ! (keySpec instanceof LList))
      {
        // Handle key sequence.
	Sequence seq;
        int len;
        String str;
        /* #ifdef use:java.lang.CharSequence */
        boolean hackMeta = keySpec instanceof CharSequence;
        /* #else */
        // boolean hackMeta = keySpec instanceof String || keySpec instanceof CharSeq;
        /* #endif */
        if (keySpec instanceof Sequence)
          {
            seq = (Sequence) keySpec;
            len = seq.size();
            str = null;
          }
        else
          {
            seq = null;
            str = keySpec.toString();
            len = str.length();
          }
        for (int i = 0;  i < len; )
          {
            Object keyValue = (seq != null ? seq.get(i)
                               : Convert.toObject(str.charAt(i)));
            boolean sawMeta = false;
            i++;
	    int key = asKeyStroke(keyValue);
	    if (key == 0)
	      throw new Error("unknown keyspec: "+keyValue);
	    if (hackMeta && key > 127 && key <= 255)
	      {
		sawMeta = true;
		key = key - 128;
	      }
	    if ((getModifiers(key) & META_MASK) != 0)
	      {
		key = stripMeta(key);
		sawMeta = true;
	      }
            if (sawMeta)
              keymap = keymap.definePrefix(metaKey);
            if (i < len)
              keymap = keymap.definePrefix(key);
            else
              keymap.defineKey(key, binding);
          }
      }
    else
      {
        // Handle single key.
	int key = asKeyStroke(keySpec);
	if (key == 0)
	  throw new Error("unknown keyspec: "+keySpec);
        defineKey(key, binding);
      }
  }

  public void defineKey(int key, Object binding)
  {
    boolean sawMeta = false;
    if ((getModifiers(key) & META_MASK) != 0)
      {
	key = stripMeta(key);
	sawMeta = true;
      }
    EKeymap keymap = this;
    if (sawMeta)
      keymap = keymap.definePrefix(metaKey);
    keymap.setAction(key, binding);
  }

  public static int asKeyStroke(char ch, int mods)
  {
    if (mods == SHIFT_MASK && Character.isLetter(ch))
      {
	ch = Character.toUpperCase(ch);
	mods = 0;
      }
    if (ch < ' ')
      {
        mods |= CTRL_MASK|PRESSED;
        ch = ch == '\0' ? ' ' : (char) ('@' + (ch & 31));
      }
    return ch | (mods << 16);
  }

  /** Map an Emacs key name to one of the KeyEVent VK_XXX codes.
   * Returns VK_UNDEFINED if the name isn't recognized.
   */
  public static int getKeyForName (String name)
  {
    name = name.toLowerCase();
    int len = name.length();
    if (len == 0)
      return KeyEvent.VK_UNDEFINED;
    char c0 = name.charAt(0);
    if (len == 1)
      return c0;
    switch (c0)
      { 
      case 'b':
	if (name == "backspace")       return KeyEvent.VK_BACK_SPACE;
	break;
      case 'd':
	if (name == "delete")          return KeyEvent.VK_DELETE;
	if (name == "down")            return KeyEvent.VK_DOWN;
	break;
      case 'e':
	if (name == "enter")           return KeyEvent.VK_ENTER;
	break;
      case 'f':
	if (len == 2)
	  {
	    char c1 = name.charAt(1);
	    if (c1 > '0' && c1 <= '9')
	      return KeyEvent.VK_F1 + c1 - '1';
	  }
	else if (len == 3 && name.charAt(0) == 'f')
	  {
	    int c1 = name.charAt(1);
	    int c2 = name.charAt(2);
	    if (c1 > '0' && c1 <= '9'
		&& c2 > '0' && c2 <= '9'
		&& (c1 = (c1 - '0') * 10 + (c2 - '0')) <= 24)
	      {
		if (c1 <= 12)
		  return KeyEvent.VK_F10 + c2 - '0';
		else
		  return KeyEvent.VK_F13 + c1 - 13;
	      }
	  }
	break;
      case 'h':
	if (name == "help")            return KeyEvent.VK_HELP;
	break;
      case 'k':
	if (name == "kp-left")         return KeyEvent.VK_KP_LEFT;
	else if (name == "kp-right")   return KeyEvent.VK_KP_RIGHT;
	else if (name == "kp-up")      return KeyEvent.VK_KP_UP;
	else if (name == "kp-down")    return KeyEvent.VK_KP_DOWN;
	else if (name == "kp-delete")  return KeyEvent.VK_DELETE;
	break;
      case 'l':
	if (name == "left")            return KeyEvent.VK_LEFT;
	break;
      case 'n':
	if (name == "next")            return KeyEvent.VK_PAGE_DOWN;
	break;
      case 'p':
	if (name == "prior")           return KeyEvent.VK_PAGE_UP;
	break;
      case 'r':
	if (name == "enter")           return KeyEvent.VK_ENTER;
	if (name == "return")          return '\r';
	if (name == "right")           return KeyEvent.VK_RIGHT;
	break;
      case 't':
	if (name == "tab")             return KeyEvent.VK_TAB;
	break;
      case 'u':
	if (name == "up")              return KeyEvent.VK_UP;
	break;
      }
    return KeyEvent.VK_UNDEFINED;
  }

  public static int asKeyStroke(Object key)
  {
    int m = 0;
    while (key instanceof Pair)
      {
	Pair pair = (Pair) key;
	if (pair.getCdr() == LList.Empty)
	  key = pair.getCar();
	else
	  {
	    Object car = pair.getCar();
	    if (car instanceof Symbol)
	      car = ((Symbol) car).getName();
	    if (car == "control")
	      m |= CTRL_MASK;
	    if (car == "meta")
	      m |= META_MASK;
	    if (car == "shift")
	      m |= SHIFT_MASK;
	    if (car == "alt")
	      m |= ALT_MASK;
	    key = pair.getCdr();
	  }
      }
    if (key instanceof Char)
      {
	return asKeyStroke(((Char) key).charValue(), m);
      }
    if (key instanceof IntNum)
      {
	return asKeyStroke((char) ((IntNum) key).intValue(), m);
      }
    if (key instanceof String || key instanceof Symbol)
      {
	String name = key instanceof String ? (String) key
	  : ((Symbol) key).getName();
	if (name.length() == 1)
	  {
	    char ch = name.charAt(0);
	    if (m == 0)
	      return asKeyStroke((char) ch, 0);
	    else
	      {
		ch = Character.toUpperCase(ch);
		return asKeyStroke(ch, m);
	      }
	  }
	int code = getKeyForName(name);
	if (code == KeyEvent.VK_UNDEFINED)
	  throw new Error("unknown key-name: "+name);
	return code | ((m|PRESSED) << 16);
      }
    return 0; // FIXME toInt((KeyStroke) key);
  }

  public static int stripMeta(int key)
  {
    int mods = getModifiers(key);
    if ((mods & META_MASK) == 0)
      return key;
    mods &= ~ META_MASK;
    int code = key & 0xFFFF;
    boolean onRelease = (key & (RELEASED << 16)) != 0;
    if ((mods & ~SHIFT_MASK) != 0 || onRelease
        || code > 127 || code < ' ')
      return code | ((mods|RELEASED) << 16);
    else
      {
        if (code >= 'A' && code <= 'Z'&& mods != SHIFT_MASK)
          code = code + 'a' - 'A';
        return code;
      }
  }

  /**
   * True for a KeyStroke if the default action should be to ignore it.
   * For example, pressing a shift key should not be an action!
   * We also have the complication that both KEY-PRESSED and KEY_TYPED
   * events and we typically want to ignore one but not both.
   * (If both are handled, we have problems with default actions, as
   * well as when to abort a prefix sequence.  Swing does not have
   * this problem because it does not have prefix sequences and hence state.)
   */
  public static boolean ignorable (int key)
  {
    if ((key & (RELEASED << 16)) != 0)
      return true;
    int mods = getModifiers(key);
    // If there are no modifiers, and it's a normal non-control character,
    // we prefer the KEY_TYPED (keyChar) event.
    // Otherwise, we prefer the KEY_PRESSED (keyCode) event.
    int code = key & 0xFFFF;
    if ((key & (PRESSED << 16)) == 0)
      { // It's a KEY_TYPED (keyChar) event.
        char ch = (char) key;
        return (mods & ~SHIFT_MASK) != 0 || (ch < ' ' && ch != metaKey) || ch >= 127;
      }
    else
      { // It's a KEY_PRESSED (keyCODE) event.
        // Basically, in the case of KEY_PRESSED events that will
        // map without loss of information into normal KEY_TYPED events,
        // we prefer the KEY_TYPED events (as they don't depend on the
        // keyboard layout).
	if (code == KeyEvent.VK_CONTROL || code == KeyEvent.VK_SHIFT
	    || code == KeyEvent.VK_ALT || code == KeyEvent.VK_META
            || code == KeyEvent.VK_ESCAPE)
	  return true;
        return (mods & ~SHIFT_MASK) == 0
	  && code >= KeyEvent.VK_SPACE && code < KeyEvent.VK_DELETE;
      }
  }

  /*
  public static String toString(KeyStroke key)
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append('[');
    char ch = key.getKeyChar();
    if (ch != '\0')
      {
        sbuf.append("char:'");
        gnu.jemacs.lang.ELisp.readableChar(ch, sbuf, true);
        sbuf.append("'");
      }
    int code = key.getKeyCode();
    if (code != 0)
      {
        sbuf.append("code:");
        sbuf.append(code);
      }
    int mods = key.getModifiers();
    if (mods != 0)
      {
        sbuf.append(" mods:");
        sbuf.append(mods);
      }
    if (key.isOnKeyRelease())
      sbuf.append(" release");
    sbuf.append(']');
    return sbuf.toString();
  }
  */

  public static String toString(int code)
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append('[');
    if (((code >> 16) & (PRESSED|RELEASED)) == 0)
      {
        sbuf.append("char:'");
        gnu.jemacs.lang.ELisp.readableChar((char) code, sbuf, true);
        sbuf.append("'");
      }
    else
      {
        sbuf.append("code:");
        sbuf.append(code & 0xFFFF);
      }
    int mods = (code >> 16) & 0xff;
    if (mods != 0)
      {
        sbuf.append(" mods:");
        sbuf.append(mods);
      }
    if ((code & (RELEASED << 16)) != 0)
      sbuf.append(" release");
    sbuf.append(']');
    return sbuf.toString();
  }

  public Object
  lookupKey(Sequence keys, boolean acceptDefaults)
  {
    int nKeys = keys.size();
    int[] prefixKeys = new int[nKeys];
    java.util.Enumeration enumKeys = keys.elements();
    for (int i = 0;  enumKeys.hasMoreElements();  i++)
      {
        prefixKeys[i] = asKeyStroke(enumKeys.nextElement());
      }
    return lookupKey(prefixKeys, nKeys, 0, acceptDefaults);
  }

  public Object lookupKey(int[] prefixKeys, int nPrefix,
			  int key, boolean acceptDefaults)
  {
    EKeymap keymap = this;
    int nKeys = nPrefix + (key != 0 ? 1 : 0);
    boolean pendingMeta = false; // FIXME this is never set
    if (nKeys == 0)
     throw new Error("no keys");
    for (int i = 0;  ; )
      {
        int key_i = i == nPrefix ? key : prefixKeys[i];

        if (pendingMeta)
          key_i |= ((META_MASK|PRESSED) << 16);
        Object command = keymap.get(key_i, false);
        Object metaCommand;
        if (command == null
            && (getModifiers(key_i) & META_MASK) != 0
            && (metaCommand = keymap.get(metaKey, false)) instanceof EKeymap)
          {
            command = ((EKeymap) metaCommand).get(stripMeta(key_i), false);
          }
        i++;
        if (command == null)
          {
            if (ignorable(key))
            {
              return EToolkit.getInstance().getIgnoreAction();
            }
            else
	      return keymap.getDefaultBinding();
          }
        if (i == nKeys)
          return command;
	Object comm;
	if (command instanceof String || command instanceof Symbol)
	  command = Command.resolveSymbol(command);
	if (command instanceof EKeymap)
	  keymap = (EKeymap) command;
	else
	  return null;
      }
  }

  /*
   * For debugging 
   */
  public static String show(int binary) 
  {
    StringBuffer sb = new StringBuffer(Integer.toBinaryString(binary));
    for (int i = 32 - sb.length() - 1; i >= 0; i--)
      sb.insert(0, '0');
    return sb.toString();
  }
}

