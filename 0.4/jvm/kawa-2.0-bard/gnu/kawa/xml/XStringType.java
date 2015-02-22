// Copyright (c) 2007  Per M.A. Bothner.
// This is free software;  for specifics see ../../../COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.xml.*;
/* #ifdef use:java.util.regex */
import java.util.regex.Pattern;
import java.util.regex.Matcher;
/* #endif */

public class XStringType extends XDataType
{
  /* #ifdef use:java.util.regex */
  Pattern pattern;
  /* #endif */

  static ClassType XStringType = ClassType.make("gnu.kawa.xml.XString");

  public XStringType (String name, XDataType base, int typeCode,
                      String pattern)
  {
    super(name, XStringType, typeCode);
    baseType = base;
    /* #ifdef use:java.util.regex */
    if (pattern != null)
      this.pattern = Pattern.compile(pattern);
    /* #endif */
  }

  public static final XStringType normalizedStringType =
    new XStringType ("normalizedString", stringType,
                     NORMALIZED_STRING_TYPE_CODE, null);

  public static final XStringType tokenType =
    new XStringType ("token", normalizedStringType,
                     TOKEN_TYPE_CODE, null);

  public static final XStringType languageType =
    new XStringType ("language", tokenType,
                     LANGUAGE_TYPE_CODE, "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*");

  public static final XStringType NMTOKENType =
    new XStringType ("NMTOKEN", tokenType,
                     NMTOKEN_TYPE_CODE, "\\c+"); // FIXME check pattern

  public static final XStringType NameType =
    new XStringType ("Name", tokenType,
                     NAME_TYPE_CODE, null);

  public static final XStringType NCNameType =
    new XStringType ("NCName", NameType,
                     NCNAME_TYPE_CODE, null);

  public static final XStringType IDType =
    new XStringType ("ID", NCNameType,
                     ID_TYPE_CODE, null);

  public static final XStringType IDREFType =
    new XStringType ("IDREF", NCNameType,
                     IDREF_TYPE_CODE, null);

  public static final XStringType ENTITYType =
    new XStringType ("ENTITY", NCNameType,
                     ENTITY_TYPE_CODE, null);

  public boolean isInstance (Object obj)
  {
    if (! (obj instanceof XString))
      return false;
    /*
    if (this == stringType)
      return true;
    */
    XDataType objType = ((XString) obj).getStringType();
    while (objType != null)
      {
        if (objType == this)
          return true;
        objType = objType.baseType;
      }
    return false;
  }

  /** Check if the String matches the restrictions on this type.
   * Assumes any normalization has been done.
   * @return null on success or an error message otherwise.
   */
  public String matches (String value)
  {
    boolean status;
    switch (typeCode)
      {
      /* #ifndef use:java.util.regex */
      // case LANGUAGE_TYPE_CODE:
      /* #endif */
      case NORMALIZED_STRING_TYPE_CODE:
      case TOKEN_TYPE_CODE:
        // Assumes that TextUtils.replaceWhitespace returns the original
        // string if it was original normalized.
        // This is suboptimal, but the extra cost is minor when the string
        // is already normalized, which presumably is the common case.
        boolean collapse = typeCode != NORMALIZED_STRING_TYPE_CODE;
        status = value == TextUtils.replaceWhitespace(value, collapse);
        break;
      case NAME_TYPE_CODE:
        status = XName.isName(value);
        break;
      case NCNAME_TYPE_CODE:
      case ID_TYPE_CODE:
      case IDREF_TYPE_CODE:
      case ENTITY_TYPE_CODE:
        status = XName.isNCName(value);
        break;
      case NMTOKEN_TYPE_CODE:
        status = XName.isNmToken(value);
        break;
      default:
        /* #ifdef use:java.util.regex */
        status = pattern == null || pattern.matcher(value).matches();
        /* #else */
        // status = true;
        /* #endif */
      }
    // If we haven't returned a more specific error message:
    return status ? null : "not a valid XML "+getName();
  }

  public Object valueOf (String value)
  {
    value = TextUtils.replaceWhitespace(value, this != normalizedStringType);
    String err = matches(value);
    if (err != null) // we're not using err yet.  FIXME.
      throw new ClassCastException("cannot cast "+value+" to "+name);
    return new XString(value, this);
  }

  public Object cast (Object value)
  {
    if (value instanceof XString)
      {
        XString xvalue = (XString) value;
        if (xvalue.getStringType() == this)
          return xvalue;
      }
    return valueOf((String) stringType.cast(value));
  }

  public static XString makeNCName (String value)
  {
    return (XString) NCNameType.valueOf(value);
  }
}
