// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for specifics see ../../../COPYING.

package gnu.kawa.xml;
import java.util.Calendar;
import gnu.bytecode.*;
import gnu.math.*;
import java.util.GregorianCalendar;
import java.util.TimeZone;

public class XTimeType extends XDataType
{
  static ClassType typeDateTime = ClassType.make("gnu.math.DateTime");

  public static final XTimeType dateTimeType =
    new XTimeType("dateTime", DATE_TIME_TYPE_CODE);

  public static final XTimeType dateType =
    new XTimeType("date", DATE_TYPE_CODE);
  public static final XTimeType timeType =
    new XTimeType("time", TIME_TYPE_CODE);
  public static final XTimeType gYearMonthType =
    new XTimeType("gYearMonth", G_YEAR_MONTH_TYPE_CODE);
  public static final XTimeType gYearType =
    new XTimeType("gYear", G_YEAR_TYPE_CODE);
  public static final XTimeType gMonthType =
    new XTimeType("gMonth", G_MONTH_TYPE_CODE);
  public static final XTimeType gMonthDayType =
    new XTimeType("gMonthDay", G_MONTH_DAY_TYPE_CODE);
  public static final XTimeType gDayType =
    new XTimeType("gDay", G_DAY_TYPE_CODE);

  XTimeType(String name, int code)
  {
    super((Object) name/*FIXME*/, typeDateTime, code);
  }

  static int components (int typeCode)
  {
    switch (typeCode)
      {
      case DATE_TIME_TYPE_CODE:
        return DateTime.DATE_MASK|DateTime.TIME_MASK;
      case DATE_TYPE_CODE:
        return DateTime.DATE_MASK;
      case TIME_TYPE_CODE:
        return DateTime.TIME_MASK;
      case G_YEAR_MONTH_TYPE_CODE:
        return DateTime.YEAR_MASK|DateTime.MONTH_MASK;
      case G_YEAR_TYPE_CODE:
        return DateTime.YEAR_MASK;
      case G_MONTH_DAY_TYPE_CODE:
        return DateTime.MONTH_MASK|DateTime.DAY_MASK;
      case G_DAY_TYPE_CODE:
        return DateTime.DAY_MASK;
      case G_MONTH_TYPE_CODE:
        return DateTime.MONTH_MASK;
      case DURATION_TYPE_CODE:
        return DateTime.DATE_MASK|DateTime.TIME_MASK;
      case YEAR_MONTH_DURATION_TYPE_CODE:
        return DateTime.YEAR_MASK|DateTime.MONTH_MASK;
      case DAY_TIME_DURATION_TYPE_CODE:
        return DateTime.DAY_MASK|DateTime.TIME_MASK;
      default:
        return 0;
      }
  }

  /*
  RuntimeException badFormat(String str)
  {
    return new NumberFormatException("Unrecognized "+name+": '"+str+'\'');
  }
  */

  /** Return the current date or time in this type. */
  public DateTime now ()
  {
    return new DateTime(XTimeType.components(typeCode)|DateTime.TIMEZONE_MASK,
                        (GregorianCalendar) Calendar.getInstance(fixedTimeZone()));

  }

  private static TimeZone fixedTimeZone;
  private static synchronized TimeZone fixedTimeZone ()
  {
    if (fixedTimeZone == null)
      {
        int offset = TimeZone.getDefault().getRawOffset() / 60000;
        fixedTimeZone = DateTime.minutesToTimeZone(offset);
      }
    return fixedTimeZone;
  }


  public static DateTime parseDateTime (String value, int mask)
  {
    DateTime time = DateTime.parse(value, mask);
    // Replace an implicit timezone by by one that is fixed wrt DST.
    // I.e. TimeZone.getDefault() may be a timezone whose offset varies
    // depending upon whether Daylight Savings Time is in effect.
    // Replace it by one with a fixed offset.  This is required for
    // XQuery semantics, though I believe it is wrong.
    if (time.isZoneUnspecified())
      time.setTimeZone(fixedTimeZone());
    return time;
  }

  public Object valueOf (String value)
  {
    return parseDateTime(value, XTimeType.components(typeCode));
  }

  public boolean isInstance (Object obj)
  {
    if (! (obj instanceof DateTime))
      return false;
    int thisMask = components(this.typeCode);
    int objMask = ((DateTime) obj).components();
    //return (thisMask & ~objMask) == 0;
    return thisMask == objMask;
  }
}
