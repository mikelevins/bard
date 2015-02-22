package gnu.xquery.util;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.math.*;
import gnu.mapping.WrongType;
import gnu.mapping.Values;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.TimeZone;

public class TimeUtils
{
  static DateTime coerceToDateTime (String fun, Object value)
  {
    if (XTimeType.dateTimeType.isInstance(value))
      return (DateTime) value;
    if (value instanceof KNode || value instanceof UntypedAtomic)
      return XTimeType.parseDateTime(TextUtils.stringValue(value),
                                     DateTime.DATE_MASK|DateTime.TIME_MASK);
    throw new WrongType(fun, 1, value, "xs:dateTime");
  }

  static DateTime coerceToDate (String fun, Object value)
  {
    if (XTimeType.dateType.isInstance(value))
      return (DateTime) value;
    if (value instanceof KNode || value instanceof UntypedAtomic)
      return XTimeType.parseDateTime(TextUtils.stringValue(value),
                                     DateTime.DATE_MASK);
    throw new WrongType(fun, 1, value, "xs:date");
  }

  static DateTime coerceToTime (String fun, Object value)
  {
    if (XTimeType.timeType.isInstance(value))
      return (DateTime) value;
    if (value instanceof KNode || value instanceof UntypedAtomic)
      return XTimeType.parseDateTime(TextUtils.stringValue(value),
                                     DateTime.TIME_MASK);
    throw new WrongType(fun, 1, value, "xs:time");
  }

  static Duration coerceToDuration (String fun, Object value)
  {
    if (value instanceof Duration)
      return (Duration) value;
    throw new WrongType(fun, 1, value, "xs:duration");
  }

  static Object timeZoneFromXTime (DateTime time)
  {
    if (time.isZoneUnspecified())
      return Values.empty;
    return Duration.makeMinutes(time.getZoneMinutes());
  }

  static IntNum asInteger(int value)
  {
    return IntNum.make(value);
  }

  public static Object yearFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("year-from-dateTime", arg).getYear());
  }

  public static Object monthFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("month-from-dateTime", arg).getMonth());
  }

  public static Object dayFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("day-from-dateTime", arg).getDay());
  }

  public static Object hoursFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("hours-from-dateTime", arg).getHours());
  }

  public static Object minutesFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDateTime("minutes-from-dateTime", arg).getMinutes());
  }

  static Number getSeconds (DateTime date)
  {
    int seconds = date.getSecondsOnly();
    long nanos = date.getNanoSecondsOnly();
    if (nanos == 0)
      return IntNum.make(seconds);
    nanos += seconds * 1000000000L;
    return new BigDecimal(BigInteger.valueOf(nanos), 9);
  }

  public static Object secondsFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return getSeconds(coerceToDateTime("seconds-from-dateTime", arg));
  }

  public static Object timezoneFromDateTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return timeZoneFromXTime(coerceToDateTime("timezone-from-datetime", arg));
  }

  public static Object yearFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDate("year-from-date", arg).getYear());
  }

  public static Object monthFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDate("month-from-date", arg).getMonth());
  }

  public static Object dayFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDate("day-from-date", arg).getDay());
  }

  public static Object timezoneFromDate (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return timeZoneFromXTime(coerceToDate("timezone-from-date", arg));
  }

  public static Object hoursFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToTime("hours-from-time", arg).getHours());
  }

  public static Object minutesFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToTime("minutes-from-time", arg).getMinutes());
  }

  public static Object secondsFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return getSeconds(coerceToTime("seconds-from-time", arg));
  }

  public static Object timezoneFromTime (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return timeZoneFromXTime(coerceToTime("timezone-from-time", arg));
  }

  public static Object yearsFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("years-from-duration", arg).getYears());
  }

  public static Object monthsFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("months-from-duration", arg).getMonths());
  }

  public static Object daysFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("days-from-duration", arg).getDays());
  }

  public static Object hoursFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("hours-from-duration", arg).getHours());
  }

  public static Object minutesFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    return asInteger(coerceToDuration("minutes-from-duration", arg).getMinutes());
  }

  public static BigDecimal secondsBigDecimalFromDuration (long s, int n)
  {
    if (n == 0)
      return BigDecimal.valueOf(s);
    int scale = 9;
    // A simple way to make sure s * 1000000000L doesn't overflow:
    boolean huge = (int) s != s;
    long ns = huge ? n : s * 1000000000L + n;
    while (ns % 10 == 0)
      {
        ns = ns / 10;
        scale--;
      }
    BigDecimal dec = new BigDecimal(BigInteger.valueOf(ns), scale);
    if (huge)
      dec = BigDecimal.valueOf(s).add(dec);
    return dec;
  }

  public static Object secondsFromDuration (Object arg)
  {
    if (arg == null || arg == Values.empty) return arg;
    Duration d = coerceToDuration("seconds-from-duration", arg);
    int s = d.getSecondsOnly();
    int n = d.getNanoSecondsOnly();
    if (n == 0)
      return asInteger(s);
    else
      return secondsBigDecimalFromDuration(s, n);
  }

  public static Duration getImplicitTimezone ()
  {
    return Duration.makeMinutes(TimeZone.getDefault().getRawOffset() / 60000);
  }

  public static Object adjustDateTimeToTimezone (Object time)
  {
    return adjustDateTimeToTimezone(time, getImplicitTimezone());
  }

  public static Object adjustDateTimeToTimezone (Object time, Object zone)
  {
    if (time == Values.empty || time == null)
      return time;
    DateTime dtime = coerceToDateTime("adjust-dateTime-to-timezone", time);
    return adjustDateTimeToTimezoneRaw(dtime, zone);
  }

  public static Object adjustDateToTimezone (Object time)
  {
    return adjustDateToTimezone(time, getImplicitTimezone());
  }

  public static Object adjustDateToTimezone (Object time, Object zone)
  {
    if (time == Values.empty || time == null)
      return time;
    DateTime dtime = coerceToDate("adjust-date-to-timezone", time);
    return adjustDateTimeToTimezoneRaw(dtime, zone);
  }

  public static Object adjustTimeToTimezone (Object time)
  {
    return adjustTimeToTimezone(time, getImplicitTimezone());
  }

  public static Object adjustTimeToTimezone (Object time, Object zone)
  {
    if (time == Values.empty || time == null)
      return time;
    DateTime dtime = coerceToTime("adjust-time-to-timezone", time);
    return adjustDateTimeToTimezoneRaw(dtime, zone);
  }

  static Object adjustDateTimeToTimezoneRaw (DateTime dtime, Object zone)
  {
    if (zone == Values.empty || zone == null)
      return dtime.withZoneUnspecified();
    Duration d = (Duration) zone;
    if (d.getNanoSecondsOnly() != 0 || d.getSecondsOnly() != 0)
      throw new IllegalArgumentException("timezone offset with fractional minute");
    int delta = (int) d.getTotalMinutes();
    if (delta < -14 * 60 || delta > 14 * 60)
      throw new IllegalArgumentException("timezone offset out of range");
    return dtime.adjustTimezone(delta);
  }

  public static DateTime now ()
  {
    return gnu.kawa.xml.XTimeType.dateTimeType.now();
  }

  public static Object dateTime (Object arg1, Object arg2)
  {
    if (arg1 == null || arg1 == Values.empty) return arg1;
    if (arg2 == null || arg2 == Values.empty) return arg2;
    DateTime date = coerceToDate("dateTime", arg1);
    DateTime time = coerceToTime("dateTime", arg2);
    StringBuffer sbuf = new StringBuffer();
    date.toStringDate(sbuf);
    sbuf.append('T');
    time.toStringTime(sbuf);
    boolean hasZone1 = ! date.isZoneUnspecified();
    boolean hasZone2 = ! time.isZoneUnspecified();
    if (hasZone1 || hasZone2)
      {
        int zone1 = date.getZoneMinutes();
        int zone2 = time.getZoneMinutes();
        if (hasZone1 && hasZone2 && zone1 != zone2)
          throw new Error("dateTime: incompatible timezone in arguments");
        DateTime.toStringZone(hasZone1 ? zone1 : zone2, sbuf);
      }
    return (DateTime) XTimeType.dateTimeType.valueOf(sbuf.toString());
  }

  /* #ifdef JAVA2 */
  /* #ifdef JAVA5 */
  static final ThreadLocal<DateTime> currentDateTimeLocal =
    new ThreadLocal<DateTime>();
  /* #else */
  // static final ThreadLocal currentDateTimeLocal = new ThreadLocal();
  /* #endif */
  /* #else */
  // static DateTime currentDateTime;
  /* #endif */

  public static DateTime currentDateTime ()
  {
    /* #ifdef JAVA2 */
    DateTime current = (DateTime) currentDateTimeLocal.get();
    if (current == null)
      {
        current = now();
        currentDateTimeLocal.set(current);
      }
    /* #else */
    // DateTime current = currentDateTime;
    // if (current == null)
    //   {
    //     current = now();
    //     currentDateTime = current;
    //   }
    /* #endif */
    return current;
  }

  public static DateTime currentDate ()
  {
    return currentDateTime().cast(DateTime.DATE_MASK);
  }

  public static DateTime currentTime ()
  {
    return currentDateTime().cast(DateTime.TIME_MASK);
  }

  public static Object implicitTimezone ()
  {
    return timeZoneFromXTime(currentDateTime());
  }
}
