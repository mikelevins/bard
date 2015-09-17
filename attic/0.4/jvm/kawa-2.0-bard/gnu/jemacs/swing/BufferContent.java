package gnu.jemacs.swing;
import gnu.jemacs.buffer.*;
import javax.swing.text.*;
import javax.swing.undo.*;
import gnu.lists.*;

/** A Content class that supports Emacs-style Markers.
 * The standard GapContent is close, but unfortunately it only
 * supports inserting *before* marks, which is not the Emacs default.
 * This provides a superset of the Position functionality (except for undo).
 */

public class BufferContent extends gnu.kawa.swingviews.SwingContent
{
  public BufferContent()
  {
    this(100);
  }

  public BufferContent(int initialSize)
  {
    super(initialSize);
  }

  public static int indexOf(char[] buffer, int start, int limit, char ch)
  {
    for (int i = start; i < limit; i++)
      {
        if (buffer[i] == ch)
          return i;
      }
    return -1;
  }

  /** Search for the last occurrence of a character
   * in buffer[limit..start]. */
  public static int lastIndexOf(char[] buffer, int start, int limit, char ch)
  {
    for (int i = start; i >= limit; i--)
      {
        if (buffer[i] == ch)
          return i;
      }
    return -1;
  }

  /** Search in BUF for COUNT instances of the character TARGET between START and END.
   * If COUNT is positive, search forwards; END must be >= START.
   * If COUNT is negative, search backwards for the -COUNTth instance;
   *   END must be <= START.
   * If COUNT is zero, do anything you please; run rogue, for all I care.
   * START and END are both 0-origin.
   *
   * If we find COUNT instances, SHORTAGE is zero, and return the
   * (0-origin) position after the COUNTth match.  Note that for reverse motion
   * this is not the same as the usual convention for Emacs motion commands.

   * If we don't find COUNT instances before reaching END, set SHORTAGE
   * to the number of TARGETs left unfound, and return (shortage<<32|END).
   * @return (SHORTAGE<<32|POS)
  */

  public final long scan(char target, int start, int end,
                         int count, boolean allowQuit)
  {
    CharBuffer b = buffer;
    int limit = end > b.gapStart ? end + b.gapEnd - b.gapStart : end;
    if (start > b.gapStart)
      start += b.gapEnd - b.gapStart;
    if (count > 0)
      {
        while (start < limit && count > 0)
          {
            int ceil;
            if (start == b.gapStart)
              start = b.gapEnd;
            if (start < b.gapStart && limit > b.gapStart)
              ceil = b.gapStart;
            else
              {
                ceil = limit;
              }
            if (allowQuit)
              {
                if (ceil - start > 5000)
                  ceil = start + 5000;
                Signal.checkQuit();
              }
            int i = indexOf(b.getArray(), start, ceil, target);
            if (i >= 0)
              {
                count--;
                start = i + 1;
              }
            else
              start = ceil;
          }
        if (start > b.gapEnd)
          start -= b.gapEnd - b.gapStart;
        return ((long) count << 32) | start;
      }
    else
      {
        while (start > limit && count < 0)
          {
            if (start == b.gapEnd)
              start = b.gapStart;
            int floor;
            if (start <= b.gapStart || limit >= b.gapEnd)
              floor = limit;
            else
              floor = b.gapEnd;
            if (allowQuit)
              {
                if (start - floor > 5000)
                  floor = start - 5000;
                Signal.checkQuit();
              }
            int i = lastIndexOf(b.getArray(), start - 1, floor, target);
            if (i >= 0)
              {
                count++;
                start = i;
              }
            else
              start = floor;
          }
  
        if (start >= b.gapEnd)
          start -= b.gapEnd - b.gapStart;
        if (count != 0)
          return ((long) (- count) << 32) | start;
        else
          {
            // We found the character we were looking for; we have to return
            // the position *after* it due to the strange way that the return
            // value is defined.
            return start + 1;
          }
      }
  }
}
