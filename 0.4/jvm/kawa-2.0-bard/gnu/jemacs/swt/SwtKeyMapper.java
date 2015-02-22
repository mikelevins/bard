package gnu.jemacs.swt;

import gnu.jemacs.buffer.EKeymap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;

/**
 * A utility class to do mapping between swt- and awt keyevents
 * 
 */
public class SwtKeyMapper
{
  public static int swtKey2EKey(KeyEvent swtEvent)
  {
    int mods = transformStatemask(swtEvent);
   
    if ((swtEvent.keyCode & ~SWT.MODIFIER_MASK) == 0) // It's a modifier key
    {
      return -1;
    }
    else if ((swtEvent.keyCode & SWT.KEYCODE_BIT) == 0) // It's a unicode
    {
      char ch = swtEvent.character;
      if (mods == EKeymap.SHIFT_MASK && ch != swtEvent.keyCode)
      {
        // ch already 'contains' shift so no need to flag it.
        mods = 0;
      }
      if (0 < ch  && ch < 0x1A)
      {
        mods |= EKeymap.PRESSED;
        if ((mods & EKeymap.CTRL_MASK) != 0)
        	ch = (char) (ch + 'A' - 1);
      }
      return ch | (mods << 16);
    }
    else // It's a key code.
    {
      switch (swtEvent.keyCode) 
      {
      case SWT.F1:            return java.awt.event.KeyEvent.VK_F1 | (EKeymap.PRESSED << 16);
      case SWT.F2:            return java.awt.event.KeyEvent.VK_F2 | (EKeymap.PRESSED << 16);
      case SWT.F3:            return java.awt.event.KeyEvent.VK_F3 | (EKeymap.PRESSED << 16);
      case SWT.F4:            return java.awt.event.KeyEvent.VK_F4 | (EKeymap.PRESSED << 16);
      case SWT.F5:            return java.awt.event.KeyEvent.VK_F5 | (EKeymap.PRESSED << 16);
      case SWT.F6:            return java.awt.event.KeyEvent.VK_F6 | (EKeymap.PRESSED << 16);
      case SWT.F7:            return java.awt.event.KeyEvent.VK_F7 | (EKeymap.PRESSED << 16);
      case SWT.F8:            return java.awt.event.KeyEvent.VK_F8 | (EKeymap.PRESSED << 16);
      case SWT.F9:            return java.awt.event.KeyEvent.VK_F9 | (EKeymap.PRESSED << 16);
      case SWT.F10:           return java.awt.event.KeyEvent.VK_F10 | (EKeymap.PRESSED << 16);
      case SWT.F11:           return java.awt.event.KeyEvent.VK_F11 | (EKeymap.PRESSED << 16);
      case SWT.F12:           return java.awt.event.KeyEvent.VK_F12 | (EKeymap.PRESSED << 16);
      case SWT.F13:           return java.awt.event.KeyEvent.VK_F13 | (EKeymap.PRESSED << 16);
      case SWT.F14:           return java.awt.event.KeyEvent.VK_F14 | (EKeymap.PRESSED << 16);
      case SWT.F15:           return java.awt.event.KeyEvent.VK_F15 | (EKeymap.PRESSED << 16);
      case SWT.HELP:          return java.awt.event.KeyEvent.VK_HELP | (EKeymap.PRESSED << 16);
      case SWT.ARROW_LEFT:    return java.awt.event.KeyEvent.VK_LEFT | (EKeymap.PRESSED << 16);
      case SWT.ARROW_RIGHT:   return java.awt.event.KeyEvent.VK_RIGHT | (EKeymap.PRESSED << 16);
      case SWT.ARROW_UP:      return java.awt.event.KeyEvent.VK_UP | (EKeymap.PRESSED << 16);
      case SWT.ARROW_DOWN:    return java.awt.event.KeyEvent.VK_DOWN | (EKeymap.PRESSED << 16);
      default:                return 0; 
      }
    }
  }

  public static int transformStatemask(KeyEvent swtEvent)
  {
    return ((swtEvent.stateMask & SWT.ALT)     != 0 ? EKeymap.META_MASK  : 0) |
	   ((swtEvent.stateMask & SWT.COMMAND) != 0 ? EKeymap.ALT_MASK   : 0) |
	   ((swtEvent.stateMask & SWT.SHIFT)   != 0 ? EKeymap.SHIFT_MASK : 0) |
	   ((swtEvent.stateMask & SWT.CONTROL) != 0 ? EKeymap.CTRL_MASK  : 0);
  }
}
