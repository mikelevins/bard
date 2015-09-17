package gnu.jemacs.swt;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.StyledTextContent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;

/**
 * Class to do the graphical representation of an JEmacs window.
 * It contains a text area (styledtext) for displaying the buffer, and a
 * label displaying the modeline.
 * 
 * @author Christian Surlykke
 */
public class SwtWindowWidget extends Composite
{
  private StyledText styledText;
  private StyledText modeline;
  
  public SwtWindowWidget(Composite parent, 
                         StyledTextContent styledTextContent, 
                         int firstVisibleLine)
  {
   super(parent, 0);
   
   setBackground(new Color(getDisplay(), 0, 0, 0));
   GridLayout layout = new GridLayout();
   layout.numColumns = 1;
   layout.horizontalSpacing = layout.verticalSpacing = 2;
   layout.marginWidth = layout.marginHeight = 2;
   setLayout(layout);
   
   styledText = new StyledText(this,  SWT.H_SCROLL | SWT.V_SCROLL);
   styledText.setContent(styledTextContent);
   styledText.setTopIndex(firstVisibleLine);
   styledText.setLayoutData(new GridData(GridData.FILL_BOTH));
  
   modeline = new StyledText(this, SWT.SINGLE | SWT.READ_ONLY);
   modeline.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
  }

  public StyledText getStyledText() {
    return this.styledText;
  }

  public StyledText getModeline() {
    return this.modeline;
  }
}
