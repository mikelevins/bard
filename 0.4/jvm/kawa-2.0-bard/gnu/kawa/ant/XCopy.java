package gnu.kawa.ant;

import org.apache.tools.ant.taskdefs.Copy;

import org.apache.tools.ant.BuildException;

public class XCopy extends Copy
{
   private boolean dataOnly = false;
   
   public XCopy()
   {
   }
   
   public LineStripperSet createLineStripperSet()
   {
      LineStripperSet stripper = new LineStripperSet();
      getFilterSets().addElement(stripper);
      return stripper;
   }
   
   public LineCommenterSet createLineCommenterSet()
   {
      LineCommenterSet stripper = new LineCommenterSet();
      getFilterSets().addElement(stripper);
      return stripper;
   }
   
   public void setDataOnly(final boolean f)
   {
      dataOnly = f;
   }
   
   public boolean isDataOnly()
   {
      return dataOnly;
   }  

    /**
     * Performs the copy operation.
     * Support dataOnly attribute which makes this task a no-op if true.
     */
    public void execute() 
        throws BuildException 
    {
        if (isDataOnly()) 
            return;
        
        super.execute();
    }  
}
