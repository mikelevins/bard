package gnu.kawa.ant;

import org.apache.tools.ant.types.FilterSet;

import org.apache.tools.ant.Project;

import java.util.Enumeration;
import java.util.Hashtable;

public class LineCommenterSet extends FilterSet
{
    // The replaceTokens method is stateful.
    // If this is not null, then we ignore all lines until we find that token.
    private String commentingUntil = null;
    private boolean commenting = false;
    
    public LineCommenterSet()
    {
    }
    
    /**
     * Individual filter component of filterset
     *
     * @author    Jim White
     * Created   2001-11-14.
     */
    public static class LineCommenter extends FilterSet.Filter
    {
        String first;
        String last;
        boolean comment;
        
        public LineCommenter()
        {
        }
        
        public void setFirst(final String s)
        {
            first = s;
        }
        
        public void setLast(final String s)
        {
            last = s;
        }
        
        public void setComment(final boolean f)
        {
        	comment = f;
        }
        
        public String getFirst()
        {
            return first;
        }
        
        public String getLast()
        {
            return last;
        }
        
        public boolean isComment()
        {
        	return comment;
        }
    }
    
    /**
     * Gets the filter hash of the FilterSet.
     *
     * @return   The hash of the tokens and values for quick lookup.
     */
    public Hashtable getCommenterHash() {
        final int filterSize = getFilters().size();
        final Hashtable stripperHash = new Hashtable(filterSize);
        for (final Enumeration e = getFilters().elements(); e.hasMoreElements();) {
           LineCommenter commenter = (LineCommenter) e.nextElement();
           stripperHash.put(commenter.getFirst(), commenter);
        }
        return stripperHash;
    }
    
    /**
     * Does replacement on the given string with token matching.
     * This uses the defined begintoken and endtoken values which default to @ for both.
     *
     * @param line  The line to process the tokens in.
     * @return      The string with the tokens replaced.
     */
    public String replaceTokens(String line) 
    {
        final String beginToken = getBeginToken();
        final String endToken = getEndToken();
        final int index = line.indexOf(beginToken);
        
        if (commentingUntil != null) {
            log( "Commenting until: " + beginToken + commentingUntil + endToken 
               , Project.MSG_VERBOSE );
        }
           
        try {
			if (index < 0) {
			   if (commentingUntil == null) {
				   return line;
			   }
			   
			   return comment(line);
			}
				
			final Hashtable tokens = getCommenterHash();
        
            final int endIndex = line.indexOf(endToken, index + beginToken.length() + 1 );

			if (endIndex < 1) {
			   if (commentingUntil == null) {
				   return line;
			   }
			   
			   return comment(line);
			}
            
			final String token = line.substring(index + beginToken.length(), endIndex );
			
			if (commentingUntil == null) {
				// We're not commenting, so look for a "first" token.
				if (tokens.containsKey(token)) {
				    final LineCommenter commenter = (LineCommenter) tokens.get(token);
					commentingUntil = commenter.getLast();
					commenting = commenter.isComment();
					log( "Commenting: " + beginToken + token + endToken, Project.MSG_VERBOSE );
					// We'll begin commenting with the next line.
				}
			} else {
				// We're commenting until we see "commentingUntil".
				// This means that commenters do not nest.
				if (commentingUntil.equals(token)) {
					log( "Commenting ends with: " + beginToken + token + endToken 
					   , Project.MSG_VERBOSE );
					// We've found it.
					// Switch back to looking for a new "first".
					commentingUntil = null;
				} else {
					return comment(line);
                }
            }
            
            return line;
        } catch (StringIndexOutOfBoundsException e) {
            return line;
        }
    }
    
    private final static String commentString = "// ";
    
    private String comment(final String line)
    {
      final String text = line.trim();
      final String whitespace = line.substring(0, line.lastIndexOf(text));
      
      if (commenting) {
		  if (text.startsWith(commentString))
		     return line;
		  
		  return whitespace + commentString + text;
      }
       
	  if (text.startsWith(commentString))
		 return whitespace + text.substring(commentString.length());
	   
      return line;
    }
    
    /**
     * Create a new filter
     *
     * @param commenter the filter to be added
     */
    public void addLineCommenter(LineCommenter commenter)
    {
        if (isReference()) {
            throw noChildrenAllowed();
        }
        getFilters().addElement(commenter);
    }
    
}
