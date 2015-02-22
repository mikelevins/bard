package gnu.kawa.ant;

import org.apache.tools.ant.types.FilterSet;

import org.apache.tools.ant.Project;

import java.util.Enumeration;
import java.util.Hashtable;

public class LineStripperSet extends FilterSet
{
    // The replaceTokens method is stateful.
    // If this is not null, then we ignore all lines until we find that token.
    private String strippingUntil = null;
    
    public LineStripperSet()
    {
    }
    
    /**
     * Individual filter component of filterset
     *
     * @author    Jim White
     * Created   2001-11-14.
     */
    public static class LineStripper extends FilterSet.Filter
    {
        String first;
        String last;
        
        public LineStripper()
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
        
        public String getFirst()
        {
            return first;
        }
        
        public String getLast()
        {
            return last;
        }
        
    }
    
    /**
     * Gets the filter hash of the FilterSet.
     *
     * @return   The hash of the tokens and values for quick lookup.
     */
    public Hashtable getStripperHash() {
        final int filterSize = getFilters().size();
        final Hashtable stripperHash = new Hashtable(filterSize);
        for (final Enumeration e = getFilters().elements(); e.hasMoreElements();) {
           LineStripper stripper = (LineStripper) e.nextElement();
           stripperHash.put(stripper.getFirst(), stripper.getLast());
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
        int index = line.indexOf(beginToken);
        
        if (strippingUntil != null) {
            log( "Stripping until: " + beginToken + strippingUntil + endToken 
               , Project.MSG_VERBOSE );
        }
           
        if (index < 0) {
           if (strippingUntil == null) {
               return line;
           }
           
           return "";
        }
            
        final Hashtable tokens = getStripperHash();
        try {
            StringBuffer b = new StringBuffer();
            int i = 0;
            String token = null;
            
            do {
                int endIndex = line.indexOf(endToken, index + beginToken.length() + 1 );
                if (endIndex == -1) {
                    break;
                }
                token = line.substring(index + beginToken.length(), endIndex );
                
                if (strippingUntil == null) {
                    // We're not stripping, so look for a "first" token.
                    b.append(line.substring(i, index));
                    if (tokens.containsKey(token)) {
                        strippingUntil = (String) tokens.get(token);
                        log( "Stripping: " + beginToken + token + endToken, Project.MSG_VERBOSE );
                        i = index + beginToken.length() + token.length() + endToken.length();
                    } else {
                        // just append beginToken and search further
                        b.append(beginToken);
                        i = index + beginToken.length();
                    }
                } else {
                    // We're stripping until we see "strippingUntil".
                    // This means that strippers do not nest.
                    // That could be an option later.
                    if (strippingUntil.equals(token)) {
                        log( "Stripping ends with: " + beginToken + token + endToken 
                           , Project.MSG_VERBOSE );
                        // We've found it.
                        // Skip over the token.
                        i = index + beginToken.length() + token.length() + endToken.length();
                        // Switch back to looking for a new "first".
                        strippingUntil = null;
                    } else {
                        // Keep looking.
                        i = index + beginToken.length();
                    }
                }
            } while ((index = line.indexOf( beginToken, i )) > -1 );
            
            b.append(line.substring(i));
            return b.toString();
        } catch (StringIndexOutOfBoundsException e) {
            return line;
        }
    }
    
    /**
     * Create a new filter
     *
     * @param stripper the filter to be added
     */
    public void addLineStripper(LineStripper stripper)
    {
        if (isReference()) {
            throw noChildrenAllowed();
        }
        getFilters().addElement(stripper);
    }
    
}
