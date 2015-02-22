package gnu.xml;
import java.io.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.kawa.io.BinaryInPort;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;

/** Reads XML from a char array.
 * Assumes a state-less character encoding containing ascii as a sub-set,
 * and where no byte in a multi-byte character is the same as a xml special
 * character.  Any bytes with high-order bit set are treated as if they
 * are letters, and can be part of names.
 *
 * Handles CR/LF, CDATA, entity references, processing instructions, DOCTYPE,
 * as well as the obvious (text, element, and attributes).
 *
 * @author Per Bothner
 */

public class XMLParser
{
  private static final int EXPECT_NAME_MODIFIER = 1;
  private static final int SKIP_SPACES_MODIFIER = 2;
  private static final int INIT_STATE = 0;
  private static final int TEXT_STATE = 1;
  private static final int BEGIN_ELEMENT_STATE = 2;
  private static final int END_ELEMENT_STATE = 4;
  private static final int SAW_ENTITY_REF = 6;  // Saw '&'.  
  private static final int ATTRIBUTE_SEEN_NAME_STATE = 8;
  private static final int MAYBE_ATTRIBUTE_STATE = 10;
  private static final int ATTRIBUTE_SEEN_EQ_STATE = 11;
  private static final int DOCTYPE_SEEN_STATE = 13;
  private static final int DOCTYPE_NAME_SEEN_STATE = 16;
  private static final int SAW_LEFT_STATE = 14;
  private static final int SAW_LEFT_SLASH_STATE = 19; // Seen '</'
  private static final int SAW_LEFT_EXCL_STATE = 20;
  private static final int SAW_LEFT_QUEST_STATE = 21; // Seen '<?'
  private static final int SAW_LEFT_EXCL_MINUS_STATE = 22;
  private static final int SAW_AMP_STATE = 25;  // Saw '&'.  
  private static final int SAW_AMP_SHARP_STATE = 26;  // Saw '&#'.  
  private static final int EXPECT_RIGHT_STATE = 27;
  private static final int PREV_WAS_CR_STATE = 28;
  private static final int INIT_LEFT_QUEST_STATE = 30;
  private static final int INIT_TEXT_STATE = 31;
  private static final int INIT_LEFT_STATE = 34;
  private static final int INVALID_VERSION_DECL = 35;
  private static final int SAW_ERROR = 36;
  private static final int SAW_EOF_ERROR = 37;  // Unexpected end-of-file.
  private static final int MISSING_XML_DECL = 38;

  static final String BAD_ENCODING_SYNTAX = "bad 'encoding' declaration";
  static final String BAD_STANDALONE_SYNTAX = "bad 'standalone' declaration";

  public static void parse (Object uri, SourceMessages messages, Consumer out)
    throws java.io.IOException
  {
    parse(Path.openInputStream(uri), uri, messages, out);
  }

    public static BinaryInPort XMLStreamReader(InputStream strm)
        throws java.io.IOException {
        BinaryInPort in = new BinaryInPort(strm);
        in.setFromByteOrderMark();
        in.setKeepFullLines(false);
        return in;
    }

  public static void parse (InputStream strm, Object uri,
                            SourceMessages messages, Consumer out)
    throws java.io.IOException
  {
    BinaryInPort in = XMLStreamReader(strm);
    if (uri != null)
      in.setName(uri);
    parse(in, messages, out);
    in.close();
  }

  public static void parse (InPort in, SourceMessages messages, Consumer out)
    throws java.io.IOException
  {
    XMLFilter filter = new XMLFilter(out);
    filter.setMessages(messages);
    filter.setSourceLocator(in);
    filter.startDocument();
    Object uri = in.getPath();
    if (uri != null)
      filter.writeDocumentUri(uri);
    parse(in, filter);
    filter.endDocument();
  }

  public static void parse (InPort in, SourceMessages messages, XMLFilter filter)
    throws java.io.IOException
  {
    filter.setMessages(messages);
    filter.setSourceLocator(in);
    filter.startDocument();
    Object uri = in.getPath();
    if (uri != null)
      filter.writeDocumentUri(uri);
    parse(in, filter);
    filter.endDocument();
    in.close();
  }

  public static void parse(InPort in, XMLFilter out)
  {
    // Cache fields in local variables, for speed.
    char[] buffer = in.buffer;
    int pos = in.pos;
    int limit = in.limit;
    boolean strict = false;

    // The flow logic of this method is unusual.  It is one big state machine,
    // but with two "subroutines": SKIP_SPACES_MODIFIER and EXPECT_NAME_MODIFIER.
    // There is also a "subroutine" to get a new character (and leave it in 'ch')
    // when 'break handleChar' is executed, except this has the hard-wired
    // continuation of switching on the 'state'.
    //
    // The justification for this rather usual design is performance.
    // As long as the input is contained within 'buffer', we don't need
    // to call input methods (only methods for emitting parsed data is
    // called).  We also maximize use of local variables - we do not
    // access any object fields (including fields of 'this') except
    // for getting the next char from 'buffer'.  These properties mean
    // this method can be compiled to very tight efficient code.

    int state = INIT_STATE;
    // 0: normal - in character context.
    // 1: seen '&'

    // The next two varibles are only relevant if state==INIT_STATE:
    char terminator = (char) '<';
    int continue_state = SAW_LEFT_STATE;
    char ch = (char) ' '; // ???
    int length = 0;
    int dstart = -1;
    String message = null;

    int start = -1;
  mainLoop:
    for (;;)
      {
        handleChar:  // When done get next character.
        switch (state)
          {
          case INIT_STATE:
            state = INIT_TEXT_STATE;
            break handleChar;

          case INIT_TEXT_STATE:
            if (ch == '<')
              {
                state = INIT_LEFT_STATE;
                break handleChar;
              }
            state = strict ?  MISSING_XML_DECL : TEXT_STATE;
            continue mainLoop;

          case INIT_LEFT_STATE:
            if (ch == '?')
              {
                start = pos;
                state = EXPECT_NAME_MODIFIER + SKIP_SPACES_MODIFIER + INIT_LEFT_QUEST_STATE;
                break handleChar;
              }
            state = strict ?  MISSING_XML_DECL : SAW_LEFT_STATE;
            continue mainLoop;

          case MISSING_XML_DECL:
             message = "missing XML declaration";
             state = SAW_ERROR;
             continue mainLoop;

          case INVALID_VERSION_DECL:
            pos = dstart;
            message = "invalid xml version specifier";
            state = SAW_ERROR;
            continue mainLoop;

          case SAW_ERROR:
            in.pos = pos;
            start = -1;
            out.error('e', message);
            for (;;)
              {
                if (pos >= limit)
                  break mainLoop;
                ch = buffer[pos++];
                if (ch == '>')
                  {
                    state = TEXT_STATE;
                    break handleChar;
                  }
              }

          case SAW_EOF_ERROR:
            in.pos = pos;
            out.error('f', "unexpected end-of-file");
            return;

          case TEXT_STATE:
            // This state handle text not inside tags (in which case
            // terminator=='<').  It also handles attribute values (in
            // which case terminator is '\'' or '"').
            start = pos - 1;
            // Not length now, but used to calculate length when done.
            length = pos;
            for (;;)
              {
                if (ch == terminator)
                  {
                    state = continue_state;
                    break;
                  }
                if (ch == '&')
                  {
                    state = SAW_AMP_STATE;
                    break;
                  }
                if (ch == '\r')
                  {
                    length = pos - length;
                    in.pos = pos;
                    if (length > 0)
                      out.textFromParser(buffer, start, length);
                    if (pos < limit)
                      {
                        ch = buffer[pos];
                        if (ch == '\n')
                          {
                            start = pos;
                            length = ++pos;
                          }
                        else
                          {
                            out.linefeedFromParser();
                            if (ch == 0x85)
                              {
                                start = pos++;
                                length = pos + 1;
                              }
                            else
                              {
                                in.incrLineNumber(1, pos);
                                start = pos;
                                length = ++pos;
                                continue;
                              }
                          } 
                        in.incrLineNumber(1, pos);
                      }
                    else
                      {
                        out.linefeedFromParser();
                        state = PREV_WAS_CR_STATE;
                        break handleChar;
                      }
                  }
                else if (ch == 0x85 || ch == 0x2028)
                  {
                    length = pos - length;
                    in.pos = pos-1;
                    if (length > 0)
                      out.textFromParser(buffer, start, length);
                    out.linefeedFromParser();
                    in.incrLineNumber(1, pos);
                    length = pos + 1;
                    start = pos;
                  }
                else if (ch == '\n')
                  {
                    in.incrLineNumber(1, pos);
                  }
                if (pos == limit)
                  {
                    length--;
                    break;
                  }
                ch = buffer[pos++];
              }
            length = pos - length;
            if (length > 0)
              {
                in.pos = pos;
                out.textFromParser(buffer, start, length);
              }
	    start = -1;
            break handleChar;

          case PREV_WAS_CR_STATE:
            // The previous character was a '\r', and we passed along '\n'
            // to out.  If the new character is '\n' or 0x85 ignore it.
            state = TEXT_STATE;
            if (ch == '\n' || ch == 0x85)
              {
                in.incrLineNumber(1, pos);
                break handleChar;
              }
            else
              {
                in.incrLineNumber(1, pos-1);
                continue;
              }

          case SKIP_SPACES_MODIFIER + EXPECT_RIGHT_STATE:
          case SKIP_SPACES_MODIFIER + MAYBE_ATTRIBUTE_STATE:
          case SKIP_SPACES_MODIFIER + SAW_LEFT_QUEST_STATE:
          case SKIP_SPACES_MODIFIER + INIT_LEFT_QUEST_STATE:
          case SKIP_SPACES_MODIFIER + DOCTYPE_SEEN_STATE:
            // "Subroutine" for skipping whitespace.
            if (ch == ' ' || ch == '\t')
              break handleChar;
            if (ch == '\n' || ch == '\r'
		|| ch == '\u0085' || ch == '\u2028')
              {
                in.incrLineNumber(1, pos);
                break handleChar;
              }
            // Not a space, so "return" to next state.
            state -= SKIP_SPACES_MODIFIER;
            continue mainLoop;

          case EXPECT_NAME_MODIFIER + BEGIN_ELEMENT_STATE:
          case EXPECT_NAME_MODIFIER + END_ELEMENT_STATE:
          case EXPECT_NAME_MODIFIER + ATTRIBUTE_SEEN_NAME_STATE:
          case EXPECT_NAME_MODIFIER + SAW_ENTITY_REF:
          case EXPECT_NAME_MODIFIER + DOCTYPE_NAME_SEEN_STATE:
          case EXPECT_NAME_MODIFIER + SKIP_SPACES_MODIFIER + SAW_LEFT_QUEST_STATE:
          case EXPECT_NAME_MODIFIER + SKIP_SPACES_MODIFIER + INIT_LEFT_QUEST_STATE:
            length = start+1;
            // "Subroutine" for reading a Name.
            for (;;)
              {
		// XML 1.1 candidate recommendation:
		// [2] Char    ::=    #x9 | #xA | #xD | [#x20-#x7E] | #x85
		//   | [#xA0-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
		// [4]  NameStartChar := ":" | [A-Z] | "_" | [a-z] |
		//   [#xC0-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] |
		//   [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] |
		//   [#x3001-#xD7FF] | [#xF900-#xEFFFF]
		// [4a] NameChar := NameStartChar | "-" | "." | [0-9] | #xB7 |
		//   [#x0300-#x036F] | [#x203F-#x2040]
                if ((ch >= 'a' && ch <= 'z') ||
		    (ch >= 'A' && ch <= 'Z') ||
		    ch == '_' || ch == ':' ||
		    (ch >= 0xC0 && (ch <= 0x2FF ||
				    (ch >= 0x370 &&
				     ((ch <= 0x1FFF && ch != 0x37E) ||
				      (ch >= 0x200C &&
				       (ch <= 0x200D ||
					(ch >= 0x2070 && ch <= 0x218F)||
					(ch >= 0x2C00 && ch <= 0x2FEF) ||
					(ch >= 0x3001 && ch <= 0xD7FF) ||
					(ch >= 0xF900 && ch <= 0xFFFD))))))) ||
		    (pos > length &&
		     (ch >= '0' && ch <= '9') ||
		      ch == '.' || ch == '-' ||
		     ch == 0xB7 ||
		     (ch > 0x300 &&
		      (ch <= 0x36F || (ch >= 0x203F && ch <= 0x2040)))))
		  {
                  }
                else
                  {
		    state -= EXPECT_NAME_MODIFIER;
		    length = pos - length;
		    if (length == 0)
		      {
			if (state == ATTRIBUTE_SEEN_NAME_STATE)
			  message = "missing or invalid attribute name";
                        else if (state == BEGIN_ELEMENT_STATE
                                 || state == END_ELEMENT_STATE)
			  message = "missing or invalid element name";
			else
			  message = "missing or invalid name";
			state = SAW_ERROR;
		      }
                    continue mainLoop;
                  }
                if (pos < limit)
		  ch = buffer[pos++];
		else
		  break handleChar;
              }
          case SAW_AMP_SHARP_STATE:
	    for (;;)
	      {
		if (ch == ';')
		  {
                    in.pos = pos;
		    out.emitCharacterReference(length,
					       buffer, start, pos-1-start);
		    state = TEXT_STATE;
		    break handleChar;
		  }
		if (ch == 'x' && dstart == 0)
		  dstart = 16;
		else if (length >= 0x8000000)
		  break; // Overflow likely.
		else
		  {
		    int base = dstart == 0 ? 10 : dstart;
		    int digit = Character.digit((char) ch, base);
		    if (digit < 0)
		      break;
		    length = length * base + digit;
		  }
                if (pos < limit)
		  ch = buffer[pos++];
		else
		  break handleChar;
	      }
            in.pos = pos;
            out.error('e', "invalid character reference");
	    state = TEXT_STATE;
            break handleChar;

          case SAW_AMP_STATE:
            if (ch == '#')
              {
                state = SAW_AMP_SHARP_STATE;
		start = pos;
		length = 0;  // accumulated value; -1 means error, -2 overflow
		dstart = 0;  // base - 0 means not seen yet
                break handleChar;
              }
	    start = pos - 1;
            state = EXPECT_NAME_MODIFIER + SAW_ENTITY_REF;
            continue mainLoop;

          case SAW_ENTITY_REF:
            in.pos = pos;
            if (ch != ';')
              out.error('w', "missing ';'");
            out.emitEntityReference(buffer, start, length);
	    start = -1;
            state = TEXT_STATE;
            break handleChar;

          case SAW_LEFT_STATE: // Saw '<'
            if (ch == '/')
              {
                state = SAW_LEFT_SLASH_STATE;
                break handleChar;
              }
            if (ch == '?')
              {
		start = pos;
                state = EXPECT_NAME_MODIFIER + SKIP_SPACES_MODIFIER + SAW_LEFT_QUEST_STATE;
                break handleChar;
              }
            if (ch == '!')
              {
                state = SAW_LEFT_EXCL_STATE;
		start = pos;
                break handleChar;
              }
            // Read Name then goto BEGIN_ELEMENT_STATE.
	    start = pos - 1;
            state = EXPECT_NAME_MODIFIER + BEGIN_ELEMENT_STATE;
            continue mainLoop;
          case BEGIN_ELEMENT_STATE:
            in.pos = pos-length;  // position of start of name, for errors.
            out.emitStartElement(buffer, start, length);
            state = SKIP_SPACES_MODIFIER + MAYBE_ATTRIBUTE_STATE;
	    start = -1;
            continue mainLoop;

          case SAW_LEFT_QUEST_STATE: // Seen '<?' Name Spaces
          case INIT_LEFT_QUEST_STATE: // Seen '<?' Name Spaces
	    if (dstart < 0)
	      dstart = pos - 1;
            for (;;)
              {
		int end;
		if (ch == '>'
		    && buffer[end = pos - 2] == '?'
		    && end >= dstart)
		  {
                    in.pos = pos;
                    if (length == 3
                        && buffer[start] == 'x'
                        && buffer[start+1] == 'm'
                        && buffer[start+2] == 'l')
                      {
                        if (state == INIT_LEFT_QUEST_STATE)
                          {
                            if (end <= dstart+7
                                || buffer[dstart] != 'v'
                                || buffer[dstart+1] != 'e'
                                || buffer[dstart+2] != 'r'
                                || buffer[dstart+3] != 's'
                                || buffer[dstart+4] != 'i'
                                || buffer[dstart+5] != 'o'
                                || buffer[dstart+6] != 'n')
                              {
                                // FIXME should allow if !strict
                                pos = dstart;
                                message = "xml declaration without version";
                                state = SAW_ERROR;
                                continue mainLoop;
                              }
                            dstart += 7;
                            ch = buffer[dstart];
                            while (Character.isWhitespace(ch)
                                   && ++dstart < end)
                              ch = buffer[dstart];
                            if (ch != '=')
                              {
                                state = INVALID_VERSION_DECL;
                                continue mainLoop;
                              }
                            ch = buffer[++dstart];
                            while (Character.isWhitespace(ch)
                                   && ++dstart < end)
                              ch = buffer[dstart];
                            if (ch != '\'' && ch != '\"')
                              {
                                state = INVALID_VERSION_DECL;
                                continue mainLoop;
                              }
                            char quote = ch;
                            int i = ++dstart;
                            for (;; i++)
                              {
                                if (i == end)
                                  {
                                    state = INVALID_VERSION_DECL;
                                    continue mainLoop;
                                  }
                                ch = buffer[i];
                                if (ch == quote)
                                  break;
                              }
                            if (i == dstart + 3 && buffer[dstart] == '1'
                                && buffer[dstart+1] == '.'
                                && (ch = buffer[dstart+2]) == '0' || ch == '1')
                              {
                                // Save version number, if that is useful.
                              }
                            else
                              {
                                state = INVALID_VERSION_DECL;
                                continue mainLoop;
                              }
                            dstart = i+1;
                            while (dstart < end
                                   && Character.isWhitespace(buffer[dstart]))
                              dstart++;
                            if (end > dstart + 7
                                && buffer[dstart] == 'e'
                                && buffer[dstart+1] == 'n'
                                && buffer[dstart+2] == 'c'
                                && buffer[dstart+3] == 'o'
                                && buffer[dstart+4] == 'd'
                                && buffer[dstart+5] == 'i'
                                && buffer[dstart+6] == 'n'
                                && buffer[dstart+7] == 'g')
                              {
                                dstart += 8;
                                ch = buffer[dstart];
                                while (Character.isWhitespace(ch)
                                       && ++dstart < end)
                                  ch = buffer[dstart];
                                if (ch != '=')
                                  {
                                    message = BAD_ENCODING_SYNTAX;
                                    state = SAW_ERROR;
                                    continue mainLoop;
                                  }
                                ch = buffer[++dstart];
                                while (Character.isWhitespace(ch)
                                       && ++dstart < end)
                                  ch = buffer[dstart];
                                if (ch != '\'' && ch != '\"')
                                  {
                                    message = BAD_ENCODING_SYNTAX;
                                    state = SAW_ERROR;
                                    continue mainLoop;
                                  }
                                quote = ch;
                                i = ++dstart;
                                for (;; i++)
                                  {
                                    if (i == end)
                                      {
                                        message = BAD_ENCODING_SYNTAX;
                                        state = SAW_ERROR;
                                        continue mainLoop;
                                      }
                                    ch = buffer[i];
                                    if (ch == quote)
                                      break;
                                  }
                                String encoding = new String(buffer,dstart, i-dstart);
                                if (in instanceof BinaryInPort)
                                  ((BinaryInPort) in).setCharset(encoding);
                                dstart = i+1;
                                while (dstart < end
                                       && Character.isWhitespace(buffer[dstart]))
                                  dstart++;
                              }
                            if (end > dstart + 9
                                && buffer[dstart] == 's'
                                && buffer[dstart+1] == 't'
                                && buffer[dstart+2] == 'a'
                                && buffer[dstart+3] == 'n'
                                && buffer[dstart+4] == 'd'
                                && buffer[dstart+5] == 'a'
                                && buffer[dstart+6] == 'l'
                                && buffer[dstart+7] == 'o'
                                && buffer[dstart+8] == 'n'
                                && buffer[dstart+9] == 'e')
                              {
                                dstart += 10;
                                ch = buffer[dstart];
                                while (Character.isWhitespace(ch)
                                       && ++dstart < end)
                                  ch = buffer[dstart];
                                if (ch != '=')
                                  {
                                    message = BAD_STANDALONE_SYNTAX;
                                    state = SAW_ERROR;
                                    continue mainLoop;
                                  }
                                ch = buffer[++dstart];
                                while (Character.isWhitespace(ch)
                                       && ++dstart < end)
                                  ch = buffer[dstart];
                                if (ch != '\'' && ch != '\"')
                                  {
                                    message = BAD_STANDALONE_SYNTAX;
                                    state = SAW_ERROR;
                                    continue mainLoop;
                                  }
                                quote = ch;
                                i = ++dstart;
                                for (;; i++)
                                  {
                                    if (i == end)
                                      {
                                        message = BAD_STANDALONE_SYNTAX;
                                        state = SAW_ERROR;
                                        continue mainLoop;
                                      }
                                    ch = buffer[i];
                                    if (ch == quote)
                                      break;
                                  }
                                if (i == dstart+3
                                    && buffer[dstart] == 'y'
                                    && buffer[dstart+1] == 'e'
                                    && buffer[dstart+2] == 's')
                                  {
                                  }
                                else if (i == dstart+2
                                         && buffer[dstart] == 'n'
                                         && buffer[dstart+1] == 'o')
                                  {
                                  }
                                else
                                  {
                                    message = BAD_STANDALONE_SYNTAX;
                                    state = SAW_ERROR;
                                    continue mainLoop;
                                  }
                                dstart = i+1;
                                while (dstart < end
                                       && Character.isWhitespace(buffer[dstart]))
                                  dstart++;
                              }
                            if (end != dstart)
                              {
                                message = "junk at end of xml declaration";
                                pos = dstart;
                                state = SAW_ERROR;
                                continue mainLoop;
                              }
                          }
                        else
                          {
                            message = "<?xml must be at start of file";
                            state = SAW_ERROR;
                            continue mainLoop;
                          }
                      }
                    else if (strict && state == INIT_LEFT_QUEST_STATE)
                      {
                        state = MISSING_XML_DECL;
                        continue mainLoop;
                      }
                    else
                      out.processingInstructionFromParser(buffer, start, length,
                                                          dstart, end - dstart);
		    start = -1;
		    dstart = -1;
		    state = TEXT_STATE;
		    break handleChar;
		  }
                if (pos < limit)
		  ch = buffer[pos++];
		else
		  break handleChar;
              }

          case SAW_LEFT_EXCL_STATE: // Seen '<!'
	  exclLoop:
	    for (;;)
	      {
		if (ch == '>')
		  {
		    length = pos - 1 - start;
		    if (length >= 4
			&& buffer[start] == '-'
			&& buffer[start+1] == '-')
		      {
			if (buffer[pos-2] == '-'
			    && buffer[pos-3] == '-')
			  {
                            in.pos = pos;
			    out.commentFromParser(buffer, start + 2, length - 4);
                            start = -1;
			    break exclLoop;
			  }
		      }
		    else if (length >= 6
			     && buffer[start] == '['
			     && buffer[start+1] == 'C'
			     && buffer[start+2] == 'D'
			     && buffer[start+3] == 'A'
			     && buffer[start+4] == 'T'
			     && buffer[start+5] == 'A'
			     && buffer[start+6] == '[')
		      {
			if (buffer[pos-2] == ']'
			    && buffer[pos-3] == ']')
			  {
                            in.pos = pos;
			    out.writeCDATA(buffer, start + 7, pos - 10 - start);
                            start = -1;
			    break exclLoop;
			  }
		      }
		    else
		      {
			// FIXME ignoreing <!ELEMENT ... > etc.
			break exclLoop;
		      }
		  }
		else if (pos == start+7
			 &&  buffer[start] == 'D'
			 &&  buffer[start+1] == 'O'
			 &&  buffer[start+2] == 'C'
			 &&  buffer[start+3] == 'T'	
			 &&  buffer[start+4] == 'Y'
			 &&  buffer[start+5] == 'P'
			 &&  ch == 'E')
		  {
                    start = -1;
		    state = SKIP_SPACES_MODIFIER + DOCTYPE_SEEN_STATE;
		    break handleChar;
		  }
                if (pos < limit)
		  ch = buffer[pos++];
		else
		  break handleChar;
	      }
	    start = -1;
	    state = TEXT_STATE;
	    break handleChar;

          case DOCTYPE_SEEN_STATE:  /* Seen '<!DOCTYPE' S* */
	    state = EXPECT_NAME_MODIFIER + DOCTYPE_NAME_SEEN_STATE;
	    start = pos - 1;
	    continue mainLoop;

          case DOCTYPE_NAME_SEEN_STATE:  /* Seen '<!DOCTYPE' S* Name */
	    if (dstart < 0)
	      {
                // First type - i.e. not after a handelChar call.
		dstart = pos - 1;
                dstart -= start; // Make relative.
                dstart <<= 1; // Add bit for whether in a '['.
		terminator = 0;
	      }
            for (;;)
              {
		if (ch == '\'' || ch == '\"')
                  {
                    if (terminator == 0)
                      terminator = ch;
                    else if (terminator == ch)
                      terminator = 0;
                  }
                else if (terminator == 0) // I.e. not inside a string.
                  {
                    // Low-order bit of dstart is 1 if we've seen a '['.
                    if (ch == '[')
                      dstart |= 1;
                    else if (ch == ']')
                      dstart &= ~1;
                    else if (ch == '>' && (dstart & 1) == 0)
                      {
                        in.pos = pos;
                        dstart >>= 1;
                        dstart += start;
                        out.emitDoctypeDecl(buffer, start, length,
                                            dstart, pos - 1 - dstart);
                        terminator = (char) '<';
                        start = -1;
                        dstart = -1;
                        state = TEXT_STATE;
                        break handleChar;
                      }
                  }
                if (pos < limit)
		  ch = buffer[pos++];
		else
		  break handleChar;
              }

          case MAYBE_ATTRIBUTE_STATE:
            terminator = '<';
            continue_state = SAW_LEFT_STATE;
            if (ch == '/')
              {
                in.pos = pos;
                out.emitEndAttributes();
                out.emitEndElement(null, 0, 0);
                state = EXPECT_RIGHT_STATE;
                break handleChar;
              }
            if (ch == '>')
              {
                in.pos = pos;
                out.emitEndAttributes();
                state = TEXT_STATE;
                break handleChar;
              }
	    start = pos - 1;
            state = EXPECT_NAME_MODIFIER + ATTRIBUTE_SEEN_NAME_STATE;
            continue mainLoop;
          case ATTRIBUTE_SEEN_NAME_STATE:
            if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
		|| ch == '\u0085' || ch == '\u2028')
              break handleChar;
            in.pos = pos-length; // position of start of name, for errors.
            out.emitStartAttribute(buffer, start, length);
	    start = -1;
            if (ch == '=')
              {
                state = ATTRIBUTE_SEEN_EQ_STATE;
                break handleChar;
              }
            out.emitEndAttributes();
            message = "missing or misplaced '=' after attribute name";
            state = SAW_ERROR;
            continue mainLoop;
          case ATTRIBUTE_SEEN_EQ_STATE:
            if (ch == '\'' || ch == '\"')
              {
                terminator = ch;
                continue_state = SKIP_SPACES_MODIFIER + MAYBE_ATTRIBUTE_STATE;
                state = TEXT_STATE;
                break handleChar;
              }
            if (ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
		|| ch == '\u0085' || ch == '\u2028')
              break handleChar;
            out.emitEndAttributes();
            message = "missing or unquoted attribute value";
            state = SAW_ERROR;
            continue mainLoop;

          case SAW_LEFT_SLASH_STATE: // Seen '</'.
            // Do "Name" subroutine, then goto END_ELEMENT_STATE.
	    start = pos - 1;
            state = EXPECT_NAME_MODIFIER + END_ELEMENT_STATE;
            continue mainLoop;

          case END_ELEMENT_STATE:  // Seen '</' Name.
            in.pos = pos;
            out.emitEndElement(buffer, start, length);
	    start = -1;
            // Skip spaces then goto EXPECT_RIGHT_STATE.
            state = SKIP_SPACES_MODIFIER + EXPECT_RIGHT_STATE;
            continue mainLoop;
          
          case EXPECT_RIGHT_STATE: // Looking for '>'.
            if (ch != '>')
              {
                message = "missing '>'";
                state = SAW_ERROR;
                continue mainLoop;
              }
            state = TEXT_STATE;
            break handleChar;
          }

        // After 'break handleChar', we get here.
        if (pos >= limit)
          {
	    int saved = pos - start;
            try
              {
                if (start >= 0)
                  {
                      in.setSaveStart(start);
                  }
                in.pos = pos;
                int x = in.peek();
                if (x < 0)
                  {
                    if (state == TEXT_STATE || state == PREV_WAS_CR_STATE)
                      return;
                    state = SAW_EOF_ERROR;
                    continue;
                  }
                if (start >= 0)
                  {
                    in.setSaveStart(-1);
                  }
              }
            catch (java.io.IOException ex)
              {
                throw new RuntimeException(ex.getMessage());
              }
            pos = in.pos;
            buffer = in.buffer;

            limit = in.limit;
            start = start >= 0 ? pos - saved : limit;
          }
        ch = buffer[pos++];
      }
  }
}
