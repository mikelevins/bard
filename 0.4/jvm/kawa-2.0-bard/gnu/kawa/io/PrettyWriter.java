// Copyright (c) 2001, 2004, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.io;
import gnu.kawa.util.IntHashTable;
import java.io.*;
import gnu.mapping.ThreadLocation;
import gnu.lists.LList;

/** 
 * A pretty printer.
 *
 * <p>
 * This code is transcribed from pprint.lisp in Steel Bank Common Lisp,
 * which is again based on the code in CMU Common Lisp.
 * Modifications have been made to accommodate shared structures, as described
 * in SRFI-38. 
 * </p>
 * <p>
 * The pretty printer (hereafter PP) is responsible for formatting the results 
 * of evaluating expressions so that the human user can easily read them. The PP
 * may also be responsible for choosing where to break lines so that the output 
 * looks reasonable.
 * </p>
 * <p>
 * Raw text that has not yet been written out is stored in the "buffer" array,
 * while information about the structure of the text is stored in a queue of
 * "formatting tokens" (see below) called queueInts. A circular queue is used
 * when the PP is not dealing with shared structures, and a linear array is used
 * when it is. This only effects how the formatting tokens are enqueued into the
 * structure, and how the structure is dynamically expanded.
 * </p>
 * <p>
 * Formatting tokens are simply contiguous chunks of the queue. All tokens have
 * a type and a size. For example, there is a formatting token called
 * {@code QITEM_NEWLINE_TYPE} whose size is specified by {@code QITEM_NEWLINE_SIZE}. The size
 * includes these type and size bits. The tokens use the space allocated to them
 * in different ways. For example, the tab token uses two of its chunks ({@code int}s)
 * to specify at which column it was entered, and to which column it should
 * expand to.
 * </p>
 * <p>
 * The tokens are buffered by a display formatter. The display formatter walks
 * the structure to be printed emitting these tokens when it finds it has to,
 * as well as the actual characters to be printed! Remember, the formatting
 * tokens are distinct from the actual characters written, they just control how the
 * characters are displayed. To accommodate this dichotomy, the character buffer
 * holds the actual characters as they appear to the display formatter, the queue 
 * holds the formatting tokens plus a mapping that specifies which character in 
 * the buffer they end at.
 * </p>
 * <p>
 * Once the display formatter has finished walking the expression to be printed,
 * the formatting queue is walked by the "destructive" methods, i.e., the ones
 * that actually flush the buffer to the output port. As mentioned above, each
 * token encodes the position it references in the character buffer, so we use
 * this information to work our way through the buffer, occasionally modifying
 * the buffer to look pretty (dictated by the formatting tokens). Examples of
 * such modifications include changing a space character to a newline character
 * so that the line will prettily fit on the specified line width, or more
 * drastically, remove a shared object of the expression and replacing
 * it will print-circle notation.
 * </p>
 * <p>
 * Once everything has been flushed, the initial conditions are reset ready for
 * the next printing
 * </p>
 * 
 * @author Per Bothner
 * @author Charles Turner
 */

public class PrettyWriter extends java.io.Writer
{
  protected Writer out;

  /**
   * Construct a PrettyWriter with {@code prettyPrintingMode = 1}
   * @param out The output to write to
   * @see #prettyPrintingMode
   */
  public PrettyWriter(java.io.Writer out)
  {
    this.out = out;
    setPrettyPrintingMode(1);
  }

  /**
   * Construct a PrettyWriter which breaks on a given line length.
   * If {@code lineLength} is strictly greater than one, 
   * {@code prettyPrintingMode} is set to 1, otherwise it's set to 0.
   *
   * @param out The output to write to
   * @param lineLength The column width lines should break on
   * @see #prettyPrintingMode #lineLength
   */
  public PrettyWriter(java.io.Writer out, int lineLength)
  {
    this.out = out;
    this.lineLength = lineLength;
    setPrettyPrintingMode(lineLength > 1 ? 1 : 0);
  }

  /**
   * Construct a PrettyWriter.
   * @param out The output port to write to
   * @param prettyPrintingMode If {@code true} then {@code prettyPrintingMode = 1}
   *        otherwise {@code prettyPrintingMode = 0}
   */
  public PrettyWriter(java.io.Writer out, boolean prettyPrintingMode)
  {
    this.out = out;
    setPrettyPrintingMode(prettyPrintingMode ? 1 : 0);
  }

  /** Line length we should format to. */
  int lineLength = 80;

  /**
   * If the right-hand-side margin is less than or equal to this value, the
   * miser printing mode is entered. This is compact style of printing where
   * line breaks are taken at every inter-token space.
   * (1
   *  2
   *  3) is an example of a miser-style printing.
   */
  int miserWidth = 40;

  /**
   * This variable is used to determine how the queueInts array should be
   * expanded. When printing with shared structure, we don't want a circular
   * queue, since we rely on being able to reference formatting tokens in
   * previous queue slots. When the queue has wrapped and expanded several times,
   * keeping track of such references is a nightmare.
   */
  boolean sharing = false;

  /**
   * When the display formatter has written a back-reference, we know for certain
   * that print-circle notation has been emitted.
   * This variable is used to avoid the expensive resolveBackReferences method 
   * from running in the case that we have a non-recursive object.
   */
  boolean reallySharing = false;

  public static ThreadLocation lineLengthLoc
    = new ThreadLocation("line-length");
  public static ThreadLocation miserWidthLoc
    = new ThreadLocation("miser-width");
  public static ThreadLocation indentLoc
    = new ThreadLocation("indent");

  /**
   * Whether to resolve shared structures is dependent on the out:print-circle
   * command line switch being set true. The PrettyWriter uses a different
   * data structure to resolve back-references when we're emitting print-circle
   * notation.
   */
  public static ThreadLocation isSharing 
    = new ThreadLocation("print-circle");

  /** The current pretty-printing mode.
   * See setPrettyPrintingMode for valid values. */
  private int prettyPrintingMode;
  
  private IntHashTable idhash;

  public void initialiseIDHash ()
  {
    Object share = isSharing.get(null);
    idhash = new IntHashTable();
  }

  public void clearIDHash()
  {
    idhash.clear();
  }
  
    public void finishIDHash() {
        idhash.clear();
        writeEndOfExpression();
        resolveBackReferences();
        flush();
    }

  public int IDHashLookup(Object obj) {
    if (idhash == null) initialiseIDHash();
    return idhash.lookup(obj);
  }

  public int IDHashGetFromIndex(int index) {
    return idhash.getFromIndex(index);
  }

  public int IDHashPutAtIndex(Object obj, int value, int index) {
    return idhash.putAtIndex(obj, value, index);
  }

  public int IDHashRemove(Object obj) {
    return idhash.remove(obj);
  }

  /** Control pretty-printing mode.
   * @param mode the value 0 disables pretty-printing;
   *   the value 1 enables explicit pretty-printing;
   *   the value 2 enables pretty-printing with auto-fill, which means that
   *   spaces are treated like enqueing NEWLINE_SPACE (essentiall a 'fill').
   */
    public void setPrettyPrintingMode (int mode) {
        prettyPrintingMode = mode;
    }

    public void setSharing (boolean sharing) {
        this.sharing = sharing;
    }

  /** Return pretty-printing mode.
   * @return 0, 1, 2, as described for {@link #setPrettyPrintingMode(int)}.
   */
  public int getPrettyPrintingMode () { return prettyPrintingMode; }

  /** Is pretty printing enabled? */
  public boolean isPrettyPrinting () { return prettyPrintingMode > 0; }

  /** Turn pretty printing on or off.
   * Equivalent to {@code setPrettyPrintingMode(mode?1:0)}.
   */
  public void setPrettyPrinting (boolean mode)
  {
    setPrettyPrintingMode(mode ? 0 : 1);
  }

    /** Should we write directly to out without using buffer?
     * Currently, this is always false, but if you want to support
     * "unbuffered mode", you need to change this.
     */
    private boolean isPassingThrough() {
        return buffer.length == 0 && out != null;
    }

  /** Holds all the text that has been output but not yet printed. */
  public /* FIXME */ char[] buffer = new char[2048];

  /** The index into BUFFER where more text should be put. */
  public /* FIXME */ int bufferFillPointer;

  /** Total amount of stuff that has been shifted out of the buffer.
   * Whenever we output stuff from the buffer, we shift the remaining noise
   * over. This makes it difficult to keep references to locations in
   * the buffer. */
  int bufferOffset;

  /** The column the first character in the buffer will appear in.
   * Normally zero, but if we end up with a very long line with no breaks in it
   * we might have to output part of it. Then this will no longer be zero.
   * Ditto after emitting a prompt. */
  int bufferStartColumn;
  
  /** The line number we are currently on. Used for *print-lines* abrevs and
   * to tell when sections have been split across multiple lines. */
  int lineNumber;

  // There are three different units for measuring character positions:
  //   COLUMN - offset (in characters) from the start of the current line.
  //   INDEX - index into the output buffer.
  //   POSN - some position in the stream of characters cycling through
  //          the output buffer.

  /**
   * Return the adjusted index into the {@link #buffer}. The position is adjusted
   * such this index will point to data that has not already been printed.
   * @param index The index into the {@link #buffer}
   * @return The adjusted index accounting for already written data
   */
  private int indexPosn (int index)
  {
    return index + bufferOffset;
  }

  /**
   * Return the "real" index to {@link #buffer} which may index data already 
   * written. Useful for QITEM_POSN's
   * @param posn The adjusted index into the {@link #buffer}
   * @return  The "real" index into the {@link #buffer}
   * @see #indexPosn(int)
   */
  private int posnIndex (int posn)
  {
    return posn - bufferOffset;
  }

  /**
   * Returns the index of a QITEM position relative to the output buffer
   * @param posn The QITEM position to be processed
   * @return The position of this QITEM relative to the {@link #buffer}
   */
  private int posnColumn (int posn)
  {
    return indexColumn(posnIndex(posn));
  }

  /** Stack of logical blocks in effect at the buffer start.
   * I.e. blocks for which {@code reallyStartLogicalBlock} has been called.
   * Each block uses {@code LOGICAL_BLOCK_LENGTH} {@code int} in this array. */
  int[] blocks = new int[10 * LOGICAL_BLOCK_LENGTH];
  /** Number of {@code int}s used by each block in the {@code blocks} array. */
  static final private int LOGICAL_BLOCK_LENGTH = 6;
  static final private int BLOCK_START_COLUMN = -1;
  static final private int BLOCK_SECTION_COLUMN = -2;
  static final private int BLOCK_PER_LINE_PREFIX_END = -3;
  static final private int BLOCK_PREFIX_LENGTH = -4;
  static final private int BLOCK_SUFFIX_LENGTH = -5;
  static final private int BLOCK_SECTION_START_LINE = -6;
  /** The "stack pointer" in the {@code blocks} array. */
  int blockDepth = LOGICAL_BLOCK_LENGTH;

  /** Buffer holding the per-line prefix active at the buffer start.
   * Indentation is included in this. The length of this is stored
   * in the logical block stack. */
  char[] prefix = new char[128];

  /** Buffer holding the total remaining suffix active at the buffer start.
   * The characters are right-justified in the buffer to make it easier
   * to output the buffer. The length is stored in the logical block stack. */
  char[] suffix = new char[128];

  static final int QUEUE_INIT_ALLOC_SIZE = 300; // FIXME

  /** A queue of pending operations.
   * This is primarily stored in the circular buffer queueInts.  There
   * are different kinds of operation types, and each operation can
   * require a variable number of elements in the buffer, depending on
   * the operation type.  Given an operation at 'index', the type
   * operation type code is 'getQueueType(index)' (one of the
   * QITEM_XXX_TYPE macros below), and the number of elements in the
   * buffer is 'getQueueSize(index)' (one of the QITEM_XXX_SIZE values
   * below).  You can think of the various QITEM_XXX_TYPEs as
   * "sub-classes" of queued operations, but instead of creating
   * actual Java objects, we allocate the objects' fields in the
   * queueInts and QueueStrings arrays, to avoid expensive object
   * allocation.  The special QITEM_NOP_TYPE is a used as a marker for
   * when there isn't enough space in the rest of buffer, so we have
   * to wrap around to the start.  The other QITEM_XXX macros are the
   * offsets of the various "fields" relative to the start index. */
  int[] queueInts = new int[QUEUE_INIT_ALLOC_SIZE];

  /** For simplicity, queueStrings is the same size as queueInts. */
  String[] queueStrings = new String[QUEUE_INIT_ALLOC_SIZE];
  /** Index in queueInts and queueStrings of oldest enqueued operation. */
  int queueTail;
  /** Number of elements (in queueInts and queueStrings) in use. */
  int queueSize;
  /** If >= 0, index (into queueInts) of current unclosed begin-block node.
   * This is a head of a linked linked of queued BLOCK_START for which
   * we haven't seen the matching BLOCK_END  */
  int currentBlock = -1;
  /** Number of startLogicalBlock - number of endLogicalBlock. */
  public int pendingBlocksCount;

  /**
   * The queue types and respective sizes are packed into a 32-bit integer.
   *
   * The most-significant 16-bits of a QITEM defines the type code. This type
   * code is defined by one of the QITEM_.+_TYPE fields below.
   * The least significant 8-bits of QITEM defines the type size. This size
   * is defined by one of the QITEM_.+_SIZE fields below.
   * Bits (8, 16] are currently used to store flags for their respective queue
   * types
   * The only exception to this rule is QITEM_NOP_TYPE, which acts as a marker
   * for when there isn't enough space in the rest of the buffer.
   * All other QITEMs have in the second word (at offset QITEM_POSN)
   * the corresponding text position as a POSN.
   */

  /** The first int of a QITEM contains its type code and size. */
  static final int QITEM_TYPE_AND_SIZE = 0;
  private int getQueueType(int index) { return queueInts[index] & 0xFF; }
  private int getQueueSize(int index) { return queueInts[index] >> 16; }
  /** Relative offset of POSN field of a QITEM. */
  static final int QITEM_POSN = 1;
  /** Size of "base part" of a QITEM. The type and size packing, plus the position */
  static final int QITEM_BASE_SIZE = 2;

  /** A dummy queue item used at the high end of the queue buffer
   * when there isn't enough space for the needed queue item. */
  static final int QITEM_NOP_TYPE = 0;

  /** "Abstract" type for beginning of section.
   * A section is from a block-start to a newline, from a newline to
   * the next newline (in the same block?), or from a newline to
   * the block end (?). */
  /*static final int QITEM_SECTION_START_TYPE = 1;*/
  static final int QITEM_SECTION_START_SIZE = QITEM_BASE_SIZE + 2;
  static final int QITEM_SECTION_START_DEPTH = QITEM_BASE_SIZE;
  static final int QITEM_SECTION_START_SECTION_END = QITEM_BASE_SIZE + 1;

  /** A newline queue item. */
  static final int QITEM_NEWLINE_TYPE = 2;
  static final int QITEM_NEWLINE_SIZE = QITEM_SECTION_START_SIZE + 1;
  static final int QITEM_NEWLINE_KIND = QITEM_SECTION_START_SIZE;
  public static final int NEWLINE_LINEAR = 'N';
  public static final int NEWLINE_LITERAL = 'L';
  public static final int NEWLINE_FILL = 'F';
  /** A non-nested ' ' gets an implicit NEWLINE_SPACE.
   * This is treated similarly to NEWLINE_FILL, but not quite. */
  public static final int NEWLINE_SPACE = 'S';
  public static final int NEWLINE_MISER = 'M';
  public static final int NEWLINE_MANDATORY = 'R';  // "required"

  static final int QITEM_INDENTATION_TYPE = 3;
  static final int QITEM_INDENTATION_SIZE = QITEM_BASE_SIZE + 2;
  static final int QITEM_INDENTATION_KIND = QITEM_BASE_SIZE;
  static final char QITEM_INDENTATION_BLOCK = 'B';
  static final char QITEM_INDENTATION_CURRENT = 'C';
  static final int QITEM_INDENTATION_AMOUNT = QITEM_BASE_SIZE + 1;

  /** A "block-start" queue item. */
  static final int QITEM_BLOCK_START_TYPE = 4;
  static final int QITEM_BLOCK_START_SIZE = QITEM_SECTION_START_SIZE + 3;
  /** If the QITEM_BLOCK_START_BLOCK_END < 0, it points to
   * the previous (outer) un-closed block-start.
   * If QITEM_BLOCK_START_BLOCK_END > 0, it points to the
   * corresponding block-end node.
   * In both cases the pointers are relative to the current BLOCK_START. */
  static final int QITEM_BLOCK_START_BLOCK_END = QITEM_SECTION_START_SIZE;
  static final int QITEM_BLOCK_START_PREFIX = QITEM_SECTION_START_SIZE + 1;
  static final int QITEM_BLOCK_START_SUFFIX = QITEM_SECTION_START_SIZE + 2;

  static final int QITEM_BLOCK_END_TYPE = 5;
  static final int QITEM_BLOCK_END_SIZE = QITEM_BASE_SIZE;

  static final int QITEM_TAB_TYPE = 6;
  static final int QITEM_TAB_SIZE = QITEM_BASE_SIZE + 3;
  static final int QITEM_TAB_FLAGS = QITEM_BASE_SIZE;
  static final int QITEM_TAB_IS_SECTION = 1;
  static final int QITEM_TAB_IS_RELATIVE = 2;
  static final int QITEM_TAB_COLNUM = QITEM_BASE_SIZE + 1;
  static final int QITEM_TAB_COLINC = QITEM_BASE_SIZE + 2;

  /**
   * Position markers are used to "name" shared parts of an expression. 
   * For instance in #1=(a #1# c), #1= is the position marker.
   * The GROUP_TYPE is used as a bit mask to determine from the type+size
   * packing whether this position marker is grouping a sub-object. See the
   * PAIR_END_TYPE comments below for a more detailed description of this.
   */
  static final int QITEM_POSNMARKER_TYPE = 7;
  static final int QITEM_POSNMARKER_GROUP_TYPE = 8;
  static final int QITEM_POSNMARKER_SIZE = QITEM_BASE_SIZE + 1;
  /** The datum label to use for this object, or 0 if no label.
   * The label is initially 0, indicating no back-references.
   * It is set to 1 when a back-reference is seen.
   * Finally resolveBackReferences sets it to a ordinal that gets written
   * out - i.e. the N in {@code #N#}.
   */
  static final int QITEM_POSNMARKER_LOC = QITEM_BASE_SIZE;

  /**
   * Represents a back reference to a value defined by a previous QITEM_POSNMARKER
   * @param posn The index into the {@link #queueInts} array of the position
   * marker
   * @return true if the position marker has been referenced.
   */
  static final int QITEM_BACKREF_TYPE = 8;
  static final int QITEM_BACKREF_SIZE = QITEM_BASE_SIZE + 1;
  static final int QITEM_BACKREF_TARGET = QITEM_BASE_SIZE;

  /**
   * Pair end types are also linked to position markers. If the position
   * marker is found to be grouping (i.e. that it references a non-initial
   * sublist) the printer must emit an extra closing parenthesis so that the
   * position marker correctly points to referenced sublist.
   * 
   * '(a *b c d*)..
   * 
   * For instance, if the '(b c d) sublist was found to be a reference, we would
   * need to add extra parenthesis around the sublist like so,
   * 
   * '(a . #N=(b c d))...
   * 
   * These types deal with the closing parenthesis of such cases.
   */
  static final int QITEM_PAIR_END_TYPE = 9;
  static final int QITEM_PAIR_END_SIZE = QITEM_BASE_SIZE + 1;
  static final int QITEM_PAIR_END_REF = QITEM_BASE_SIZE;

  /**
   * When performing a shared structure printing, with a prettyPrintingMode of
   * zero, the only queueItem tokens contained in the string are newline tokens.
   * This isn't enough for the back-reference resolver. This token is appended
   * to the end of an s-exp so that the resolver will be able consume all of the
   * expression.
   */
  static final int QITEM_EOE_TYPE = 10;
  static final int QITEM_EOE_SIZE = QITEM_BASE_SIZE;

  /**
   * To determine whether the position maker should group what it references.
   * This is necessary when the CDR of a non-initial pair has been found to be
   * referenced. We explout some unused space in the queue item
   * type+size packing to accomodate this information.
   * @param posn The position marker under test
   * @return true if this position marker should group
   */
  private boolean posnMarkerIsGrouping (int posn)
  {
    return ((queueInts[posn] >> QITEM_POSNMARKER_GROUP_TYPE) & 1) != 0;
  }

  /**
   * To determine whether a position marker has been referenced
   * @param posn The index into the {@link #queueInts} array of the position
   * marker
   * @return true if the position marker has been referenced.
   */
  private boolean posnMarkerActive (int posn)
  {
    return queueInts[posn + QITEM_POSNMARKER_LOC] != 0;
  }

  private int getSectionColumn()
  {
    return blocks[blockDepth+BLOCK_SECTION_COLUMN];
  }

  private int getStartColumn()
  {
    return blocks[blockDepth+BLOCK_START_COLUMN];
  }

  private int getPerLinePrefixEnd()
  {
    return blocks[blockDepth+BLOCK_PER_LINE_PREFIX_END];
  }

  private int getPrefixLength()
  {
    return blocks[blockDepth+BLOCK_PREFIX_LENGTH];
  }

  private int getSuffixLength()
  {
    return blocks[blockDepth+BLOCK_SUFFIX_LENGTH];
  }

  private int getSectionStartLine()
  {
    return blocks[blockDepth+BLOCK_SECTION_START_LINE];
  }

  boolean wordEndSeen;

  /** Note the end of a "word".  See {@link #writeWordStart}. */
  public void writeWordEnd ()
  {
    wordEndSeen = true;
  }

  /** Maybe write a word-separating space.
   * Specifically, write a space if the previous output
   * was {@link #writeWordEnd}.  Otherwise, do nothing.
   */
  public void writeWordStart ()
  {
    if (wordEndSeen)
      write(' ');
    wordEndSeen = false;
  }

  public void clearWordEnd ()
  {
    wordEndSeen = false;
  }

  /**
   * Write a character to the buffer. If we're pretty printing and the character
   * is a space character, two cases arise. If the character is a newline, then
   * enqueue a NEWLINE_LITERAL, which has to used. If the character is a space,
   * then we may at our discretion place a newline there if the printing
   * requires that we do so. Note that this doesn't mean we *will* print
   * a newline, just that we can. For other characters, we just populate the
   * buffer.
   * @param ch The character to write.
   */
  public void write (int ch)
  {
    wordEndSeen = false;
    //log("{WRITE-ch: "+((char)ch)+"}");
    if (isPassingThrough()) {
        if (ch == '\n' || ch == '\r')
            bufferStartColumn = 0;
        writeToBase(ch);
    }
    else if (ch == '\n' && prettyPrintingMode > 0)
      enqueueNewline(NEWLINE_LITERAL);
    else
      {
	ensureSpaceInBuffer(1);
	int fillPointer = bufferFillPointer;
	buffer[fillPointer] = (char) ch;
	bufferFillPointer = 1 + fillPointer;
	if (ch == ' ' && prettyPrintingMode > 1 && currentBlock < 0)
	  enqueueNewline(NEWLINE_SPACE);
      }
  }

  public void write (String str)
  {
    write(str, 0, str.length());
  }

  /**
   * Write a (sub)string to the output buffer. Newlines and spaces are handled,
   * and appropriate formatting token emitted in such cases.
   * 
   * @param str The string to use
   * @param start Where to start in the string
   * @param count The number of character to write from the string
   */
  public void write (String str, int start, int count)
  {
    wordEndSeen = false;
    //log("{WRITE-str: "+str.substring(start, start+count)+"}");
    if (isPassingThrough()) {
        for (int i = count; ; ) {
            if (--i < 0) {
                bufferStartColumn += count;
                break;
            }
            char ch = str.charAt(start+i);
            if (ch == '\r' || ch == '\n') {
                bufferStartColumn = count - (i + 1);
                break;
            }
        }
        writeToBase(str, start, count);
        return;
    }
    while (count > 0)
      {
	int cnt = count;
	// May allocate for space than we need (if the buffer gets fluhed).  FIXME
	int available = ensureSpaceInBuffer(count);
	if (cnt > available)
	  cnt = available;
	int fillPointer = bufferFillPointer;
	count -= cnt;
	while (--cnt >= 0)
	  {
	    char ch = str.charAt(start++);
	    if (ch == '\n' && prettyPrintingMode > 0)
	      {
		bufferFillPointer = fillPointer;
		enqueueNewline(NEWLINE_LITERAL);
		fillPointer = bufferFillPointer;
	      }
	    else
	      {
		buffer[fillPointer++] = (char) ch;
		if (ch == ' ' && prettyPrintingMode > 1 && currentBlock < 0)
		  {
		    bufferFillPointer = fillPointer;
		    enqueueNewline(NEWLINE_SPACE);
		    fillPointer = bufferFillPointer;
		  }
	      }
	  }
	bufferFillPointer = fillPointer;
      }
  }

  public void write (char[] str)
  {
    write(str, 0, str.length);
  }

  public void write (char[] str, int start, int count)
  {
    wordEndSeen = false;
    //log("{WRITE: "+new String(str, start, count)+"}");
    if (isPassingThrough()) {
        for (int i = count; ; ) {
            if (--i < 0) {
                bufferStartColumn += count;
                break;
            }
            char ch = str[start+i];
            if (ch == '\r' || ch == '\n') {
                bufferStartColumn = count - (i + 1);
                break;
            }
        }
        writeToBase(str, start, count);
        return;
    }
    int end = start + count;
  retry:
    while (count > 0)
      {
	// Look for newline.  Should be merged with following loop.  FIXME.
	for (int i = start;  i < end;  i++)
	  {
	    char c;
	    if (prettyPrintingMode > 0
		&& ((c = str[i]) == '\n'
		    || (c == ' ' && currentBlock < 0)))
	      {
		write(str, start, i - start); // Recurse
		write(c);
		start = i + 1;
		count = end - start;
		continue retry;
	      }
	  }

	for (;;)
	  {
	    int available = ensureSpaceInBuffer(count);
	    int cnt = available < count ? available : count;
	    int fillPointer = bufferFillPointer;
	    int newFillPtr = fillPointer + cnt;
	    for (int i = fillPointer;  i < newFillPtr;  i++)
	      buffer[i] = str[start++];
	    bufferFillPointer = newFillPtr;
	    count -= cnt;
	    if (count == 0)
	      break;
	  }
      }
  }

    private void writeToBase(char[] buf, int start, int count) {
        try {
            out.write(buf, start, count);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        } 
    }
    
    private void writeToBase(String str, int start, int count) {
        try {
            out.write(str, start, count);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        } 
    }

    private void writeToBase(String str) {
        writeToBase(str, 0, str.length());
    }
    private void writeToBase(int ch) {
        try {
            out.write(ch);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        } 
    }

  /**
   * A position marker is queued for every Scheme object that *could* refer
   * to itself (or other parts of the structure). 
   * Currently, this applies only to vectors and lists. For each
   * such object, the printer records it. All position markers are assumed
   * inactive (i.e. not back-referenced) until proven otherwise. Some position
   * markers are required to group new sublists, the grouping variable
   * notes this.
   * @return Integer The index in {@link #queueInts} where this marker resides
   *                 it's used for back-reference lookup.
   * @see #writeBackReference(int)
   */
  public int writePositionMarker (boolean grouping)
  {
    int result = enqueue(QITEM_POSNMARKER_TYPE, QITEM_POSNMARKER_SIZE);
    // A zero implies we haven't assigned this position marker a count yet
    queueInts[result + QITEM_POSNMARKER_LOC] = 0;
    // Are we marking a non-initial sublist?
    queueInts[result] |= (grouping ? 1 : 0) << QITEM_POSNMARKER_GROUP_TYPE;
    // Return the index of this position marker in the queueInts
    //log("POSNMARK: @"+result);
    return result;
  }

  /**
   * Enqueue a back-reference token to the formatting queue.
   *
   * When the display formatter has detected that an object is referring to an
   * "enclosing" object, a back-reference is queued. This makes the printer
   * emit suitable notation to demonstrate the shared structure. The position
   * marker this back-reference references is made active. If the structure
   * to be printed was really sharing before, it is after this method.
   * @param posn The index in the {@link #queueInts} array of the referenced
   * position marker.
   */
  public void writeBackReference (int posn)
  {
    if (!reallySharing)
      reallySharing = true;
    int result = enqueue(QITEM_BACKREF_TYPE, QITEM_BACKREF_SIZE);
    //log("BACKREF: @"+result+" references "+posn);
    queueInts[result + QITEM_BACKREF_TARGET] = posn;
    // The position marker this references is now active.
    queueInts[posn + QITEM_POSNMARKER_LOC] = 1;
  }

  /**
   * Enqueue a pair-end token to the formatting queue.
   *
   * When a position marker is grouping it creates a new sublist, which of
   * course needs a closing parenthesis that's not originally present in the
   * structure to be printed. This token informs the back reference resolver
   * to emit an extra closing parenthesis so that the reader will be happy.
   *
   * @param posn The index in the {@link #queueInts} array of the referenced
   * position marker.
   */
  public void writePairEnd (Integer posn)
  {
    int result = enqueue(QITEM_PAIR_END_TYPE, QITEM_PAIR_END_SIZE);
    //log("PAIREND: @"+result+" referencing: "+posn+" queueSize="+queueSize);
    queueInts[result + QITEM_PAIR_END_REF] = posn;
  }

  public void writeEndOfExpression ()
  {
    enqueue(QITEM_EOE_TYPE, QITEM_EOE_SIZE);    
  }

  private void pushLogicalBlock(int column,
				int perLineEnd,
				int prefixLength, int suffixLength,
				int sectionStartLine)
  {
    int newLength = blockDepth + LOGICAL_BLOCK_LENGTH;
    if (newLength >= blocks.length)
      {
	int[] newBlocks = new int[2 * blocks.length];
	System.arraycopy(blocks, 0, newBlocks, 0, blockDepth);
	blocks = newBlocks;
      }
    blockDepth = newLength;
    blocks[blockDepth + BLOCK_START_COLUMN] = column;
    blocks[blockDepth + BLOCK_SECTION_COLUMN] = column;
    blocks[blockDepth + BLOCK_PER_LINE_PREFIX_END] = perLineEnd;
    blocks[blockDepth + BLOCK_PREFIX_LENGTH] = prefixLength;
    blocks[blockDepth + BLOCK_SUFFIX_LENGTH] = suffixLength;
    blocks[blockDepth + BLOCK_SECTION_START_LINE] = sectionStartLine;
  }
  
  /**
   * 
   * @param column the column of the output port where this block starts
   * @param prefix the block's prefix
   * @param suffix the block's suffix
   */
  void reallyStartLogicalBlock(int column, String prefix, String suffix)
  {
    int perLineEnd = getPerLinePrefixEnd();
    int prefixLength = getPrefixLength();
    int suffixLength = getSuffixLength();
    pushLogicalBlock(column, perLineEnd, prefixLength, suffixLength,
		     lineNumber);
    setIndentation(column);
    if (prefix != null)
      {
	blocks[blockDepth + BLOCK_PER_LINE_PREFIX_END] = column;
	int plen = prefix.length();
	prefix.getChars(0, plen, this.suffix, column - plen);
      }
    if (suffix != null)
      {
	// Prepend the new suffix in front of the old suffix in this.suffix.
	// The suffix is stored at the "right" (high-index) end of
	// this.suffix to make it easier to prepend new suffixes.
	char[] totalSuffix = this.suffix;
	int totalSuffixLen = totalSuffix.length;
	int additional = suffix.length();
	int newSuffixLen = suffixLength + additional;
	if (newSuffixLen > totalSuffixLen)
	  {
	    int newTotalSuffixLen = enoughSpace(totalSuffixLen, additional);
	    this.suffix = new char[newTotalSuffixLen];
	    System.arraycopy(totalSuffix, totalSuffixLen - suffixLength,
			     this.suffix, newTotalSuffixLen - suffixLength,
			     suffixLength);
	    totalSuffixLen = newTotalSuffixLen;
	  }
	suffix.getChars(0, additional,
			totalSuffix, totalSuffixLen - newSuffixLen);
	blocks[blockDepth + BLOCK_SUFFIX_LENGTH] = newSuffixLen;
      }

  }

  int enqueueTab (int flags, int colnum, int colinc) // DONE
  {
    int addr = enqueue(QITEM_TAB_TYPE, QITEM_TAB_SIZE);
    queueInts[addr + QITEM_TAB_FLAGS] = flags;
    queueInts[addr + QITEM_TAB_COLNUM] = colnum;
    queueInts[addr + QITEM_TAB_COLINC] = colinc;
    return addr;
  }

  /** Calculate how much space to allocate for a buffer.
   * @param current the current size of the buffer
   * @param want how much more space is needed
   */
  private static int enoughSpace(int current, int want)
  {
    int doubled = 2 * current;
    int enough = current + ((5 * want) >> 2);
    return doubled > enough ? doubled : enough;
  }

  public void setIndentation (int column)
  {
    char[] prefix = this.prefix;
    int prefixLen = prefix.length;
    int current = getPrefixLength();
    int minimum = getPerLinePrefixEnd();
    if (minimum > column)
      column = minimum;
    if (column > prefixLen)
      {
	prefix = new char[enoughSpace(prefixLen, column - prefixLen)];
	System.arraycopy(this.prefix, 0, prefix, 0, current);
	this.prefix = prefix;
      }
    if (column > current)
      {
	for (int i = current;  i < column;  i++)
	  prefix[i] = ' ';
      }
    blocks[blockDepth + BLOCK_PREFIX_LENGTH] = column;
  }

  void reallyEndLogicalBlock ()
  {
    int oldIndent = getPrefixLength();
    blockDepth -= LOGICAL_BLOCK_LENGTH;  // Pop
    int newIndent = getPrefixLength();
    if (newIndent > oldIndent)
      {
	for (int i = oldIndent;  i < newIndent;  i++)
	  prefix[i] = ' ';
      }
  }

  /**
   * Enqueue a formatting token into {@link #queueInts}. 
   *
   * The kind and size parameters are packed into a 32-bit integer and then stored in the
   * QITEM_TYPE_AND_SIZE offset of this token's computed base address. If the
   * {@link #queueInts} array is not big enough to hold the new token, two cases
   * arise. If we're not handling a shared structure, the queue is expanded, and
   * the old formatting tokens are moved to the high end of the queue, leaving
   * new space at the front. If we are handling shared structure, the queue
   * size is double, and we continue to enqueue items from the old queueSize.
   * 
   * @param kind The type of formatting token
   * @param size The size of this formatting token
   * @return The address of the token in {@link #queueInts}
   */
  public int enqueue (int kind, int size)
  {
    int oldLength = queueInts.length;
    int endAvail = oldLength - queueTail - queueSize;
    // If there are 5 int's left at the end of the queue, and we need to
    // enqueue an item of size 7, the we don't "bend" the item around the
    // queue, we just place a dummy formatting token that consumes the rest
    // of this end, and start again from the beginning.
    if (endAvail > 0 && size > endAvail && !sharing)
      enqueue(QITEM_NOP_TYPE, endAvail);
    if (queueSize + size > oldLength)
      {
        int newLength = oldLength;
        int enough = enoughSpace(oldLength, size);

        int[] newInts;
        String[] newStrings;

        if (sharing) // then do a linear array expansion
          {
            newLength = oldLength;
            do
              {
                newLength <<= 1;
              } while (newLength < enough);

            //log("sharing expand: oldLength="+oldLength+" newLength="+newLength+" queueTail="+queueTail);
            
            newInts = new int[newLength];
            newStrings = new String[newLength];

            System.arraycopy(queueInts, 0, newInts, 0, queueSize);
            System.arraycopy(queueStrings, 0, newStrings, 0, queueStrings.length);
          }
        else // shift the old items to the top, and insert from the front of a doubled array
          {            
            newLength = enough;
            newInts = new int[newLength];
            newStrings = new String[newLength];
            
            //log("non-sharing expand: oldLength="+oldLength+" newLength="+newLength+" queueTail="+queueTail);
            
            int queueHead = queueTail + queueSize - oldLength;
            if (queueHead > 0)
              { // Wraps around.
                System.arraycopy(queueInts, 0, newInts, 0, queueHead);
                System.arraycopy(queueStrings, 0, newStrings, 0, queueHead);
              }
            int part1Len = oldLength - queueTail;
            int deltaLength = newLength - oldLength;
            System.arraycopy(queueInts, queueTail,
                             newInts, queueTail + deltaLength,
                             part1Len);
            System.arraycopy(queueStrings, queueTail,
                             newStrings, queueTail + deltaLength,
                             part1Len);

            if (currentBlock >= queueTail)
              currentBlock += deltaLength;
            queueTail += deltaLength;
          }
        queueInts = newInts;
        queueStrings = newStrings;
      }
    int addr = queueTail + queueSize;
    // Wrap around if we're using a circular queue
    if (!sharing && addr >= queueInts.length)
      addr -= queueInts.length;
    queueInts[addr + QITEM_TYPE_AND_SIZE] = kind | (size << 16);
    if (size > 1) // then it's not a NOP, so needs a position
      queueInts[addr + QITEM_POSN] = indexPosn(bufferFillPointer);
    //log("enqueue "+itemKindString(kind)+" size:"+size+" at:"+queueSize+enqueueExtraLog); enqueueExtraLog = "";
    queueSize += size;
    return addr;
  }

  /**
   * Enqueue a newline formatting token. 
   *
   * A pass is made through the formatting tokens up to this one, where section 
   * sizes are computed. If we're not sharing, then a tentative output is done 
   * when the current buffer cannot fit on one line.
   * @param kind The type of newline to enqueue
   */
  private void enqueueNewline (int kind)
  {
    wordEndSeen = false;
    int depth = pendingBlocksCount;
    //enqueueExtraLog = " kind:"+(char) kind;
    int newline = enqueue(QITEM_NEWLINE_TYPE, QITEM_NEWLINE_SIZE);
    queueInts[newline + QITEM_NEWLINE_KIND] = kind;
    queueInts[newline + QITEM_SECTION_START_DEPTH] = pendingBlocksCount;
    queueInts[newline + QITEM_SECTION_START_SECTION_END] = 0;
    int entry = queueTail;
    int todo = queueSize;
    while (todo > 0)
      {
	if (entry == queueInts.length)
	  entry = 0;
	if (entry == newline)
	  break;
	int type = getQueueType(entry);
	if ((type == QITEM_NEWLINE_TYPE
	     || type == QITEM_BLOCK_START_TYPE)
	    && queueInts[entry + QITEM_SECTION_START_SECTION_END] == 0
	    && depth <= queueInts[entry + QITEM_SECTION_START_DEPTH])
	  {
	    int delta = newline - entry;
	    if (delta < 0)
	      delta += queueInts.length;
	    queueInts[entry + QITEM_SECTION_START_SECTION_END] = delta;
	  }
	int size = getQueueSize(entry);
	todo -= size;
	entry += size;
      }
    if (!sharing) //!!
      maybeOutput (kind == NEWLINE_LITERAL || kind == NEWLINE_MANDATORY, false);
  }

  public final void writeBreak(int kind)
  {
    if (prettyPrintingMode > 0 || sharing) //!!
      enqueueNewline(kind);
  }

  public int enqueueIndent (char kind, int amount)
  {
    //enqueueExtraLog = " kind:"+kind+" amount:"+amount;
    int result = enqueue(QITEM_INDENTATION_TYPE, QITEM_INDENTATION_SIZE);
    queueInts[result + QITEM_INDENTATION_KIND] = kind;
    queueInts[result + QITEM_INDENTATION_AMOUNT] = amount;
    return result;
  }

  public void addIndentation(int amount, boolean current)
  {
    if (prettyPrintingMode > 0)
      enqueueIndent((current ? QITEM_INDENTATION_CURRENT
		     : QITEM_INDENTATION_BLOCK),
		    amount);
  }

  public void startLogicalBlock (String prefix, boolean perLine, String suffix)
  {
    // If the queue is empty, it is a good time to check if line-length etc
    // have been changed.
    if (queueSize == 0 && bufferFillPointer == 0)
      {
        Object llen = lineLengthLoc.get(null);
        if (llen == null)
          lineLength = 80;
        else
          lineLength = Integer.parseInt(llen.toString());

        Object mwidth = miserWidthLoc.get(null);
        if (mwidth == null || mwidth == Boolean.FALSE
            // For Common Lisp nil.  Should we use Language.isTrue() FIXME.
            || mwidth == LList.Empty)
          miserWidth = -1;
        else
          miserWidth = Integer.parseInt(mwidth.toString());

        Object indent = indentLoc.get(null);
        // if (indent == null || indent ...
      }
    if (prefix != null)
      write(prefix);
    if (prettyPrintingMode == 0)
      return;
    int start = enqueue (QITEM_BLOCK_START_TYPE,
			 QITEM_BLOCK_START_SIZE);
    queueInts[start + QITEM_SECTION_START_DEPTH] = pendingBlocksCount;
    queueStrings[start + QITEM_BLOCK_START_PREFIX]
      = perLine ? prefix : null;
    queueStrings[start + QITEM_BLOCK_START_SUFFIX] = suffix;
    pendingBlocksCount++;
    int outerBlock = currentBlock;
    if (outerBlock < 0)
      outerBlock = 0;
    else
      {
	outerBlock -= start;
	if (outerBlock > 0)
	  outerBlock -= queueInts.length;
      }
    queueInts[start + QITEM_BLOCK_START_BLOCK_END] = outerBlock;
    queueInts[start + QITEM_SECTION_START_SECTION_END] = 0;
    currentBlock = start;
  }

  public void endLogicalBlock ()
  {
    int end = enqueue (QITEM_BLOCK_END_TYPE, QITEM_BLOCK_END_SIZE);
    pendingBlocksCount--;
    if (currentBlock < 0)
      {
	// reallyStartLogicalBlock has been called for the matching
	// BEGIN_BLOCK, so it is no longer in the queue.  Instead it is in
	// the 'blocks' stack.
	int suffixLength = blocks[blockDepth+BLOCK_SUFFIX_LENGTH];
	int suffixPreviousLength
	  = blocks[blockDepth - LOGICAL_BLOCK_LENGTH + BLOCK_SUFFIX_LENGTH];
	if (suffixLength > suffixPreviousLength)
	  write(this.suffix,
		this.suffix.length - suffixLength,
		suffixLength - suffixPreviousLength);
	currentBlock = -1;
	return;
      }
    int start = currentBlock;
    int outerBlock = queueInts[start + QITEM_BLOCK_START_BLOCK_END];
    if (outerBlock == 0)
      currentBlock = -1;
    else
      {
	int qtailFromStart = queueTail - start;
	if (qtailFromStart > 0)
	  qtailFromStart -= queueInts.length;
	if (outerBlock < qtailFromStart)
	  {
	    // reallyStartLogicalBlock has been called for the outer block,
	    // so there is no currentBlock.
	    currentBlock = -1;
	  }
	else
	  {
	    // Make currentBlock absolute instead of relative.
	    outerBlock += start;
	    if (outerBlock < 0)
	      outerBlock += queueInts.length;
	    currentBlock = outerBlock;
	  }
      }
    String suffix = queueStrings[start + QITEM_BLOCK_START_SUFFIX];
    if (suffix != null)
      write(suffix);
    int endFromStart = end - start;
    if (endFromStart < 0) // wrap-around.
      endFromStart += queueInts.length;
    queueInts[start + QITEM_BLOCK_START_BLOCK_END] = endFromStart;
    //log("endLogicalBlock end:"+end+" start:"+start+" rel:"+endFromStart);
  }

  public void endLogicalBlock (String suffix)
  {
    if (prettyPrintingMode > 0)
      endLogicalBlock();
    else if (suffix != null)
      write(suffix);
  }

  // Tab support

  int computeTabSize (int tab, int sectionStart, int column) // DONE
  {
    int flags = queueInts[tab + QITEM_TAB_FLAGS];
    boolean isSection = (flags & QITEM_TAB_IS_SECTION) != 0;
    boolean isRelative = (flags & QITEM_TAB_IS_RELATIVE) != 0;
    int origin = isSection ? sectionStart : 0;
    int colnum = queueInts[tab + QITEM_TAB_COLNUM];
    int colinc = queueInts[tab + QITEM_TAB_COLINC];
    if (isRelative)
      {
	if (colinc > 1)
	  {
	    int newposn = column + colnum;
	    int rem = newposn % colinc;
	    if (rem != 0)
	      colnum += colinc = rem;
	  }
	return colnum;
      }
    else if (column <= colnum + origin)
      return column + origin - column;
    else
      return colinc - (column - origin) % colinc;
  }

  int indexColumn(int index)
  {
    int column = bufferStartColumn;
    int sectionStart = getSectionColumn();
    int endPosn = indexPosn(index);
    int op = queueTail;
    int todo = queueSize;
    while (todo > 0)
      {
	// If at end of queueInts, skip.
	if (op >= queueInts.length)
	  op = 0;
	int type = getQueueType(op);
	if (type != QITEM_NOP_TYPE)
	  {
	    int posn = queueInts[op + QITEM_POSN];
	    if (posn >= endPosn)
	      break;
	    if (type == QITEM_TAB_TYPE)
	      column += computeTabSize(op, sectionStart,
				       column + posnIndex (posn));
	    else if (type == QITEM_NEWLINE_TYPE
		     || type == QITEM_BLOCK_START_TYPE)
	      sectionStart
		= column + posnIndex(queueInts[op + QITEM_POSN]);
	  }
	int size = getQueueSize(op);
	todo -= size;
	op += size;
      }
    return column + index;
  }

  void expandTabs (int through)
  {
    int numInsertions = 0;
    int additional = 0;
    int column = bufferStartColumn;
    int sectionStart = getSectionColumn();
    int op = queueTail;
    int todo = queueSize;
    int blocksUsed = LOGICAL_BLOCK_LENGTH * pendingBlocksCount;
    while (todo > 0)
      {
	if (op == queueInts.length)
	  op = 0;
	if (op == through)
	  break;
	int type = getQueueType(op);
	if (type == QITEM_TAB_TYPE)
	  {
	    int index = posnIndex(queueInts[op + QITEM_POSN]);
	    int tabsize = computeTabSize (op, sectionStart, column + index);
	    if (tabsize != 0)
	      {
		// We use the blocks array for a temporary tab buffer.
		if (blocksUsed + 2 * numInsertions + 1 >= blocks.length)
		  {
		    int[] newBlocks = new int[2 * blocks.length];
		    System.arraycopy(blocks, 0, newBlocks, 0, blocks.length);
		    blocks = newBlocks;
		  }
		blocks[blocksUsed + 2 * numInsertions] = index;
		blocks[blocksUsed + 2 * numInsertions + 1] = tabsize;
		numInsertions++;
		additional += tabsize;
		column += tabsize;
	      }
	  }
	else if (op == QITEM_NEWLINE_TYPE || op == QITEM_BLOCK_START_TYPE)
	  {
	    sectionStart = column + posnIndex(queueInts[op + QITEM_POSN]);
	  }
	int size = getQueueSize(op);
	todo -= size;
	op += size;
      }
    if (numInsertions > 0)
      {
	int fillPtr = bufferFillPointer;
	int newFillPtr = fillPtr + additional;
	char[] buffer = this.buffer;
	char[] newBuffer = buffer;
	int length = buffer.length;
	int end = fillPtr;
	if (newFillPtr > length)
	  {
	    int newLength = enoughSpace (fillPtr, additional);
	    newBuffer = new char[newLength];
	    this.buffer = newBuffer;
	  }
	bufferFillPointer = newFillPtr;
	bufferOffset -= additional;
	for (int i = numInsertions;  --i >= 0; )
	  {
	    int srcpos = blocks[blocksUsed + 2 * i];
	    int amount = blocks[blocksUsed + 2 * i + 1];
	    int dstpos = srcpos + additional;
	    System.arraycopy(buffer, srcpos, newBuffer, dstpos, end - srcpos);
	    for (int j = dstpos - amount;  j < dstpos;  j++)
	      newBuffer[j] = ' ';
	    additional -= amount;
	    end = srcpos;
	  }
	if (newBuffer != buffer)
	  System.arraycopy(buffer, 0, newBuffer, 0, end);
      }
  }

  // stuff to do the actual outputting
  
  /**
   * Make sure we can write into the buffer without overflowing it. 
   *
   * If the current {@link #bufferFillPointer} is greater than the line length, 
   * do a tentative output.
   * @param want How much space we need
   * @return The amount of space available, possible after flushes or expansion
   */
  int ensureSpaceInBuffer (int want)
  {
    char[] buffer = this.buffer;
    int length = buffer.length;
    int fillPtr = bufferFillPointer;
    int available = length - fillPtr;
    if (available > 0)
      return available;
    else if (out != null && fillPtr > lineLength && !sharing)
      {
	if (! maybeOutput(false, false))
	  outputPartialLine();
	return ensureSpaceInBuffer(want);
      }
    else
      {
	int newLength = enoughSpace(length, want);
	char[] newBuffer = new char[newLength];
	this.buffer = newBuffer;
	for (int i = fillPtr;  --i >= 0; )
	  newBuffer[i] = buffer[i];
	return newLength - fillPtr;
      }
  }

  /**
   *
   * @param forceNewlines is true if we're printing a "literal" newline or if
   *                      the printer has decided that we need to force a
   *                      newline for a sensible formatting.
   * @param flushing is true when the buffer is actually written to the output
   *                 port. This occurs on {@link #flush()} operations for instance.
   * @return true if something has been output, false otherwise.
   */
  boolean maybeOutput(boolean forceNewlines, boolean flushing)
  {
    boolean outputAnything = false;
    //log("maybeOutput("+forceNewlines+"):");  dumpQueue();
  loop:
    while (queueSize > 0)
      {
	if (queueTail >= queueInts.length)
	  queueTail = 0;
	int next = queueTail;
	int type = getQueueType(next);
	switch (type)
	  {
	  case QITEM_NEWLINE_TYPE:
	    boolean cond;
            int fits = -1;
	    switch (queueInts[next+QITEM_NEWLINE_KIND])
	      {
	      default: // LINEAR, LITERAL, or MANDATORY:
		cond = true;
		break;
	      case NEWLINE_MISER:
		cond = isMisering();
		break;
	      case NEWLINE_FILL:
		if (isMisering()
		    || (lineNumber > getSectionStartLine()))
		  {
		    cond = true;
		    break;
		  }
		/// ... fall through to ...
	      case NEWLINE_SPACE:
		int end = queueInts[next+QITEM_SECTION_START_SECTION_END];
		if (end == 0)
		  end = -1;
		else
		  { // convert relative->absolute.
		    end = next + end;
		    if (end >= queueInts.length)
		      end -= queueInts.length;
		  }
		fits = fitsOnLine(end, forceNewlines);
		if (fits > 0)
		  cond = false;
		else if (fits < 0 || flushing)
		  cond = true;
		else
		  break loop;
		break;
	      }
	    if (cond)
	      {
		outputAnything = true;
		if (flushing && fits == 0)
                  outputPartialLine();
                else
                  outputLine(next);
	      }
	    break;
	  case QITEM_INDENTATION_TYPE:
	    if (! isMisering())
	      {
		int kind = queueInts[next+QITEM_INDENTATION_KIND];
		int indent = queueInts[next+QITEM_INDENTATION_AMOUNT];
		if (kind == QITEM_INDENTATION_BLOCK)
		  indent += getStartColumn();
		else
		  indent += posnColumn(queueInts[next+QITEM_POSN]);
		//log("setIndent from "+next+": "+queueInts[next+QITEM_INDENTATION_AMOUNT]+" column:"+indent);
		setIndentation(indent);
	      }
	    break;
	  case QITEM_BLOCK_START_TYPE:
	    int start = next;
	    int end = queueInts[next + QITEM_SECTION_START_SECTION_END];
	    // Convert relative offset to absolute index:
	    end = end > 0 ? (end + next) % queueInts.length : -1;
	    fits = fitsOnLine (end, forceNewlines);
	    //log("block-start @"+next+" end:"+end+" force:"+forceNewlines+" fits:"+fits);
	    if (fits > 0)
	      {
		// Just nuke the whole logical block and make it look
		// like one nice long literal.
		int endr = queueInts[next + QITEM_BLOCK_START_BLOCK_END];
		// make absolute:
		next = (endr + next) % queueInts.length;
		expandTabs(next);
		queueTail = next;
		queueSize -= endr;
		//log("remove block -> next:"+next+" endr:"+endr+" qSize:"+queueSize);
	      }
	    else if (fits < 0 || flushing)
	      {
		String prefix = queueStrings[next + QITEM_BLOCK_START_PREFIX];
		String suffix = queueStrings[next + QITEM_BLOCK_START_SUFFIX];
		//log("reallyStartLogicalBlock: "+blockDepth+" at:"+next);
		reallyStartLogicalBlock (posnColumn(queueInts[next + QITEM_POSN]),
					 prefix, suffix);
	      }
	    else // Don't know.
	      break loop;
	    if (currentBlock == start)
	      currentBlock = -1;
	    break;
	  case QITEM_BLOCK_END_TYPE:
	    //log("reallyEndLogicalBlock: "+blockDepth+" at:"+next);
	    reallyEndLogicalBlock();
	    break;
	  case QITEM_TAB_TYPE:
	    expandTabs(next);
	    break;
	  }
	int size = getQueueSize(queueTail);
	queueSize -= size;
	//log("maybeWrite size: "+size+" ->"+queueSize);
	queueTail = next + size;
      }
    return outputAnything;
  }

  protected int getMiserWidth () // DONE
  {
    // CommonLisp:  Use *print-miser-width*.
    return miserWidth;
  }

  boolean isMisering() // DONE
  {
    int mwidth = getMiserWidth ();
    return (mwidth > 0
	    && lineLength - getStartColumn() <= mwidth);
  }

  int getMaxLines ()
  {
    // Should be value of CommonLisp *print-lines*.
    return -1;
  }

  boolean printReadably()
  {
    // Should be value of CommonLisp *print-readably*.
    return true;
  }

  /** Return 1 if true;  -1 if false; 0 if don't know. */
  int fitsOnLine (int sectionEnd, boolean forceNewlines) // DONE
  {
    int available = lineLength;
    if (! printReadably() && getMaxLines() == lineNumber)
      {
	available -= 3;  // For the " ..".
	available -= getSuffixLength();
      }
    if (sectionEnd >= 0)
      return posnColumn(queueInts[sectionEnd + QITEM_POSN]) <= available ? 1 : -1;
    if (forceNewlines)
      return -1;
    if (indexColumn(bufferFillPointer) > available)
      return -1;
    return 0; // don't know.
  }

  public void lineAbbreviationHappened()
  {
    // Hook.
  }

  /** Output a new line.
   * @param newline index of a newline queue item
   */
  void outputLine (int newline)
  {
    char[] buffer = this.buffer;
    int kind = queueInts[newline + QITEM_NEWLINE_KIND];
    boolean isLiteral = kind == NEWLINE_LITERAL;
    int amountToConsume = posnIndex(queueInts[newline + QITEM_POSN]);
    int amountToPrint;
    if (isLiteral)
      amountToPrint = amountToConsume;
    else
      {
	// Skip trailing spaces.
	for (int i = amountToConsume; ; )
	  {
	    if (--i < 0)
	      {
		amountToPrint = 0;
		break;
	      }
	    if (buffer[i] != ' ')
	      {
		amountToPrint = i + 1;
		break;
	      }
	  }
      }
    writeToBase(buffer, 0, amountToPrint);
    int lineNumber = this.lineNumber;
    //log("outputLine#"+lineNumber+": \""+new String(buffer, 0, amountToPrint)+"\" curBlock:"+currentBlock);
    lineNumber++;
    if (! printReadably())
      {
	int maxLines = getMaxLines();
	if (maxLines > 0 && lineNumber >= maxLines)
	  {
	    writeToBase(" ..");
	    int suffixLength = getSuffixLength();
	    if (suffixLength != 0)
	      {
		char[] suffix = this.suffix;
		int len = suffix.length;
		writeToBase(suffix, len - suffixLength, suffixLength);
	      }
	    // (throw 'line-limit-abbreviation-happened t))
	    lineAbbreviationHappened();
	  }
      }
    this.lineNumber = lineNumber;
    writeToBase('\n');
    bufferStartColumn = 0;
    int fillPtr = bufferFillPointer;
    int prefixLen = isLiteral ? getPerLinePrefixEnd() : getPrefixLength();
    int shift = amountToConsume - prefixLen;
    int newFillPtr = fillPtr - shift;
    char[] newBuffer = buffer;
    int bufferLength = buffer.length;
    if (newFillPtr > bufferLength)
      {
	newBuffer = new char[enoughSpace(bufferLength,
					 newFillPtr - bufferLength)];
	this.buffer = newBuffer;
      }
    System.arraycopy(buffer, amountToConsume, newBuffer, prefixLen,
		     fillPtr - amountToConsume);
    System.arraycopy(prefix, 0, newBuffer, 0, prefixLen);
    bufferFillPointer = newFillPtr;
    bufferOffset += shift;
    if (! isLiteral)
      {
	blocks[blockDepth+BLOCK_SECTION_COLUMN] = prefixLen;
	blocks[blockDepth+BLOCK_SECTION_START_LINE] = lineNumber;
      }
  }

  void outputPartialLine ()
  {
    //log("outputPartialLine");
    int tail = queueTail;
    while (queueSize > 0 && getQueueType(tail) == QITEM_NOP_TYPE)
      {
	int size = getQueueSize(tail);
	queueSize -= size;
	tail += size;
	if (tail == queueInts.length)
	  tail = 0;
	queueTail = tail;
      }
    int fillPtr = bufferFillPointer;
    int count = queueSize > 0 ? posnIndex (queueInts[tail + QITEM_POSN])
      : fillPtr;
    int newFillPtr = fillPtr - count;
    if (count <= 0)
        throw new Error("outputPartialLine called when nothing can be output.");
    writeToBase(buffer, 0, count);
    bufferFillPointer = count; // For the sake of the following:
    bufferStartColumn = getColumnNumber();
    System.arraycopy(buffer, count, buffer, 0, newFillPtr);
    bufferFillPointer = newFillPtr;
    bufferOffset += count;
  }

  /**
   * Walk through the queue, emitting print circle notation as described in
   * SRFI-38. 
   *
   * For each position marker encountered, if the position marker
   * has been referenced, emit the #N= notation. For each back-reference, find
   * the corresponding position marker and emit the #N# notation. A Gap Buffer
   * is employed to facilitate amortised linear buffer insertion when emitting
   * the new print-circle notation. The method populates the rest of the buffer
   * contents by using the QITEM_POSN information in SRFI-38 unrelated formatting
   * tokens. When the print-circle notation has been emitted, future QITEM_POSN's
   * need to be updated, this is achieved by maintaining a delta variable that
   * records how many extra characters have been emitted to the output buffer.
   */
  public void resolveBackReferences ()
  {
    if (!reallySharing)
      return;
    int posnMarkerCount = 0;
    GapBuffer gbuffer = new GapBuffer(buffer, buffer.length);
    // The delta variable records how many extra characters have been emitted
    // to the {@link #buffer}.
    int delta = 0;
    // Used to compute the relative delta of an SRFI-38 specific token.
    int oldDelta;
    int relativeAddress;   

    //log ("resolveBackReferences: Starting at tail = "+queueTail);
    int tail = queueTail;
    int todo = queueSize;
    while (todo > 0)
      {
	if (tail >= queueInts.length) // then wrap around
	  tail = 0;

	int next = tail;
	int type = getQueueType(next);

        // The QITEM_POSN offset will return the position this QITEM occupies
        // in the output buffer. Since this QITEM is not SRFI-38 specific, the
	// printer must output all characters up to this token into the new
	// buffer.
        if (type != QITEM_NOP_TYPE)
          gbuffer.addUpTo(posnIndex(queueInts[next + QITEM_POSN]));

	switch (type)
	  {
	  case QITEM_POSNMARKER_TYPE:
	    oldDelta = delta;
	    // A position marker is active if it has been back-referenced.
	    if (posnMarkerActive(next))
	      {
		if (posnMarkerIsGrouping(next))
		  {
		    gbuffer.add('.');
		    gbuffer.add(' ');
		    delta += 2;
		  }
		gbuffer.add('#');
		// Set this position markers reference count if it hasn't been set
		// before. Note: all position marker default to a count of one.
		if (queueInts[next + QITEM_POSNMARKER_LOC] == 1)
		  {
		    queueInts[next + QITEM_POSNMARKER_LOC] = posnMarkerCount++;
		  }
		// The index that the referenced position marker resides in
		int reference = queueInts[next + QITEM_POSNMARKER_LOC];
		// The reference could be of arbitrary magnitude, it's typically
		// a unit digit however.
		delta += gbuffer.add(reference);
		gbuffer.add('=');
		delta += 2; // For the '#' and '=' characters

		if (posnMarkerIsGrouping(next))
		  {
		    gbuffer.add('(');
		    delta++;
		    // XXX Should we start a new logical block? Doesn't look great if
		    // you try it...
		  }

		// Compute the *relative* position of this QITEM
		//log("POSN-MARKER: queueInts[" + next + " + " + QITEM_POSN + "] = " + (delta - oldDelta));
		queueInts[next + QITEM_POSN] += delta - oldDelta;
	      }
	    break;
	  case QITEM_BACKREF_TYPE:
	    oldDelta = delta;
	    // The index that the referenced position marker resides in
	    relativeAddress = queueInts[next + QITEM_BACKREF_TARGET];
	    //log("RESOLVE-BACKREF: rel=" + relativeAddress + " @:" + next + "(" + (next + relativeAddress) + ")");
	    // The count refers to the N digit in #N= notation. Starts at one.
	    int count = queueInts[relativeAddress + QITEM_POSNMARKER_LOC];
	    gbuffer.add('#');
	    // The reference could be of arbitrary magnitude, it's typically
	    // a unit digit however.
	    delta += gbuffer.add(count);
	    gbuffer.add('#');
	    delta += 2; // For the '=' and '#' characters
			// New characters have been added, so this items output position must
			// change.
	    queueInts[next + QITEM_POSN] += delta - oldDelta;
	    break;
	  case QITEM_PAIR_END_TYPE:
	    relativeAddress = queueInts[next + QITEM_PAIR_END_REF];
	    //log("RESOLVE-PAIREND: rel=" + relativeAddress + "@:" + next + "(" + (next + relativeAddress) + ")");
	    if (posnMarkerActive(relativeAddress))
	      {
		gbuffer.add(')');
		delta++;
		queueInts[next + QITEM_POSN] += 1;
	      }
	    break;
	  default:
	    // Now update the token position information with the current delta.
	    queueInts[next + QITEM_POSN] += delta;
	    break;
	  }
	int size = getQueueSize(tail);
	// "Dequeue" this token
	todo -= size;
	// Point to the next token
	tail = next + size;
      }
    // The delta variable records how many extra characters have been emitted.
    // The bufferFillPointer points to the end of the characters to be flushed
    bufferFillPointer += delta;
    // The Gap Buffer will remove its gap and present the newly created buffer.
    buffer = gbuffer.restoreBuffer();
    posnMarkerCount = 1;
  }

  public void forcePrettyOutput ()
  {
    maybeOutput(false, true);
    if (bufferFillPointer > 0)
      outputPartialLine();
    expandTabs(-1);
    bufferStartColumn = getColumnNumber();
    writeToBase(buffer, 0, bufferFillPointer);
    bufferFillPointer = 0;
    queueSize = queueTail = bufferOffset = 0;
  }

  public void flush()
  {
    if (out == null)
      return;
    try
      {
	forcePrettyOutput();
	out.flush();
      }
    catch (IOException ex)
      {
	throw new RuntimeException(ex.toString());
      }
  }

  public void close()  throws IOException
  {
    if (out != null)
      { 
	forcePrettyOutput();
        out.close();
        out = null;
      }
    buffer = null;
  }

  /** Flush and close this local Writer, but not underlying Writers. */
  public void closeThis()  throws IOException
  {
    if (out != null)
      { 
	forcePrettyOutput();
        out = null;
      }
    buffer = null;
  }

  /** Get zero-origin column number, or -1 if unknown.
   * Not meaningful if {@code prettyPrintingMode > 0}. */
  public int getColumnNumber ()
  {
    int i = bufferFillPointer;
    for (;;)
      {
	if (--i < 0)
	  return bufferStartColumn + bufferFillPointer;
	char ch = buffer[i];
	if (ch == '\n' || ch == '\r')
	  return bufferFillPointer - (i+1);
      }
  }

  public void setColumnNumber (int column)
  {
    bufferStartColumn += column - getColumnNumber ();
  }

  public void clearBuffer ()
  {
    bufferStartColumn = 0;
    bufferFillPointer = 0;
    lineNumber = 0;
    bufferOffset = 0;
    blockDepth = LOGICAL_BLOCK_LENGTH;
    queueTail = 0;
    queueSize = 0;
    pendingBlocksCount = 0;
  }
  
  private static final class GapBuffer
  {
    char[] buffer;
    char[] existingBuffer;
    int point;
    int existingIndex;
    int gapSize;

    public GapBuffer (char[] existing, int startSize)
    {
      this.buffer = new char[startSize];
      this.point = 0;
      this.existingIndex = 0;
      this.existingBuffer = existing;
    }

    /**
     * The point represents where new characters will be buffered.
     * @return The index that represents where new characters will be buffered.
     */
    public int getPoint ()
    {
      return point;
    }

    /**
     * Add a character to the buffer.
     * @param ch The character to add
     */
    public void add (char ch)
    {
      if (point + 1 >= buffer.length)
	{
	  expandBuffer(1);
	}
      buffer[point++] = ch;
    }

    /**
     * Add a non-negative integer to the buffer.
     * @param i The non-negative integer to add
     * @return The number of digits in the integer
     */
    public int add (int i)
    {
      int ndigits = 1;
      if (i >= 10)
	{	  
	  ndigits += add(i / 10);
	  i %= 10;
	}
      add((char)('0' + i));
      return ndigits;
    }

    /**
     * Copy characters from the existing buffer to the gap buffer.
     * @param end the upper index of the characters to copy
     */
    public void addUpTo(int end)
    {
      int n = end - existingIndex;
      if (point + n >= buffer.length)
	{
	  expandBuffer(n);
	}
      while (existingIndex < end)
	{
	  buffer[point++] = existingBuffer[existingIndex++];
	}
    }

    /**
     * @return The buffer with the gap removed
     */
    public char[] restoreBuffer ()
    {
      char[] retBuffer = new char[buffer.length];
      System.arraycopy(buffer, 0, retBuffer, 0, point);
      return retBuffer;
    }

    private void expandBuffer(int n)
    {
      int newLength = buffer.length;
      int minimum = newLength + n;
      do
	{
	  newLength <<= 1;
	} while (newLength < minimum);

      char[] newBuffer = new char[newLength];
      System.arraycopy(buffer, 0, newBuffer, 0, point);
      buffer = newBuffer;
    }
  }
  
  /*
  public static PrintWriter log;
  static {
    try { log = new PrintWriter(new FileOutputStream("/tmp/pplog")); }
    catch (Throwable ex) { ex.printStackTrace(); }
  }
  void log(String str)
  {
    log.println(str);
    log.flush();
  }
  void dumpQueue()
  {
    log.println("Queue tail:"+queueTail+" size:"+queueSize
		+" length:"+queueInts.length+" startCol:"+bufferStartColumn);
    dumpQueue(queueTail, queueSize, log);
  }

  void dumpQueue(int start, int todo, PrintWriter out)
  {
    int bufIndex = 0;
    while (todo > 0)
      {
	if (start == queueInts.length)
	  start = 0;
	if (start < 0 || start >= queueInts.length)
	  {
	    out.print('@');	out.print(start);  out.print(": ");
	    out.print("out of bounds - queueInts length is ");
	    out.println(queueInts.length);
	    break;
	  }
	int type = getQueueType(start);
	int size = getQueueSize(start);
	if (type != QITEM_NOP_TYPE)
	  {
	    int newIndex = posnIndex(queueInts[start+QITEM_POSN]);
	    int count = newIndex - bufIndex;
	    if (count > 0)
	      {
		out.print(count); out.print(" chars: \"");
		out.write(buffer, bufIndex, count);
		out.println('\"');
		bufIndex = newIndex;
	      }
	  }
	out.print('@');	out.print(start);  out.print(": ");
	out.print("type:");  out.print(type);
	switch (type)
	  {
	  case QITEM_NEWLINE_TYPE:
	    out.print("(newline)");  break;
	  case QITEM_INDENTATION_TYPE:
	    out.print("(indentation)");  break;
	  case QITEM_BLOCK_START_TYPE:
	    out.print("(block-start)");  break;
	  case QITEM_BLOCK_END_TYPE:
	    out.print("(block-end)");  break;
	  case QITEM_TAB_TYPE:  out.print("(tab)");
	    break;
          case QITEM_POSNMARKER_TYPE:
            out.print("(posnmarker)");  break;
          case QITEM_BACKREF_TYPE:
            out.print("(backref)");  break;
	  case QITEM_NOP_TYPE:
	    out.print("(nop)");  break;
	  }
	out.print(" size:");  out.print(size);
	out.print(";  @");  out.print(start+QITEM_POSN);
	if (type != QITEM_NOP_TYPE)
	  {
	    out.print(": posn:");
	    int posn = queueInts[start+QITEM_POSN];
	    out.print(posn);
	    out.print(" index:");
	    out.println(posnIndex(posn));
	  }
	if (type == QITEM_NEWLINE_TYPE
	    || type == QITEM_BLOCK_START_TYPE)
	    
	  {
	    out.print('@');  out.print(start+QITEM_SECTION_START_DEPTH);
	    out.print(": - depth:");
	    out.print(queueInts[start+QITEM_SECTION_START_DEPTH]);
	    out.print(";  @");
	    out.print(start+QITEM_SECTION_START_SECTION_END);
	    out.print(": section-end:");
	    out.println(queueInts[start+QITEM_SECTION_START_SECTION_END]);
	  }
	switch (type)
	  {
	  case QITEM_BLOCK_START_TYPE:
	    printQueueWord(start, QITEM_BLOCK_START_BLOCK_END, "block-end", out);
	    printQueueStringWord(start, QITEM_BLOCK_START_PREFIX, "prefix", out);
	    printQueueStringWord(start, QITEM_BLOCK_START_SUFFIX, "suffix", out);
	    break;
	  case QITEM_NEWLINE_TYPE:
	    out.print('@');
	    out.print(start+QITEM_NEWLINE_KIND);
	    out.print(": - kind: ");
	    int kind = queueInts[start+QITEM_NEWLINE_KIND];
	    String skind = "???";
	    switch (kind)
	      {
	      case NEWLINE_LINEAR:    skind = "linear";    break;
	      case NEWLINE_LITERAL:   skind = "literal";   break;
	      case NEWLINE_FILL:      skind = "fill";      break;
	      case NEWLINE_SPACE:     skind = "space";      break;
	      case NEWLINE_MISER:     skind = "miser";     break;
	      case NEWLINE_MANDATORY: skind = "mandatory"; break;
	      }
	    out.print(kind);
	    out.print('(');
	    out.print(skind);
	    out.println(')');
	    break;
          case QITEM_INDENTATION_TYPE:
	    printQueueWord(start, QITEM_INDENTATION_KIND, "kind", out);
	    printQueueWord(start, QITEM_INDENTATION_AMOUNT, "amount", out);
	    break;
	  default:
	    for (int i = 2;  i < size;  i++)
	      printQueueWord(start, i, "word#"+i, out);
	  }
	todo -= size;
	start += size;
      }
    int count = bufferFillPointer - bufIndex;
    if (count > 0)
      {
	out.print(count); out.print(" chars: \"");
	out.write(buffer, bufIndex, count);
	out.println('\"');
      }
  }

  private void printQueueWord(int start, int offset,
			      String fname, PrintWriter out)
  {
    out.print('@');
    out.print(start+offset);
    out.print(": - ");
    out.print(fname);
    out.print(": ");
    out.println(queueInts[start+offset]);
  }

  private void printQueueStringWord(int start, int offset,
				    String fname, PrintWriter out)
  {
    out.print('@');
    out.print(start+offset);
    out.print(": - ");
    out.print(fname);
    out.print(": ");
    String str = queueStrings[start+offset];
    if (str == null)
      out.println("null");
    else
      {
	out.print('\"');
	out.print(str);
	out.print('\"');
	out.print(" length: ");
	out.println(str.length());
      }
  }

  void check (String where)
  {
    String msg = null;
    if (currentBlock != -1
	&& ! (currentBlock < queueInts.length
	      && currentBlock >= queueTail
	      ? currentBlock < queueTail + queueSize
	      : currentBlock < queueTail + queueSize - queueInts.length))
      msg = ("currentBlock ("+currentBlock
	     +") >= queue length ("+queueInts.length+")");
    if (msg != null)
      {
	if (where != null)
	  msg = "in "+where+": "+msg;
	log.println(msg);
	dumpQueue(); 
	log.flush();
	throw new Error(msg);
      }
  }

  String itemKindString (int kind)
  {
    switch (kind)
      {
      case QITEM_NOP_TYPE:  return "nop";
      case QITEM_NEWLINE_TYPE:  return "newline";
      case QITEM_INDENTATION_TYPE:  return "indentation";
      case QITEM_BLOCK_START_TYPE:  return "block-start";
      case QITEM_BLOCK_END_TYPE:  return "block-end";
      case QITEM_TAB_TYPE:  return "tab";
      default: return "("+kind+" - unknown)";
      }
  }
  String enqueueExtraLog = "";
  */
}
