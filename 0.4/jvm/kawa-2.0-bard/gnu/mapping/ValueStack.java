// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.lists.*;

public class ValueStack extends Values.FromTreeList {
    public void clear() {
        buffer.oindex = 0;
        buffer.clear();
    }
}
