// Bard representations of structures

package net.bardcode;
import gnu.mapping.*;

// instances of NativeStructure are Bard objects that represent
// built-in Java and Kawa structures.

public abstract class NativeStructure extends Structure {
    Class nativeClass;

    public NativeStructure(String nm, Class natClass){
        name = nm;
        nativeClass = natClass;
    }
}

