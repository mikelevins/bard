package net.bardcode;
import gnu.kawa.functions.DisplayFormat;
import gnu.lists.*;
import gnu.mapping.*;
import net.bardcode.Structure;
import net.bardcode.Box;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.pcollections.HashPMap;

public class Print extends DisplayFormat
{

    public static final Print bardDisplayFormat
        = new Print();
    public static final DisplayFormat bardWriteSimpleFormat
        = new Print();
    public static final DisplayFormat bardWriteFormat
        = new Print();
    public static final DisplayFormat bardWriteSharedFormat
        = new Print();

    public static DisplayFormat getBardFormat()
    {
        return new Print();
    }

    static {
        bardWriteFormat.checkSharing = 0;
        bardWriteSharedFormat.checkSharing = 1;
    }
    
    public Print ()
    {
        super(true, 'B');
    }

    @Override
    public void writeBoolean(boolean v, Consumer out)
    {
        if (v) {write("true",out);} else {write("false",out);}
    }

    public void writeList(LList value, OutPort out){
        if (value == LList.Empty) {
            write("nothing",out);
        } else { super.writeList(value,out); }

    }

    public void writeObjectRaw(Object obj, Consumer out){
        if (obj instanceof Structure){
            String nm = ((Structure)obj).getName();
            write(nm,out);
        } else if (obj == Structure.undefined) {
            write("undefined",out);
        } else if (obj instanceof kawa.lang.Lambda) {
            write("method",out);
        } else if (obj instanceof net.bardcode.Box) {
            net.bardcode.Box box = (net.bardcode.Box)obj;
            write("#box(",out);
            super.writeObjectRaw(box.getValue(),out);
            write(")",out);
        } else if (obj instanceof net.bardcode.BardClass) {
            net.bardcode.BardClass cl = (net.bardcode.BardClass)obj;
            Symbol nm = cl.getName();
            String nmStr = nm.getName();
            write(nmStr,out);
        } else if (obj instanceof net.bardcode.Function) {
            net.bardcode.Function fn = (net.bardcode.Function)obj;
            write("(-> ",out);
            List sig = fn.getSignature();
            Iterator it = sig.iterator();
            while (it.hasNext()){
                Object elt = it.next();
                write(" ",out);
                writeObjectRaw(elt,out);
            }
            write(")",out);
        } else if (obj instanceof net.bardcode.Protocol) {
            net.bardcode.Protocol proto = (net.bardcode.Protocol)obj;
            Symbol nm = proto.getName();
            String nmStr = nm.getName();
            write("(protocol ",out);
            write(nmStr,out);
            HashPMap fns = proto.getFunctions();
            Set<Map.Entry> entries = fns.entrySet();
            Iterator it = entries.iterator();
            while (it.hasNext()){
                Map.Entry elt = (Map.Entry)it.next();
                Symbol fnm = (Symbol)elt.getKey();
                Function fn = (Function)elt.getValue();
                write(" ",out);
                writeObjectRaw(fnm,out);
                write(" ",out);
                writeObjectRaw(fn,out);
            }
            write(")",out);
            
        } else {
            super.writeObjectRaw(obj,out);
        }
    }
}
