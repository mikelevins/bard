package kawa;

import kawa.Shell;

public class ShellUpdater extends Shell {
    
    public static void showFormats(){
        // the final item in the inherited formats table is an unprintable null
        int formatCount = formats.length-1;
        for(int i=0;i<formatCount;i++){
            Object[] theFormat = formats[i];
            int objCount = theFormat.length;
            System.out.println("");
            for (int j=0;j<objCount;j++){
                if (j>0){System.out.print(", ");}
                Object obj = theFormat[j];
                System.out.print(obj);
            }
        }
        System.out.println("");
    }
    
    public static void addFormat(String formatName, String className, String methodName, Class[] paramTypes){
        Object[][] oldFormats = formats;
        int oldFormatCount = oldFormats.length;
        int newFormatCount = oldFormatCount+1;
        Object[][] newFormats = new Object[newFormatCount][];
        Object[] theNewFormat = {formatName, className, methodName, paramTypes};
        newFormats[0]=theNewFormat;
        for (int i = 1; i<oldFormatCount; i++) {
            newFormats[i]=oldFormats[i];
        }
        formats = newFormats;
    }
}
