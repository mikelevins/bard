package net.bardcode;

public class Box {
    Object value;

    public Box(){
        value = null;
    }

    public Box(Object val){
        value = val;
    }

    public Object getValue(){ return value; }
    public void setValue(Object val){ value=val; }
}
