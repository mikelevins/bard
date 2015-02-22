package kawa.lang;

/** A "syntactic closure" - a syntax datum with its compilation environment. */

public interface SyntaxForm
{
    public abstract Object getDatum();

    public abstract TemplateScope getScope();
}
