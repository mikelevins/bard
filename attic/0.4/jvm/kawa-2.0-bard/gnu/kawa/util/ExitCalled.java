package gnu.kawa.util;

/** Helper class for running finally-blocks when exit is invoked.
 * Usage when starting thread:
 * <pre>
 * try {
 *     ExitCalled.push();
 *     run_stuff();
 * } finally {
 *     ExitCalled.pop();
 * }
 * </pre>
 * To exit, invoking finally-handlers, do:
 * <pre>
 * ExitCalled.doExit(exitCode);
 * </pre>
 */

public class ExitCalled extends Error {

    private static ThreadLocal<Integer> isHandlingExitCalled = new ThreadLocal<Integer>();
    private static volatile ExitCalled instance;

    int exitCode;

    public static void push() {
        Integer oldCount = isHandlingExitCalled.get();
        Integer newCount = Integer.valueOf(oldCount == null ? 1
                                           : oldCount.intValue() + 1);
        isHandlingExitCalled.set(newCount);
    }

     public static int pop() {
        Integer oldCount = isHandlingExitCalled.get();
        int newCount = oldCount == null ? 0 // error
            : oldCount.intValue() - 1;
        isHandlingExitCalled.set(Integer.valueOf(newCount));
        ExitCalled ex = instance;
        if (newCount <= 0 && ex != null) {
            System.exit(ex.getExitCode());
        }
        return newCount;
    }

    public static int nesting() {
        Integer count = isHandlingExitCalled.get();
        return count == null ? 0 : count.intValue();
    }

    public int getExitCode() { return exitCode; }

    public ExitCalled(int exitCode) {
        super("(exit-called)");
        this.exitCode = exitCode;
    }

    public static void doExit(int exitCode) {
        if (nesting() > 0) {
            ExitCalled ex = new ExitCalled(exitCode);
            instance = ex;
            throw ex;
        }
        else
            System.exit(exitCode);
    }
}
