package clj_kondo;

import clojure.lang.IFn;

public class LockFix {
    static public Object lock(final Object lockee, final IFn f) {
        synchronized (lockee) {
            return f.invoke();
        }
    }
}
