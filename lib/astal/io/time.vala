public class AstalIO.Time : Object {
    public signal void now ();
    public signal void cancelled ();
    private Cancellable cancellable;
    private uint timeout_id;
    private bool fulfilled = false;

    construct {
        cancellable = new Cancellable();
        cancellable.cancelled.connect(() => {
            if (!fulfilled) {
                Source.remove(timeout_id);
                cancelled();
                dispose();
            }
        });
    }

    private void connect_closure(Closure? closure) {
        if (closure == null)
            return;

        now.connect(() => {
            Value ret = Value(Type.POINTER); // void
            closure.invoke(ref ret, {});
        });
    }

    public Time.interval_prio(uint interval, int prio = Priority.DEFAULT, Closure? fn) {
        connect_closure(fn);
        Idle.add_once(() => now());
        timeout_id = Timeout.add(interval, () => {
            now();
            return Source.CONTINUE;
        }, prio);
    }

    public Time.timeout_prio(uint timeout, int prio = Priority.DEFAULT, Closure? fn) {
        connect_closure(fn);
        timeout_id = Timeout.add(timeout, () => {
            now();
            fulfilled = true;
            return Source.REMOVE;
        }, prio);
    }

    public Time.idle_prio(int prio = Priority.DEFAULT_IDLE, Closure? fn) {
        connect_closure(fn);
        timeout_id = Idle.add(() => {
            now();
            fulfilled = true;
            return Source.REMOVE;
        }, prio);
    }

    public static Time interval(uint interval, Closure? fn) {
        return new Time.interval_prio(interval, Priority.DEFAULT, fn);
    }

    public static Time timeout(uint timeout, Closure? fn) {
        return new Time.timeout_prio(timeout, Priority.DEFAULT, fn);
    }

    public static Time idle(Closure? fn) {
        return new Time.idle_prio(Priority.DEFAULT_IDLE, fn);
    }

    public void cancel() {
        cancellable.cancel();
    }
}
