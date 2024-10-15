/**
 * `Time` provides shortcuts for GLib timeout functions.
 */
public class AstalIO.Time : Object {
    private Cancellable cancellable;
    private uint timeout_id;
    private bool fulfilled = false;

    /**
     * Emitted when the timer ticks.
     */
    public signal void now ();

    /**
     * Emitted when the timere is cancelled.
     */
    public signal void cancelled ();

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

    /**
     * Start an interval timer with a [enum@GLib.Priority].
     */
    public Time.interval_prio(uint interval, int prio = Priority.DEFAULT, Closure? fn) {
        connect_closure(fn);
        Idle.add_once(() => now());
        timeout_id = Timeout.add(interval, () => {
            now();
            return Source.CONTINUE;
        }, prio);
    }

    /**
     * Start a timeout timer with a [enum@GLib.Priority].
     */
    public Time.timeout_prio(uint timeout, int prio = Priority.DEFAULT, Closure? fn) {
        connect_closure(fn);
        timeout_id = Timeout.add(timeout, () => {
            now();
            fulfilled = true;
            return Source.REMOVE;
        }, prio);
    }

    /**
     * Start an idle timer with a [enum@GLib.Priority].
     */
    public Time.idle_prio(int prio = Priority.DEFAULT_IDLE, Closure? fn) {
        connect_closure(fn);
        timeout_id = Idle.add(() => {
            now();
            fulfilled = true;
            return Source.REMOVE;
        }, prio);
    }

    /**
     * Start an interval timer. Ticks immediately then every `interval` milliseconds.
     *
     * @param interval Tick every milliseconds.
     * @param fn Optional callback.
     */
    public static Time interval(uint interval, Closure? fn) {
        return new Time.interval_prio(interval, Priority.DEFAULT, fn);
    }

    /**
     * Start a timeout timer which ticks after `timeout` milliseconds.
     *
     * @param timeout Tick after milliseconds.
     * @param fn Optional callback.
     */
    public static Time timeout(uint timeout, Closure? fn) {
        return new Time.timeout_prio(timeout, Priority.DEFAULT, fn);
    }

    /**
     * Start a timer which will tick when there are no higher priority tasks pending.
     *
     * @param fn Optional callback.
     */
    public static Time idle(Closure? fn) {
        return new Time.idle_prio(Priority.DEFAULT_IDLE, fn);
    }

    /**
     * Cancel timer and emit [signal@AstalIO.Time::cancelled]
     */
    public void cancel() {
        cancellable.cancel();
    }
}
