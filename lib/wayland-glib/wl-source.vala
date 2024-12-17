namespace WlSource {
public class WlSource : Source {

    public Wl.Display display;
    public void* fd;
    public int error;

    public override bool dispatch(SourceFunc callback) {
        IOCondition revents = this.query_unix_fd(this.fd);
        if (this.error > 0 || (revents & (IOCondition.ERR | IOCondition.HUP)) != 0) {
            errno = this.error;
            if(callback != null) return callback();
            return Source.REMOVE;
        }
        if (((revents & IOCondition.IN) != 0) && this.display.dispatch() < 0) {
            if(callback != null) return callback();
            return Source.REMOVE;
        }
        return Source.CONTINUE;
    }

    public override bool check() {
        IOCondition revents = this.query_unix_fd(this.fd);
        return revents > 0;
    }

    public override bool prepare(out int timeout) {
        if(this.display.flush() < 0) 
          this.error = errno;
        timeout = -1;
        return false;
    }

    public WlSource() {
        base();
        this.display = new Wl.Display.connect(null);
        if(this.display == null) return;
        this.fd = this.add_unix_fd(this.display.get_fd(),
            IOCondition.IN | IOCondition.ERR | IOCondition.HUP);
        this.attach(null);
    }
}
}

