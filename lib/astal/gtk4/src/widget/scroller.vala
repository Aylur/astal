
public enum Astal.ScrollBehaviour {
  ALTERNATE,
  SLIDE
}

public class Astal.Scroller : Gtk.Widget, Gtk.Orientable, Gtk.Buildable {
  private Gtk.Widget _child;
  private double position = 0;
  private int scroll_direction = -1;
  private int64 last_time = 0;
  private int64 delay = 0;

  /**
  * The scrollspeed in pixels per second.
  */
  public double speed { get; set; default = 25; }

  /**
  * The delay used when changing the scroll direction. This has only an effect when
  * [property@Astal.Scroller:behaviour] is set to alternate.
  */
  public int direction_change_delay { get; set; default = 500; }

  /**
  * The scrolling behaviour.
  */
  public Astal.ScrollBehaviour behaviour { get; set; default = Astal.ScrollBehaviour.ALTERNATE; }
  
  /**
  * The orientation of the scroller. This determines the scroll direction.
  */
  public Gtk.Orientation orientation { get; set; default = Gtk.Orientation.HORIZONTAL; }

  /**
  * Whether or not the scroller should propagate its childs size. This will only affect
  * the scroll direction, so eg when the scrollers orientation is horizontal, it will
  * always propagates the childs height.
  */
  public bool propagate_child_size { get; set; default = true; }

  public Gtk.Widget child {
    get { return this._child; }
    set { 
      if(this._child != null) this._child.unparent();
      this._child = value;
      this._child.set_parent(this);
    }
  }

  static construct {
    set_css_name("scroller");
  }

  construct {
    this.overflow = Gtk.Overflow.HIDDEN;
    this.add_tick_callback(update_position);
  }

  public void add_child(Gtk.Builder builder, Object child, string? type) {
    if(child is Gtk.Widget) this.child = child as Gtk.Widget;
  }

  protected override void measure(Gtk.Orientation orientation,
                                  int for_size,
                                  out int minimum,
                                  out int natural,
                                  out int minimum_baseline,
                                  out int natural_baseline) {
    int min = 0;
    int nat = 0;

    if((propagate_child_size || orientation != this.orientation) && this._child != null && this._child.visible)
      this._child.measure(orientation, -1, out min, out nat, null, null);

    minimum = 0;
    natural = nat;
    minimum_baseline = -1;
    natural_baseline = -1;
  }

  protected override void size_allocate(int width, int height, int baseline) {

    if(this._child == null) return;

    int child_width = 0;
    int child_height = 0;

    Gtk.Requisition child_req;
    this._child.get_preferred_size(out child_req, null);

    child_width = child_req.width;
    child_height = child_req.height;
    if (this.orientation == Gtk.Orientation.HORIZONTAL) {
      this._child.allocate_size({ (int)this.position, 0, child_width, child_height }, -1);
    } else {
      this._child.allocate_size({ 0, (int)this.position, child_width, child_height }, -1);
    }
  }

  protected override Gtk.SizeRequestMode get_request_mode() {
    return Gtk.SizeRequestMode.CONSTANT_SIZE;
  }

  private bool update_position(Gtk.Widget widget, Gdk.FrameClock clock) {

    if(this._child == null) return Source.CONTINUE;

    int64 current_time = clock.get_frame_time();

    if (this.last_time == 0) {
      this.last_time = current_time;
      return Source.CONTINUE;
    }

    int64 elapsed = current_time - this.last_time;
    this.last_time = current_time;
    double delta = this.speed * elapsed / 1000000;

    bool is_horizontal = (this.orientation == Gtk.Orientation.HORIZONTAL);
    double limit = is_horizontal ? this.get_width() : this.get_height();
    double label_size = is_horizontal ? this._child.get_width() : this._child.get_height();

    if (this.delay >= 0) {
      this.delay += elapsed / 1000;
      if (this.delay > this.direction_change_delay) {
        this.delay = -1;
      } else {
        return Source.CONTINUE;
      }
    }

    if (this.behaviour == Astal.ScrollBehaviour.ALTERNATE) {
      if (this.scroll_direction < 0) {
        if (this.position + label_size > limit) {
          this.position = double.max(limit - label_size, this.position - delta);
        } else {
          this.scroll_direction = 1;
          this.delay = 0;
        }
      } else {
        if (this.position < 0) {
          this.position = double.min(0, this.position + delta);
        } else {
          this.scroll_direction = -1;
          this.delay = 0;
        }
      }
    } else {
      if (this.position + label_size > 0) {
        this.position -= delta;
      } else {
        this.position = limit;
      }
    }

    this.queue_resize();
    return Source.CONTINUE;
  }

  ~Scroller() {
    if(this._child != null) this._child.unparent();
  }
}
