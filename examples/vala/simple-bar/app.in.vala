class App : Astal.Application {
    public static App instance;

    public override void activate () {
        foreach (var mon in this.monitors)
            add_window(new Bar(mon));

        apply_css("@STYLE@");
    }

    public static int main() {
        App.instance = new App();
        return App.instance.run(null);
    }
}
