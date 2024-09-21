class Application : Astal.Application {
    Application() {

    }

    public override void activate () {
        this.add_window(new Bar());
    }

    public static int main() {
        var app = new Application();
        return app.run(null);
    }
}
