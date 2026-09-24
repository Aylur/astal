int main(string[] argv) {
    var loop = new MainLoop();

    var clip = AstalClipboard.get_default();

    loop.run();

    return 0;
}