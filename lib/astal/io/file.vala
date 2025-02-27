namespace AstalIO {
/**
 * Read the contents of a file synchronously.
 */
public string read_file(string path) {
    var str = "";
    try {
        FileUtils.get_contents(path, out str, null);
    } catch (Error error) {
        critical(error.message);
    }
    return str;
}

/**
 * Read the contents of a file asynchronously.
 */
public async string read_file_async(string path) throws Error {
    uint8[] content;
    yield File.new_for_path(path).load_contents_async(null, out content, null);
    return (string)content;
}

/**
 * Write content to a file synchronously.
 */
public void write_file(string path, string content, bool follow_symlinks = true) {
    try {
        var dir = Path.get_dirname(path);
        if (!FileUtils.test(dir, FileTest.IS_DIR)) {
            File.new_for_path(dir).make_directory_with_parents(null);
        }

        if (follow_symlinks) {
            while (FileUtils.test(path, FileTest.IS_SYMLINK)) {
                path = FileUtils.read_link(path);
            }
        }

        FileUtils.set_contents(path, content);
    } catch (Error error) {
        critical(error.message);
    }
}

/**
 * Write content to a file asynchronously.
 */
public async void write_file_async(string path, string content, bool follow_symlinks = true) throws Error {
    var dir = Path.get_dirname(path);
    if (!FileUtils.test(dir, FileTest.IS_DIR)) {
        File.new_for_path(dir).make_directory_with_parents(null);
    }

    if (follow_symlinks) {
        while (FileUtils.test(path, FileTest.IS_SYMLINK)) {
            path = FileUtils.read_link(path);
        }
    }

    yield File.new_for_path(path).replace_contents_async(
        content.data,
        null,
        false,
        FileCreateFlags.REPLACE_DESTINATION,
        null,
        null);
}

/**
 * Monitor a file for changes. If the path is a directory, monitor it recursively.
 * The callback will be called passed two parameters: the path of the file
 * that changed and an [enum@Gio.FileMonitorEvent] indicating the reason.
 */
public FileMonitor? monitor_file(string path, Closure callback, bool follow_symlinks = true) {
    try {
        var file = File.new_for_path(path);
        var mon = file.monitor(FileMonitorFlags.NONE);

        // For symlinks, if we should follow them, follow each of them
        // and also monitor changes on them
        if (follow_symlinks && FileUtils.test(path, FileTest.IS_SYMLINK)) {
            var linkpath = FileUtils.read_link(path); 
            var m = monitor_file(linkpath, callback, follow_symlinks);
            mon.notify["cancelled"].connect(() => {
                m.cancel();
            });
        }

        mon.changed.connect((file, _file, event) => {
            var f = Value(Type.STRING);
            var e = Value(Type.INT);
            var ret = Value(Type.POINTER);

            f.set_string(file.get_path());
            e.set_int(event);

            callback.invoke(ref ret, { f, e });
        });

        if (FileUtils.test(path, FileTest.IS_DIR)) {
            var enumerator = file.enumerate_children("standard::*",
                FileQueryInfoFlags.NONE, null);

            var i = enumerator.next_file(null);
            while (i != null) {
                if (i.get_file_type() == FileType.DIRECTORY) {
                    var filepath = file.get_child(i.get_name()).get_path();
                    if (filepath != null) {
                        var m = monitor_file(filepath, callback, follow_symlinks);
                        mon.notify["cancelled"].connect(() => {
                            m.cancel();
                        });
                    }
                }
                i = enumerator.next_file(null);
            }
        }

        mon.ref();
        mon.notify["cancelled"].connect(() => {
            mon.unref();
        });
        return mon;
    } catch (Error error) {
        critical(error.message);
        return null;
    }
}
}
