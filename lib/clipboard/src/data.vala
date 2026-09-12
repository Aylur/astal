namespace AstalClipboard {
public class Data : Object {
    private MappedFile file;
    internal int fd;

    public bool data_ready { get; private set; default = false; }

    public string mime_type { get; construct; }
    public Bytes bytes {
        owned get {
            if (this.file == null) return new Bytes(null);
            return this.file.get_bytes();
        }
    }

    public string? get_string() {
        // TODO: check if its actually a string
        return (string)this.bytes.get_data();
    }
    //TODO: add more convience getters for some mime-types, eg images

    public async void copy_pipe_to_memfd(int pipe_read_fd, int memfd) {
        var input_stream = new UnixInputStream(pipe_read_fd, true);
        var output_stream = new UnixOutputStream(memfd, false);

        uint8[] buffer = new uint8[1024];

        size_t total_bytes = 0;
        size_t bytes_read = 0;

        try {
            while (true) {
                bytes_read = yield input_stream.read_async(buffer);

                if (bytes_read > 0) {
                    yield output_stream.write_async(buffer);
                    total_bytes += bytes_read;
                } else if (bytes_read == 0) {
                    break;
                } else {
                    critical("some error");
                    break;
                }
            }

            if (Posix.ftruncate(memfd, total_bytes) != 0) {
                critical("Failed to truncate memfd: %s", Posix.strerror(Posix.errno));
            }

            output_stream.flush();
            Posix.fsync(memfd);

            this.file = new MappedFile.from_fd(memfd, false);
            this.data_ready = true;
            this.notify_property("bytes");
        } catch (Error e) {
            critical("Data transfer failed: %s", e.message);
        }
    }

    internal Data.for_offer(string mime_type, ZwlrDataControlOfferV1 offer) {
        Object(mime_type: mime_type);
        int[] pipe_fds = new int[2];
        Posix.pipe(pipe_fds);
        offer.receive(mime_type, pipe_fds[1]);
        AstalWl.Registry.get_default().get_display().flush();
        Posix.close(pipe_fds[1]);
        this.fd = Linux.memfd_create("clipboard", Linux.MemfdFlags.CLOEXEC);

        copy_pipe_to_memfd.begin(pipe_fds[0], this.fd);
    }

    internal Data(string mime_type, int fd) {
        Object(mime_type: mime_type);
        this.fd = fd;
        this.file = new MappedFile.from_fd(fd, false);
    }

    ~Data() {
        Posix.close(this.fd);
    }
}
}
