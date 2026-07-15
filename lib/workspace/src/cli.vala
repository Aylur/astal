using Quarrel;

abstract class WorkspaceCommand : Command {
    static SpecialFlag help;

    public abstract int execute();

    protected static int err(string msg) {
        printerr(@"\x1b[1;31merror:\x1b[0m $msg\n");
        return 1;
    }

    class Dump : WorkspaceCommand {
        Flag watch;

        public Dump() {
            name = "dump";
            about("Dump all data (for testing)");
            opt(watch = new Flag("watch", 'w', "Watch for changes"));
        }

        private void dump_state(AstalWorkspace.WorkspaceManager manager) {
            print("Workspaces:\n");
            foreach (var workspace in manager.workspaces) {
                print("{\n");
                print("  id: %s\n", workspace.id);
                print("  name: %s\n", workspace.name);
                print("  state: %d\n", workspace.state);
                print("  capabilities: %d\n", workspace.capabilities);
                if (workspace.coordinates != null) {
                    print("  coordinates: [");
                    for (var i = 0; i < workspace.coordinates.length; i++) {
                        if (i > 0) {
                            print(", ");
                        }
                        print("%u", workspace.coordinates[i]);
                    }
                    print("]\n");
                }
                print("}\n");
            }

            print("\nGroups:\n");
            foreach (var group in manager.groups) {
                print("{\n");
                print("  outputs: [");
                for (var i = 0; i < group.outputs.length; i++) {
                    if (i > 0) {
                        print(", ");
                    }
                    print("%s", group.outputs[i].name);
                }
                print("]\n");
                print("  workspaces: [");
                for (var i = 0; i < group.workspaces.length; i++) {
                    if (i > 0) {
                        print(", ");
                    }
                    var ws = group.workspaces[i];
                    print("(id=%s, name=%s)", ws.id, ws.name);
                }
                print("]\n");
                print("}\n");
            }
        }

        public override int execute() {
            var manager = AstalWorkspace.get_default();
            AstalWl.get_default().get_display().roundtrip();
            dump_state(manager);

            if (watch.enabled) {
                var loop = new MainLoop();
                manager.updated.connect(() => dump_state(manager));
                loop.run();
            }
            return 0;
        }
    }

    class CLI : WorkspaceCommand {
        SpecialFlag version;

        public CLI() {
            name = "astal-workspace";
            about("Read and manipulate compositor workspace state");
            opt(help = new SpecialFlag("help", 'h', "Print help"));
            opt(version = new SpecialFlag("version", 'v', "Print version"));
            subcommand(new Dump().opt(help));
        }

        public override int execute() {
            if (version.enabled) {
                print("%s\n", AstalWorkspace.VERSION);
                return 0;
            }

            printerr("%s\n", Quarrel.help(this));
            return 1;
        }

        static int main(string[] argv) {
            try {
                var cmd = new CLI().parse(argv) as WorkspaceCommand;

                if (help.enabled) {
                    print("%s\n", Quarrel.help(cmd));
                    return 0;
                }

                return cmd.execute();
            } catch (ParseError parse_error) {
                return err(parse_error.message);
            }
        }
    }
}