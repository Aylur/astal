using Quarrel;

abstract class WorkspaceCommand : Command {
    static SpecialFlag help;

    public abstract int execute();

    protected static int err(string msg) {
        printerr(@"\x1b[1;31merror:\x1b[0m $msg\n");
        return 1;
    }

    private List<AstalWl.Output> get_workspace_monitors(AstalWorkspace.WorkspaceManager manager, AstalWorkspace.Workspace workspace) {
        var result = new List<AstalWl.Output>();
        foreach (var group in manager.groups) {
            if (group.workspaces.find(workspace)) {
                foreach (var output in group.outputs) {
                    if (result.find(output) == null) {
                        result.append(output);
                    }
                }
            }
        }
        return result;
    }

    private Json.Node workspace_to_json(AstalWorkspace.Workspace workspace, List<AstalWl.Output>? monitors = null) {
        var builder = new Json.Builder()
            .begin_object()
            .set_member_name("id").add_string_value(workspace.id)
            .set_member_name("name").add_string_value(workspace.name)
            .set_member_name("active").add_boolean_value((workspace.state & AstalWorkspace.WorkspaceState.ACTIVE) != 0)
            .set_member_name("urgent").add_boolean_value((workspace.state & AstalWorkspace.WorkspaceState.URGENT) != 0)
            .set_member_name("hidden").add_boolean_value((workspace.state & AstalWorkspace.WorkspaceState.HIDDEN) != 0)
            .set_member_name("capabilities")
            .begin_object()
            .set_member_name("activate").add_boolean_value((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.ACTIVATE) != 0)
            .set_member_name("deactivate").add_boolean_value((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.DEACTIVATE) != 0)
            .set_member_name("remove").add_boolean_value((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.REMOVE) != 0)
            .set_member_name("assign").add_boolean_value((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.ASSIGN) != 0)
            .end_object()
            .set_member_name("coordinates");
        if (workspace.coordinates == null) {
            builder.add_null_value();
        } else {
            builder.begin_array();
            foreach (unowned var coord in workspace.coordinates) {
                builder.add_int_value(coord);
            }
            builder.end_array();
        }
        if (monitors != null) {
            builder.set_member_name("monitors");
            builder.begin_array();
            foreach (var output in monitors) {
                builder.add_string_value(output.name);
            }
            builder.end_array();
        }

        return builder.end_object().get_root();
    }

    private Json.Node group_to_json(AstalWorkspace.WorkspaceGroup group) {
        var builder = new Json.Builder()
            .begin_object()
            .set_member_name("capabilities")
            .begin_object()
            .set_member_name("create_workspace").add_boolean_value((group.capabilities & AstalWorkspace.GroupCapabilities.CREATE_WORKSPACE) != 0)
            .end_object()
            .set_member_name("outputs").begin_array();
        foreach (var output in group.outputs) {
            builder.add_string_value(output.name);
        }
        builder.end_array();
        builder.set_member_name("workspaces").begin_array();
        foreach (var workspace in group.workspaces) {
            builder.add_value(workspace_to_json(workspace));
        }
        builder.end_array();

        return builder.end_object().get_root();
    }

    class ListWorkspaces : WorkspaceCommand {
        Flag watch;
        Flag pretty;
        StringOpt monitor;

        public ListWorkspaces() {
            name = "list";
            about("List workspaces");
            opt(watch = new Flag("watch", 'w', "Watch for changes"));
            opt(pretty = new Flag("pretty", 'p', "Pretty print JSON"));
            opt(monitor = new StringOpt("monitor", 'm', "Filter for workspaces on a specific monitor"));
        }

        private void list_workspaces(AstalWorkspace.WorkspaceManager manager) {
            var builder = new Json.Builder()
                .begin_array();
            foreach (var workspace in manager.workspaces) {
                var monitors = get_workspace_monitors(manager, workspace);
                if (monitor.value != null) {
                    foreach (var output in monitors) {
                        if (output.name == monitor.value) {
                            builder.add_value(workspace_to_json(workspace, monitors));
                            break;
                        }
                    }
                } else {
                    builder.add_value(workspace_to_json(workspace, monitors));
                }
            }
            print("%s\n", Json.to_string(builder.end_array().get_root(), pretty.enabled));
        }

        public override int execute() {
            var manager = AstalWorkspace.get_default();
            AstalWl.get_default().get_display().roundtrip();
            list_workspaces(manager);

            if (watch.enabled) {
                var loop = new MainLoop();
                manager.updated.connect(() => list_workspaces(manager));
                loop.run();
            }
            return 0;
        }
    }

    class ListGroups : WorkspaceCommand {
        Flag watch;
        Flag pretty;

        public ListGroups() {
            name = "groups";
            about("List groups (and their workspaces)");
            opt(watch = new Flag("watch", 'w', "Watch for changes"));
            opt(pretty = new Flag("pretty", 'p', "Pretty print JSON"));
        }

        private void list_groups(AstalWorkspace.WorkspaceManager manager) {
            var builder = new Json.Builder()
                .begin_array();
            foreach (var group in manager.groups) {
                builder.add_value(group_to_json(group));
            }
            print("%s\n", Json.to_string(builder.end_array().get_root(), pretty.enabled));
        }

        public override int execute() {
            var manager = AstalWorkspace.get_default();
            AstalWl.get_default().get_display().roundtrip();
            list_groups(manager);

            if (watch.enabled) {
                var loop = new MainLoop();
                manager.updated.connect(() => list_groups(manager));
                loop.run();
            }
            return 0;
        }
    }

    abstract class WorkspaceAction : WorkspaceCommand {
        protected StringOpt w_id;
        protected StringOpt w_name;

        protected WorkspaceAction() {
            opt(w_id = new StringOpt("id", 'i', "Workspace ID"));
            opt(w_name = new StringOpt("name", 'n', "Workspace name"));
        }
    }

    class ActivateWorkspace : WorkspaceAction {
        public ActivateWorkspace() {
            name = "activate";
            about("Activate a workspace");
        }

        public override int execute() {
            if ((w_id.value == null) && (w_name.value == null)) {
                return err("at least one of ID or name must be specified");
            }

            var manager = AstalWorkspace.get_default();
            unowned var display = AstalWl.get_default().get_display();
            display.roundtrip();
            bool found_ineligible = false;
            foreach (var workspace in manager.workspaces) {
                if ((w_id.value != null) && (workspace.id != w_id.value)) {
                    continue;
                }
                if ((w_name.value != null) && (workspace.name != w_name.value)) {
                    continue;
                }
                if ((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.ACTIVATE) == 0) {
                    found_ineligible = true;
                    continue;
                }
                workspace.activate();
                display.flush();
                return 0;
            }

            if (found_ineligible) {
                return err("target workspace can't be activated");
            } else {
                return err("couldn't find target workspace");
            }
        }
    }

    class DeactivateWorkspace : WorkspaceAction {
        public DeactivateWorkspace() {
            name = "deactivate";
            about("Deactivate a workspace");
        }

        public override int execute() {
            if ((w_id.value == null) && (w_name.value == null)) {
                return err("at least one of ID or name must be specified");
            }

            var manager = AstalWorkspace.get_default();
            unowned var display = AstalWl.get_default().get_display();
            display.roundtrip();
            bool found_ineligible = false;
            foreach (var workspace in manager.workspaces) {
                if ((w_id.value != null) && (workspace.id != w_id.value)) {
                    continue;
                }
                if ((w_name.value != null) && (workspace.name != w_name.value)) {
                    continue;
                }
                if ((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.DEACTIVATE) == 0) {
                    found_ineligible = true;
                    continue;
                }
                workspace.deactivate();
                display.flush();
                return 0;
            }

            if (found_ineligible) {
                return err("target workspace can't be deactivated");
            } else {
                return err("couldn't find target workspace");
            }
        }
    }

    class RemoveWorkspace : WorkspaceAction {
        public RemoveWorkspace() {
            name = "remove";
            about("Remove a workspace");
        }

        public override int execute() {
            if ((w_id.value == null) && (w_name.value == null)) {
                return err("at least one of ID or name must be specified");
            }

            var manager = AstalWorkspace.get_default();
            unowned var display = AstalWl.get_default().get_display();
            display.roundtrip();
            bool found_ineligible = false;
            foreach (var workspace in manager.workspaces) {
                if ((w_id.value != null) && (workspace.id != w_id.value)) {
                    continue;
                }
                if ((w_name.value != null) && (workspace.name != w_name.value)) {
                    continue;
                }
                if ((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.REMOVE) == 0) {
                    found_ineligible = true;
                    continue;
                }
                workspace.remove();
                display.flush();
                return 0;
            }

            if (found_ineligible) {
                return err("target workspace can't be removed");
            } else {
                return err("couldn't find target workspace");
            }
        }
    }

    class AssignWorkspace : WorkspaceAction {
        StringOpt monitor;

        public AssignWorkspace() {
            name = "assign";
            about("Assign a workspace to a group");
            opt(monitor = new StringOpt("monitor", 'm', "Target monitor"));
        }

        public override int execute() {
            if ((w_id.value == null) && (w_name.value == null)) {
                return err("at least one of ID or name must be specified");
            }
            if (monitor.value == null) {
                return err("target monitor must be specified");
            }

            var manager = AstalWorkspace.get_default();
            unowned var display = AstalWl.get_default().get_display();
            display.roundtrip();
            AstalWorkspace.Workspace? target_workspace = null;
            bool found_ineligible = false;
            foreach (var workspace in manager.workspaces) {
                if ((w_id.value != null) && (workspace.id != w_id.value)) {
                    continue;
                }
                if ((w_name.value != null) && (workspace.name != w_name.value)) {
                    continue;
                }
                if ((workspace.capabilities & AstalWorkspace.WorkspaceCapabilities.ASSIGN) == 0) {
                    found_ineligible = true;
                    continue;
                }
                target_workspace = workspace;
                break;
            }

            if (target_workspace == null) {
                if (found_ineligible) {
                    return err("target workspace can't be assigned");
                } else {
                    return err("couldn't find target workspace");
                }
            }

            foreach (var group in manager.groups) {
                foreach (var output in group.outputs) {
                    if (output.name == monitor.value) {
                        target_workspace.assign_to_group(group);
                        display.flush();
                        return 0;
                    }
                }
            }

            return err("couldn't find target group");
        }
    }

    // This is the only group action, so a base class is unnecessary.
    class CreateWorkspace : WorkspaceCommand {
        StringOpt monitor;

        public CreateWorkspace() {
            name = "create";
            about("Create a workspace");
            required_arg("name", "New workspace name");
            opt(monitor = new StringOpt("monitor", 'm', "Target monitor"));
        }

        public override int execute() {
            if (monitor.value == null) {
                return err("target monitor must be specified");
            }

            var manager = AstalWorkspace.get_default();
            unowned var display = AstalWl.get_default().get_display();
            display.roundtrip();
            bool found_ineligible = false;
            foreach (var group in manager.groups) {
                foreach (var output in group.outputs) {
                    if (output.name == monitor.value) {
                        if ((group.capabilities & AstalWorkspace.GroupCapabilities.CREATE_WORKSPACE) == 0) {
                            found_ineligible = true;
                            break;
                        }
                        group.create_workspace(args[0]);
                        display.flush();
                        return 0;
                    }
                }
            }

            if (found_ineligible) {
                return err("target group can't have workspaces created in it");
            } else {
                return err("couldn't find target group");
            }
        }
    }

    class CLI : WorkspaceCommand {
        SpecialFlag version;

        public CLI() {
            name = "astal-workspace";
            about("Read and manipulate compositor workspace state");
            opt(help = new SpecialFlag("help", 'h', "Print help"));
            opt(version = new SpecialFlag("version", 'v', "Print version"));
            subcommand(new ListWorkspaces().opt(help));
            subcommand(new ListGroups().opt(help));
            subcommand(new ActivateWorkspace().opt(help));
            subcommand(new DeactivateWorkspace().opt(help));
            subcommand(new RemoveWorkspace().opt(help));
            subcommand(new AssignWorkspace().opt(help));
            subcommand(new CreateWorkspace().opt(help));
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
