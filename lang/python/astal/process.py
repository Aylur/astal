import sys

from typing import List, Callable

from gi.repository import AstalIO

def subprocess(command: str | List[str], output=None, error=None) -> AstalIO.Process | None:
    if not output:
        output = lambda proc, x: sys.stdout.write(x + '\n')

    if not error:
        error = lambda proc, x: sys.stderr.write(x + '\n')

    if isinstance(command, list):
        proc = AstalIO.Process.subprocessv(command)

    else:
        proc = AstalIO.Process.subprocess(command)

    proc.connect('stdout', output)
    proc.connect('stderr', error)

    return proc

def exec(command: str | List[str]) -> str:
    if isinstance(command, list):
        return AstalIO.Process.execv(command)

    else:
        return AstalIO.Process.exec(command)

def exec_async(command: str | List[str], callback: Callable[[str, str], None] | None = None) -> None:
    def default_callback(output, error=None):
        if error:
            sys.stderr.write(error + '\n')

        else: sys.stdout.write(output + '\n')

    if not callback:
        callback = default_callback

    if isinstance(command, list):
        AstalIO.Process.exec_asyncv(command, lambda _, res: callback(AstalIO.Process.exec_asyncv_finish(res)))

    else:
        AstalIO.Process.exec_async(command, lambda _, res: callback(AstalIO.Process.exec_finish(res)))
