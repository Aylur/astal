from typing import Callable

from gi.repository import AstalIO

def read_file(fp: str) -> str:
    return AstalIO.read_file(fp)

def read_file_async(fp: str, callback: Callable[[str | None, Exception | None], None]) -> None:
    try:
        AstalIO.read_file_async(fp, lambda _, res: callback(AstalIO.read_file_finish(res), None))

    except Exception as e:
        callback(None, e)

def write_file(fp: str, content: str) -> None:
    AstalIO.write_file(fp, content)

def write_file_async(fp: str, content: str, callback: Callable[[Exception], None]) -> None:
    try:
        AstalIO.write_file_async(fp, content, lambda _, res: AstalIO.write_file_finish(res))

    except Exception as e:
        callback(e)

def monitor_file(fp: str, callback: Callable[[str, int], None]) -> None:
    return AstalIO.monitor_file(fp, callback)
