#!/usr/bin/env python3

import os
from sh import zip, cargo, ErrorReturnCode


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print("usage: zip_proj <folder>")
        return 1

    path = argv[1]
    if not os.path.isdir(path):
        print(f"error: could not find directory '{path}'")
        return 1

    cargo("clean", f"--manifest-path={path}/Cargo.toml")
    
    if path[-1] == '/':
        path = path[:-1]

    try:
        print(zip("-r", f"{path}_ayoung.zip", path))
        return 0
    except ErrorReturnCode as e:
        print(f"encountered error while zipping: {e}")
        return 1


if __name__ == "__main__":
    import sys
    sys.exit(main(sys.argv))
