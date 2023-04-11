def main(args: list[str]) -> int:
    if len(args) != 3:
        print("usage: gen_test_case.py <expr> <# nested>")
    
    nested = None
    try:
        nested = int(sys.argv[2])
    except:
        print("invalid argument")
        return 1

    expr = sys.argv[1]
    thing = ""
    for _ in range(nested):
        thing = f"({expr} {thing})"
    print(thing)

    return 0

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))
