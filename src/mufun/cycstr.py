
def print_rec(name, count):
    if (count > 0):
        print(name, end="")
        name = name[1:] + name[0]
        print_rec(name, count-1)
    else:
        print()
        return
