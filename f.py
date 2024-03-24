f = open("mesen.pal", "rb")
g = open("palette.txt", "w")

while (byte := f.read(1)):
    x = int.from_bytes(byte, 'little')
    y = int.from_bytes(f.read(1), 'little')
    z = int.from_bytes(f.read(1), 'little')
    g.write("(0x{:2x}, 0x{:2x}, 0x{:2x}), ".format(x, y, z))
    print("%2x" % x)

