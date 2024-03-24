r = open("nestest.log", "r")
w = open("a.log", "w")

for line in r.readlines():
    newline = ""

    for i in range(len(line)):
        if line[i] == '*':
            newline += ' '
            continue
        if i < 16 or i >= 48:
            newline += line[i]
    w.write(newline)

r.close()
w.close()