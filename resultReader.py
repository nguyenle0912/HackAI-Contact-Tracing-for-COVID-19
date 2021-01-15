

f = open("result.txt", "r")
str = f.read()
f.close()
str.split(",")
f = open("resultReadable", "w")
for i in str:
    f.write(i)
f.close()