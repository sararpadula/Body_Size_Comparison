import random

malebcch = [68,66,68,67,68,62,66,65,65,65,67,67,68,68,62,63,70,69,68,67,63,67,68,69,62,68,67,65,70,70,71,65,69,71,70,70,66,71,62,68,66,65,65,68,68,74,69,65,68,67]
femalebcch = [67,66,67,62,67,64,65,66,64,63,60,62,64,64,63,70,69,64,65,71,60,63,63,63,62,62,65,67,64,66,63,62,65,66,63,63,67,66,62,63,67,61,61,63,65,66,61,65,64,66]


i = 0


allvalues = []
femaleLarger = []
sameSize = []
maleSlightlyLarger = []
maleMuchLarger = []




while i < 1000:
    a = 0
    b = 0
    c = 0
    d = 0
    random.shuffle(malebcch)
    random.shuffle(femalebcch)
    for j in range(50):
        x = malebcch[j] - femalebcch[j]
        allvalues.append(x)
        if x < 0:
            a += 1
        elif x == 0:
            b += 1
        elif x < 3 and x > 0:
            c += 1
        elif x > 3:
            d += 1
    a = a/50
    b = b/50
    c = c/50
    d = d/50
    femaleLarger.append(a)
    sameSize.append(b)
    maleSlightlyLarger.append(c)
    maleMuchLarger.append(d)
    i += 1
    
    
print("FEMALE LARGER")
print(femaleLarger)
print("SAME SIZE")
print(sameSize)
print("MALE SLIGHTLY LARGER")
print(maleSlightlyLarger)
print("MALE MUCH LARGER")
print(maleMuchLarger)
    
        