import random

malemoch = [69,68,68,68,71,69,66,68,69,70,71,67,69,71,67,70,67,69,68,68,69,72,67,68,69,69,68,70,70,73,69,71,66,69,70,69,69,69,70,71,72,70,69,69,69,67,68,73,71,68,71]
femalemoch = [68,64,65,67,67,66,66,68,67,67,65,64,67,67,66,66,65,66,66,67,67,65,64,66,66,65,67,66,62,65,64,66,73,66,66,66,67,66,63,70,65,67,65,71,65,65,67,67,69,67,65]


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
    random.shuffle(malemoch)
    random.shuffle(femalemoch)
    for j in range(51):
        x = malemoch[j] - femalemoch[j]
        allvalues.append(x)
        if x < 0:
            a += 1
        elif x == 0:
            b += 1
        elif x < 3 and x > 0:
            c += 1
        elif x > 3:
            d += 1
    a = a/52
    b = b/52
    c = c/52
    d = d/52
    a = round(a, 2)
    b = round(b, 2)
    c = round(c, 2)
    d = round(d, 2)
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
    
        