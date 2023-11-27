import random


maleMOCH2021=[69,69,69,70,71,72,70,69,69,69,67,68,73,71,68,71]
maleMOCH2022=[68,69,69,68,70,70,73,69,71,66,69,70]
maleMOCH2023=[69,68,68,68,71,69,66,68,69,70,71,67,69,71,67,70,67,69,68,68,69,72,67]
femaleMOCH2021=[66,67,66,63,70,65,67,65,71,65,65,67,67,69,67,65]
femaleMOCH2022=[66,66,65,67,66,62,65,64,66,73,66,66]
femaleMOCH2023=[68,64,65,67,67,66,66,68,67,67,65,64,67,67,66,66,65,66,66,67,67,65,64]

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
    random.shuffle(maleMOCH2021)
    random.shuffle(femaleMOCH2021)
    random.shuffle(maleMOCH2022)
    random.shuffle(femaleMOCH2022)
    random.shuffle(maleMOCH2023)
    random.shuffle(femaleMOCH2023)
    for j in range(16):
        x = maleMOCH2021[j] - femaleMOCH2021[j]
        allvalues.append(x)
        if x < 0:
            a += 1
        elif x == 0:
            b += 1
        elif x < 3 and x > 0:
            c += 1
        elif x > 3:
            d += 1
    for j in range(12):
        x = maleMOCH2022[j] - femaleMOCH2022[j]
        allvalues.append(x)
        if x < 0:
            a += 1
        elif x == 0:
            b += 1
        elif x < 3 and x > 0:
            c += 1
        elif x > 3:
            d += 1
    for j in range(23):
        x = maleMOCH2023[j] - femaleMOCH2023[j]
        allvalues.append(x)
        if x < 0:
            a += 1
        elif x == 0:
            b += 1
        elif x < 3 and x > 0:
            c += 1
        elif x > 3:
            d += 1
    a = a/51
    b = b/51
    c = c/51
    d = d/51
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
    
        