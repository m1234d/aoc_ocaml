from collections import defaultdict

fishes = input().split(",")

for i in range(len(fishes)):
    fishes[i] = int(fishes[i])

mapping = defaultdict(int)

for f in fishes:
    mapping[f] += 1

for g in range(256):
    new_mapping = defaultdict(int)
    for k,v in mapping.items():
        if k == 0:
            new_mapping[8] += v
            new_mapping[6] += v
        else:
            new_mapping[k-1] += v

    mapping = new_mapping

s = 0
print(mapping)
for k, v in mapping.items():
    s += v

print(s)
