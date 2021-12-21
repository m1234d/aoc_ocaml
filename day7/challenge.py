from collections import defaultdict

crabs = input().split(",")

for i in range(len(crabs)):
    crabs[i] = int(crabs[i])

mapping = defaultdict(int)

maxh = max(crabs)

minDiff = -1
minH = -1
for i in range(maxh):
    totalDiff = 0
    for j in range(len(crabs)):
        n = abs(crabs[j] - i)
        totalDiff += n*(n+1)/2
    if totalDiff < minDiff or minDiff == -1:
        minDiff = totalDiff
        minH = i

print(minDiff, minH)