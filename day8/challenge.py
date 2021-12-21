from collections import defaultdict

digits = defaultdict(int)
input_list = []
while True:
    try:
        input_list.append(input())
    except:
        break
print(input_list)

ss = 0
for inp in input_list:
    s = inp.split(" | ")
    if len(s) == 0:
        break
    signals = s[0].split(" ")
    words = s[1].split(" ")


    
    one_segments = []
    two_segments = []
    three_segments = []
    four_segments = []
    five_segments = []
    six_segments = []
    seven_segments = []
    eight_segments = []
    nine_segments = []
    zero_segments = []

    for k in range(10):
        for w in signals:
            if len(w) == 2: #1
                one_segments = []
                for c in w:
                    one_segments.append(c)
            elif len(w) == 4: #4
                four_segments = []
                for c in w:
                    four_segments.append(c)
            elif len(w) == 3: #7
                seven_segments = []
                for c in w:
                    seven_segments.append(c)
            elif len(w) == 7: #8
                eight_segments = []
                for c in w:
                    eight_segments.append(c)

            elif len(w) == 5: #2,3,5

                found = True
                if len(one_segments) == 0:
                    found = False

                for c in one_segments:
                    if c not in w:
                        found = False

                if found:
                    three_segments = []
                    for c in w:
                        three_segments.append(c)


                elif len(six_segments) != 0:
                    missing = 0
                    for c in six_segments:
                        if c not in w:
                            missing += 1
                    
                    if missing == 2:
                        two_segments = []
                        for c in w:
                            two_segments.append(c)
                        continue
                    else:
                        five_segments = []
                        for c in w:
                            five_segments.append(c)
                        continue

            elif len(w) == 6: #0, 6, 9
                found = True #9
                if len(one_segments) == 0:
                    found = False

                for c in one_segments:
                    if c not in w:
                        found = False

                if found:
                    found = True #9
                    if len(four_segments) == 0:
                        found = False

                    for c in four_segments:
                        if c not in w:
                            found = False

                    if found:    
                        nine_segments = []
                        for c in w:
                            nine_segments.append(c)
                    else:
                        zero_segments = []
                        for c in w:
                            zero_segments.append(c)
                else:
                    six_segments = []
                    for c in w:
                        six_segments.append(c)
    print("bruh")
    print(one_segments)
    print(two_segments)
    
    full_number = 0
    count = 0
    for w in words:
        number = -1
        if len(w) == 2: #1
            number = 1
        elif len(w) == 4: #4
            number = 4
        elif len(w) == 3: #7
            number = 7
        elif len(w) == 7: #8
            number = 8
        else:
            word_set = set()
            for c in w:
                word_set.add(c)
            
            #2
            segment_set = set()
            for c in two_segments:
                segment_set.add(c)

            if word_set == segment_set:
                number = 2
            
            #3
            segment_set = set()
            for c in three_segments:
                segment_set.add(c)

            if word_set == segment_set:
                number = 3

            #6
            segment_set = set()
            for c in six_segments:
                segment_set.add(c)

            if word_set == segment_set:
                number = 6

            #5
            segment_set = set()
            for c in five_segments:
                segment_set.add(c)

            if word_set == segment_set:
                number = 5

            #9
            segment_set = set()
            for c in nine_segments:
                segment_set.add(c)

            if word_set == segment_set:
                number = 9

            #0
            segment_set = set()
            for c in zero_segments:
                segment_set.add(c)

            if word_set == segment_set:
                number = 0

        full_number = full_number*10 + number
    ss += full_number

print(ss)