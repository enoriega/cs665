''' Takes a ranker output file and converts it to Kaggle's submission spec '''

import sys
from collections import defaultdict

choices = {0:'A', 1:'B', 2:'C', 3:'D'}

def main(path):
    results = defaultdict(lambda: float('-inf'))
    with open(path) as f:
        for line in f:
            tokens = line.split('\t')
            qid, aid, score = int(tokens[0]), int(tokens[1]), float(tokens[2])
            if results[qid] < score:
                # print qid, aid, choices[aid]
                results[qid] = aid

    print 'id,correctAnswer'
    for k, v in results.iteritems():
        print '%i,%s' % (k,choices[v])


if __name__ == '__main__':
    main(sys.argv[1])
