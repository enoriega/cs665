''' Computes precission @ 1 using the dev data split from becky '''

from __future__ import division
import pandas as pd
import sys

def main(gt_path, tested_path):
    gt = pd.read_csv(gt_path)
    tested = pd.read_csv(tested_path)

    y = (gt.correctAnswer == tested.correctAnswer).astype('int')
    print "Accuracy: %f" % (y.sum() / float(y.shape[0]))

if __name__ == '__main__':
    gt_path = sys.argv[1]
    tested_path = sys.argv[2]

    main(gt_path, tested_path)
