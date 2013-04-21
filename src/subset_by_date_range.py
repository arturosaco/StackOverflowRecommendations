#!/usr/bin/env python

from pandas import Series
from random import random
import os.path
import pandas.io.parsers
import sys

#===============================================================================

def list_diff(a, b):
    bset = set(b)
    return [elt for elt in a if elt not in bset]

def split_ids(ids, split):
    ids = sorted(ids.unique().tolist())
    
    test_ids = sorted(random.sample(ids, int(split * len(ids))))
    training_ids = list_diff(ids, test_ids)
    
    return Series(test_ids), Series(training_ids)

#===============================================================================

# The test matrix contains all entries in the intersection of the test question
# ids and test user ids. The training matrix contains all other entries.

def test_matrix(df, question_ids, user_ids):
    return df[df.question_id.isin(set(question_ids)) & df.user_id.isin(set(user_ids))]

def training_matrix(df, question_ids, user_ids):
    return df[df.question_id.isin(set(question_ids)) | df.user_id.isin(set(user_ids))]

#===============================================================================

def write_ids(ids, filename):
    ids.to_csv(filename, encoding = 'UTF-8', header = False, index = False)

def write_sparse_matrix(df, filename):
    df.sort().to_csv(filename, encoding = 'UTF-8', index = False)

#===============================================================================

def ids_in_date_range(scores_df, from_date, to_date):
    df = scores_df[(scores_df.question_date >= from_date) & (scores_df.question_date < to_date)]
    
    return df.question_id.unique(), df.user_id.unique()

#===============================================================================

def restrict(A, B):
    return A[A.isin(set(B))]

#===============================================================================

def usage():
    print >> sys.stderr, 'usage: {0}' % os.path.basename(sys.argv[0])
    sys.exit(1)

#===============================================================================

def src(f):
    return 'output' + os.sep + f

def dst(f):
    return 'output1of3' + os.sep + f

def data(f):
    return 'data' + os.sep + f

#===============================================================================

scores_filename = 'scores.csv'
aggregated_filename = 'aggregated.csv'

#===============================================================================

question_test_ids_filename = 'itemID_test.txt'
question_training_ids_filename = 'itemID_train.txt'
user_test_ids_filename = 'userID_test.txt'
user_training_ids_filename = 'userID_train.txt'

score_matrix_filename = 'matrix.csv'

test_matrix_filename = 'matrix_test.csv'
training_matrix_filename = 'matrix_train.csv'

#===============================================================================

from_date = 1349049600  # 1 October 2012
to_date = 1351728000    # 1 November 2012

#===============================================================================

def main(args):
    if len(args) != 0:
        usage()
    
    # Read in the downloaded scores. 
    
    print >> sys.stderr, '# Reading the downloaded score file'
    score_df = pandas.io.parsers.read_csv(data(scores_filename), encoding = 'UTF-8')
    
    # Get the question and user ids in the given date range.
    
    question_ids, user_ids = ids_in_date_range(score_df, from_date, to_date)
    
    # Read in the question id and user id test and training splits.
    # They are read in as Series (`squeeze = True'). 
    
    print >> sys.stderr, '# Reading the old question id files'
    
    question_test_ids = pandas.io.parsers.read_csv(src(question_test_ids_filename), encoding = 'UTF-8', header = None, squeeze = True)
    question_training_ids = pandas.io.parsers.read_csv(src(question_training_ids_filename), encoding = 'UTF-8', header = None, squeeze = True)
    
    print >> sys.stderr, '# Reading the old user id files'
    
    user_test_ids = pandas.io.parsers.read_csv(src(user_test_ids_filename), encoding = 'UTF-8', header = None, squeeze = True)
    user_training_ids = pandas.io.parsers.read_csv(src(user_training_ids_filename), encoding = 'UTF-8', header = None, squeeze = True)
    
    # Restrict the test and training sets to the given date range.
    
    question_test_ids = restrict(question_test_ids, question_ids)
    question_training_ids = restrict(question_training_ids, question_ids)
    
    user_test_ids = restrict(user_test_ids, user_ids)
    user_training_ids = restrict(user_training_ids, user_ids)
    
    # Write out the lists of test and training ids.
    
    print >> sys.stderr, '# Writing the new question id files'

    write_ids(question_test_ids, dst(question_test_ids_filename))
    write_ids(question_training_ids, dst(question_training_ids_filename))
    
    print >> sys.stderr, '# Writing the new user id files'

    write_ids(user_test_ids, dst(user_test_ids_filename))
    write_ids(user_training_ids, dst(user_training_ids_filename))
    
    # Read in the aggregated score file. 
    
    print >> sys.stderr, '# Reading the aggregated score file'
    
    aggregated_df = pandas.io.parsers.read_csv(data(aggregated_filename), encoding = 'UTF-8')
    
    # Restrict the aggregated matrix to the given date range.
    
    aggregated_df = aggregated_df[aggregated_df.question_id.isin(question_ids)];
    aggregated_df = aggregated_df[aggregated_df.user_id.isin(user_ids)];
    
    # Split the score matrix into test and training sections.
    
    print >> sys.stderr, '# Splitting the test and training matrices'
    
    test_df = test_matrix(aggregated_df, question_test_ids, user_test_ids)
    training_df = training_matrix(aggregated_df, question_training_ids, user_training_ids)
    
    # Write out the score matrices.
    
    print >> sys.stderr, '# Writing the test and training matrices'

    write_sparse_matrix(test_df, dst(test_matrix_filename))
    write_sparse_matrix(training_df, dst(training_matrix_filename))
    
    print >> sys.stderr, '# Writing the score matrix'

    matrix_df = test_df.append(training_df).sort()
    write_sparse_matrix(matrix_df, dst(score_matrix_filename))
    
if __name__ == '__main__':
    main(sys.argv[1:])
