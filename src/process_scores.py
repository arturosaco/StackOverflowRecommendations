#!/usr/bin/env python

from pandas import Series
import os.path
import pandas.io.parsers
import random
import sys

#===============================================================================

def float_from_string(string):
    try:
        return float(string)
    except:
        print >> sys.stderr, 'Error: failed to parse "{0}" as a float' % string
        usage()

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

def usage():
    print >> sys.stderr, 'usage: {0} <aggregated-file> <texts-file> <question-test-split> <user-test-split>' % os.path.basename(sys.argv[0])
    sys.exit(1)

#===============================================================================

question_test_ids_filename = 'itemID_test.txt'
question_training_ids_filename = 'itemID_train.txt'
user_test_ids_filename = 'userID_test.txt'
user_training_ids_filename = 'userID_train.txt'

score_matrix_filename = 'matrix.csv'

test_matrix_filename = 'matrix_test.csv'
training_matrix_filename = 'matrix_train.csv'

#===============================================================================

def main(args):
    if len(args) != 4:
        usage()
    
    aggregated_filename = args[0]
    texts_filename = args[1]

    question_test_split = float_from_string(args[2])
    user_test_split = float_from_string(args[3])
    
    if not 0 < question_test_split < 1 or not 0 < user_test_split < 1:
        print >> sys.stderr, 'Error: split proportion must be between zero and one'
        usage()
        
    random.seed(42)
    
    # Read in the downloaded questions and answers. 
    
    print >> sys.stderr, '# Reading the texts file'
    texts_df = pandas.io.parsers.read_csv(texts_filename, encoding = 'UTF-8')
    
    # Read in the aggregated scores. 
    
    print >> sys.stderr, '# Reading the aggregated scores file'
    aggregated_df = pandas.io.parsers.read_csv(aggregated_filename, encoding = 'UTF-8')
    
    # Split the question and user ids into test and training sets.
    # The question ids are first restricted to those for which we have text.
    
    aggregated_df = aggregated_df[aggregated_df.question_id.isin(texts_df.question_id)];
    
    question_ids = aggregated_df.question_id
    user_ids = aggregated_df.user_id
    
    print >> sys.stderr, '# Splitting the test and training ids'
    
    question_test_ids, question_training_ids = split_ids(question_ids, question_test_split)
    user_test_ids, user_training_ids = split_ids(user_ids, user_test_split)
    
    # Write out the lists of test and training ids.
    
    print >> sys.stderr, '# Writing the test and training id files'

    write_ids(question_test_ids, question_test_ids_filename)
    write_ids(question_training_ids, question_training_ids_filename)
    write_ids(user_test_ids, user_test_ids_filename)
    write_ids(user_training_ids, user_training_ids_filename)
    
    # Split the score matrix into test and training sections.
    
    print >> sys.stderr, '# Splitting the test and training matrices'
    
    test_df = test_matrix(aggregated_df, question_test_ids, user_test_ids)
    training_df = training_matrix(aggregated_df, question_training_ids, user_training_ids)
    
    # Write out the score matrices.
    
    print >> sys.stderr, '# Writing the test and training matrices'

    write_sparse_matrix(test_df, test_matrix_filename)
    write_sparse_matrix(training_df, training_matrix_filename)
    
    print >> sys.stderr, '# Writing the score matrix'

    matrix_df = test_df.append(training_df).sort()
    write_sparse_matrix(matrix_df, score_matrix_filename)
    
if __name__ == '__main__':
    main(sys.argv[1:])
