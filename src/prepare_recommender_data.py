#!/usr/bin/env python

import os.path
import sys

#===============================================================================

def parse_line(tokens):
    # Skip header
    if tokens[0] == 'question_id':
        return None, None, None
    
    return long(tokens[0]), long(tokens[1]), int(tokens[2])

#===============================================================================

def logistic(score, weight):
    return weight * score / (score + weight)

#===============================================================================

def usage():
    print >> sys.stderr, 'usage: %s <weight>' % os.path.basename(sys.argv[0])
    sys.exit(1)

#===============================================================================

def main(args):
    if len(args) != 1:
        usage()
        
    weight = float(args[0])
        
    for line in sys.stdin:
        question_id, user_id, score = parse_line(line.split(','))
        if question_id and score > 0:
            print '{},{},{}'.format(question_id, user_id, logistic(score, weight)) 

if __name__ == '__main__':
    main(sys.argv[1:])
