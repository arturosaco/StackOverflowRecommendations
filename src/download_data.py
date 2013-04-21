#!/usr/bin/env python

from operator import itemgetter
from pandas import DataFrame
import os.path
import pandas.io.parsers
import re
import requests
import sys
import time
import traceback

#===============================================================================

def make_request_for_page(page, from_date):
    url = 'https://api.stackexchange.com/2.1/questions'

    args = { 'page': page,
             'fromdate': from_date,
             'todate': min(from_date + 604800, 1356998400),  # min(from_date + 1 week, 1 Jan 2013)
             'pagesize': 100,
             'order': 'asc',
             'sort': 'creation',
             'site': 'stackoverflow',
             'filter': '*ZCyJkPFpZ31(QD7HScfCJAhrmwvZOTB',
             'key': '6ymywSrsoZ7LNTAT5sfUcw(('
    }

    return requests.request('get', url, params = args)

# Response fields:
#    'backoff': [seconds]
#    'error_id': [integer]
#    'error_message': [string]
#    'error_name': [string]
#    'has_more' : boolean
#    'items' : questions*
#    'quota_max' : integer
#    'quota_remaining' : integer

def backoff_from_response(response):
    backoff = response.get('backoff', 0)
    
    if backoff > 0:
        print '***** Backoff ' + str(backoff)
        return 2 * backoff + 1
    else:
        return 2  # Throttle

def error(response):
    error = False
    
    if 'error_id' in response:
        print >> sys.stderr, '***** Error id: ' + str(response['error_id'])
        error = True
    
    if 'error_message' in response:
        print >> sys.stderr, '***** Error message: ' + str(response['error_message'])
        error = True
        
    if 'error_name' in response:
        print >> sys.stderr, '***** Error name: ' + str(response['error_name'])
        error = True
        
    return error

def quota_is_exhausted(response):
    return response['quota_remaining'] <= 0

# Answer fields:
#    answer_id: integer
#    body: string => answer: string
#    creation_date: date
#    is_accepted: boolean
#    owner: { user_id: integer } => user_id: integer
#    score: integer

def filter_unknown_users(answers):
    """Filter out all answers with no associated user."""
    return [answer for answer in answers if 'owner' in answer and 'user_id' in answer['owner']] 

def reformat_answer_for_scores(answer):
    answer = answer.copy()
    del answer['body']
    answer['answer_date'] = answer.pop('creation_date')
    answer['user_id'] = answer.pop('owner')['user_id']
    answer['answer_score'] = answer.pop('score')
    return answer

def reformat_answer_for_texts(answer):
    answer = answer.copy()
    answer['answer'] = answer.pop('body')
    answer['answer_date'] = answer.pop('creation_date')
    answer['user_id'] = answer.pop('owner')['user_id']
    return answer

def get_best_answer_for_texts(answers):
    result = sorted(filter_unknown_users(answers),
                    key = itemgetter('is_accepted', 'score'),
                    reverse = True)
    if result:
        return reformat_answer_for_texts(result[0])

# Question fields:
#    answers: answer* => { answer:, is_accepted:, user_id:, score: }
#    body: string => question: string
#    creation_date: date
#    question_id: integer
#    score: integer
#    tags: string* => string (space separated)
#    title: string

def reformat_item_for_scores(item):
    item = item.copy()
    del item['body']
    item['question_date'] = item.pop('creation_date')
    item['question_score'] = item.pop('score')
    del item['tags']
    del item['title']
    return item

def reformat_item_for_texts(item):
    item = item.copy()
    item['question'] = item.pop('body')
    item['question_date'] = item.pop('creation_date')
    item['tags'] = ' '.join(item['tags'])
    return item

def scores_from_response_items(items):
    scores = []
    
    for item in items:
        item = item.copy()
        answers = None
        
        if 'answers' in item:
            answers = item.pop('answers')
            answers = filter_unknown_users(answers)
            answers = map(reformat_answer_for_scores, answers)
        
        # Skip items with no usable answer
        if not answers:
            continue
        
        item = reformat_item_for_scores(item)
        
        for answer in answers:
            answer.update(item)
            scores.append(answer)
        
    return scores

def texts_from_response_items(items):
    texts = []
    
    for item in items:
        item = item.copy()
        answer = None
        
        if 'answers' in item:
            answers =  item.pop('answers')
            answer = get_best_answer_for_texts(answers)
            
        # Skip items with no usable answer
        if not answer:
            continue
        
        item = reformat_item_for_texts(item)
        
        item.update(answer)
        texts.append(item)
        
    return texts

def get_questions(n_pages, from_date):
    scores = []
    texts = []
    
    try:
        backoff = 0
    
        for page in range(1, n_pages + 1):
            # Backoff (+ throttle).
            time.sleep(backoff)
            
            request = make_request_for_page(page, from_date)
            
            try:
                request.raise_for_status()
            except Exception as e:
                print >> sys.stderr, '***** HTTP request failed [' + str(e) + ']'
                break
            
            response = request.json()
            
            if error(response):
                break
            
            print >> sys.stderr, '# Retrieved page ' + str(page) + ' [Remaining quota = ' + str(response['quota_remaining']) + ']'
            
            backoff = backoff_from_response(response)
            
            scores.extend(scores_from_response_items(response['items']))
            texts.extend(texts_from_response_items(response['items']))
            
            if quota_is_exhausted(response):
                print >> sys.stderr, '***** Quota exhausted'
                break
            
            if not response.get('has_more', False):
                print >> sys.stderr, '***** Last page'
                break
    except:
        traceback.print_exc(file = sys.stdout)
        
    return scores, texts

# Find a date to start the download.
# Unless both previous files exist, start from 1 Oct 2012.

def find_from_date(in_scores_filename, in_texts_filename):
    try:
        scores_from_date = pandas.io.parsers.read_csv(in_scores_filename, encoding = 'UTF-8')['question_date'].max()
    except:
        print >> sys.stderr, 'Failed to read scores input file "%s"\nAssuming a default start date for scores' % in_scores_filename
        scores_from_date = 1349049600
    
    try:
        texts_from_date = pandas.io.parsers.read_csv(in_texts_filename, encoding = 'UTF-8')['question_date'].max()
    except:
        print >> sys.stderr, 'Failed to read texts input file "%s"\nAssuming a default start date for texts' % in_texts_filename
        texts_from_date = 1349049600
        
    return min(scores_from_date, texts_from_date)

# Accepted input filenames and their corresponding output filenames:
#    <name>-<version>.<ext> => <name>-<version+1>.<ext>
#    <name>-.<ext> => <name>-1.<ext>
#    <name>.<ext> => <name>-1.<ext>
#    <name>-<version> => <name>-<version+1>.csv
#    <name>- => <name>-1.csv
#    <name> => <name>-1.csv

def get_out_filename(filename):
    match = re.match('(?P<prefix>.*)\.(?P<ext>[^.]+)$', filename)
    
    if match:
        prefix = match.group('prefix')
        ext = match.group('ext')
    else:
        prefix = filename
        ext = 'csv'

    match = re.match('(?P<name>.*)-(?P<version>[0-9]*)$', prefix)
    
    if match:
        name = match.group('name')
        version = match.group('version')
        if version == '':
            version = 0
        else:
            version = int(version)
    else:
        name = prefix
        version = 0
    
    return '%s-%s.%s' % (name, version + 1, ext)

#===============================================================================

def usage():
    
    print >> sys.stderr, 'usage: %s <scores-file> <texts-file> <num-pages>' % os.path.basename(sys.argv[0])
    #print 'usage: <scores-file> <texts-file> <num-pages>' 
    sys.exit(1)

#===============================================================================

def main(args):
    if len(args) != 3:
        usage()
    
    in_scores_filename = args[0]
    out_scores_filename = get_out_filename(in_scores_filename)
    
    if os.path.exists(out_scores_filename):
        print >> sys.stderr, 'Error: output file "{0}" already exists' % out_scores_filename
        usage()
    
    in_texts_filename = args[1]
    out_texts_filename = get_out_filename(in_texts_filename)
    
    if os.path.exists(out_texts_filename):
        print >> sys.stderr, 'Error: output file "{0}" already exists' % out_texts_filename
        usage()
    
    n_pages = int(args[2])
    
    if n_pages > 1000:
        print 'Note: num. pages capped at 1000'
        n_pages = 1000
    
    from_date = find_from_date(in_scores_filename, in_texts_filename)
    
    scores, texts = get_questions(n_pages, from_date)
    
    scores_df = DataFrame.from_records(scores)
    scores_df = scores_df.set_index('question_id')
    scores_df = scores_df.to_csv(out_scores_filename, encoding = 'UTF-8')
    
    texts_df = DataFrame.from_records(texts)
    texts_df = texts_df.set_index('question_id', verify_integrity = True)
    texts_df = texts_df.to_csv(out_texts_filename, encoding = 'UTF-8')
    
if __name__ == '__main__':
    main(sys.argv[1:])
