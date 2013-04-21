#!/usr/bin/env python

from lxml import etree
import codecs
import os.path
import pandas.io.parsers
import sys

#===============================================================================

xpath_expr = '//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]'

def text_from_html(html):
    doc = etree.HTML(html)
    items = doc.xpath(xpath_expr)
    return ' '.join(items)
  
#===============================================================================

def write_text(basename, string, dirname):
    filename = str(basename) + os.extsep + 'txt'
    path = os.path.join(dirname, filename)
    with codecs.open(path, 'w', encoding = 'utf-8') as f:
        f.write(string)

def questionText(item):
    return item['title'] + '\n' + text_from_html(item['question'])

def questionAndAnswerText(item):
    return item['title'] + '\n' + text_from_html(item['question']) + text_from_html(item['answer'])

#===============================================================================

def usage():
    print >> sys.stderr, 'usage: {0}' % os.path.basename(sys.argv[0])
    sys.exit(1)

#===============================================================================

texts_filename = 'data/texts.csv'

question_test_ids_filename = 'output/itemID_test.txt'
question_training_ids_filename = 'output/itemID_train.txt'

test_directory = 'test'
training_directory = 'train'

#===============================================================================

def main(args):
    if len(args) != 0:
        usage()
    
    if os.path.exists(test_directory):
        print >> sys.stderr, 'Error: test directory "{0}" already exists' % test_directory
        usage()
        
    if os.path.exists(training_directory):
        print >> sys.stderr, 'Error: training directory "{0}" already exists' % training_directory
        usage()
        
    # Read in the downloaded questions and answers. 
    
    print >> sys.stderr, '# Reading the texts file'

    df = pandas.io.parsers.read_csv(texts_filename, encoding = 'UTF-8')
    df = df.set_index('question_id')
    
    # Read in the question_id test and training splits.
    # They are read in as Series (`squeeze = True'). 
    
    print >> sys.stderr, '# Reading the question id files'
    
    question_test_ids = pandas.io.parsers.read_csv(question_test_ids_filename, encoding = 'UTF-8', header = None, squeeze = True)
    question_training_ids = pandas.io.parsers.read_csv(question_training_ids_filename, encoding = 'UTF-8', header = None, squeeze = True)
    
    # Create the output directories.
    
    os.mkdir(test_directory)
    os.mkdir(training_directory)
    
    # Write out the relevant texts for each question. 
    
    print >> sys.stderr, '# Writing the test texts'
    
    for question_id in question_test_ids:
        try:
            row = df.ix[question_id]
        except:
            print >> sys.stderr, 'Error: no text for test question {0}' % question_id
            continue
        write_text(question_id, questionText(row), test_directory)
    
    print >> sys.stderr, '# Writing the training texts'
        
    for _, question_id in question_training_ids.iteritems():
        try:
            row = df.ix[question_id]
        except:
            print >> sys.stderr, 'Error: no text for training question {0}' % question_id
            continue
        write_text(question_id, questionAndAnswerText(row), training_directory)
    
if __name__ == '__main__':
    main(sys.argv[1:])
