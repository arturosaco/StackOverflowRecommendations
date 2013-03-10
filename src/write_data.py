import os, pandas as pd
from pandas import Series, DataFrame
import re
import json
import pandas as pd
import numpy as np
import sys
import time

def main():
  args = sys.argv[1:]
  if not args:
    print 'usage: python write_data.py flag dump_directory path_to_data.csv'
    print 'flag = 1 for writing the score matrix, 0 otherwise'
    sys.exit(1)
  output_path = str(args[1]) + '/'
  path_to_data.csv = str(args[2])
  flag = int(args[0])

  # ================
  # = score matrix =
  # ================
  
  data = pd.read_csv(path_to_data.csv)
  data = data.set_index(["question_id", "answer_id"])
  if(flag == 1):
    mat = data["score"].unstack()
    mat.to_csv(output_path +"score_matrix.csv", encoding = "UTF-8")

  # ===================================
  # = QustionID-AcceptedAnswerOwnerID =
  # ===================================

  data[["answer_owner", "is_accepted"]].to_csv(output_path +"QID_AcceptedAnswID.csv")

  # =================
  # = Question body =
  # =================

  data = data.reset_index(["question_id", "answer_id"])
  q_data = data[["question_id", "question_body"]].drop_duplicates()
  q_data = q_data.set_index("question_id", drop = False)
  for q_id in q_data["question_id"]:
    sub = q_data[q_data["question_id"] == q_id]
    path = output_path +"q_body/" + str(sub["question_id"][q_id]) + ".html"
    with open(path, "w") as textfile:
      textfile.write(str(sub["question_body"][q_id]))

  # ========================
  # = Accepted answer text =
  # ========================

  data_accepted = data[data["is_accepted"]][["answer_id", "answer_body"]]
  data_accepted = data_accepted.set_index("answer_id", drop = False)
  for answ_id in data_accepted["answer_id"]:
    sub = data_accepted[data_accepted["answer_id"] == answ_id]
    path = output_path +"answ_body/" + str(sub["answer_id"][answ_id]) + ".html"
    with open(path, "w") as textfile:
      textfile.write(str(sub["answer_body"][answ_id]))

if __name__ == '__main__':
  main()





