from pandas import Series, DataFrame
import requests, re, json, pandas as pd, numpy as np, sys, time

# =========
# = Usage =
# =========

# python download_data.py output_path.csv number_of_pages_to_retrive

# ====================================
# = Function for retrieving the info =
# ====================================

def get_answers_df(url_answers):
  a_to_q_json = requests.get(url_answers).json()
  q_ids = []
  frames = []
  q_id_text = {}  
  
  print a_to_q_json.keys()
  
  if "backoff" in a_to_q_json.keys():
    print "Backoff for" + str(a_to_q_json["backoff"]) + " seconds"
    time.sleep(a_to_q_json["backoff"])

  for q in a_to_q_json["items"]:
    if "answers" in q.keys():
      q_id_text[q["question_id"]] = q["body"]
      q_ids.append(q["question_id"])    
      a_ids = []
      frames_2 = []
      for answer in q["answers"]:
        a_ids.append(answer["answer_id"])    
        frames_2.append(DataFrame.from_dict(answer, orient = "index").transpose())
      answers_df = pd.concat(frames_2, keys = a_ids)
      frames.append(answers_df)

  q_id_text_df = DataFrame.from_dict(q_id_text, orient = "index")
  q_id_text_df.columns = ["question_body"]
  q_id_text_df.index.names = ["question_id"]
  q_id_text_df = q_id_text_df.reset_index("question_id")

  answers_out = pd.concat(frames, keys = q_ids)  
  answers_out["owner"] = answers_out["owner"].apply(lambda x : 
    x.values()[0] if(len(x) > 0) else np.nan)
  answers_out.index.names = ["question_id", "answer_id", "x"]
  answers_out = answers_out.reset_index("x")
  answers_out = answers_out.drop(["x", "answer_id"], axis = 1)
  answers_out.columns = ["answer_body", "is_accepted", "score", "answer_owner"]
  answers_out = answers_out.reset_index("question_id")
  answers_out = answers_out.reset_index("answer_id")
  answers_out = pd.merge(answers_out, q_id_text_df, on = "question_id")
  return answers_out

def main():
  args = sys.argv[1:]

  if not args:
    print 'usage: python download_data.py output_path.csv number_of_pages_to_retrive '
    sys.exit(1)
  output_path = args[0]
  no_pages = int(args[1])

  # =============
  # = Build URL =
  # =============

  base = "https://api.stackexchange.com/2.1/"
  dataset = "questions?"
  pagesize = "pagesize=100"
  page = "page=1"
  dates = "fromdate=1359676800&todate=1362009600"
  site = "site=stackoverflow"
  #filtro_answers = "filter=!OduD9Oq(n969EztJer8LsVbnB38.3hU.m.L-3u*5IlF"
  filtro_answers = "filter=!17YvvVMnKqQnZ(4tvXc6y-t7GOxx.voHqEUOcSrH7cbbn7"
  answers_list = []
  for k in range(no_pages):
    print "Retrieving page " + str(k + 1)
    url_answers = base + dataset + "&".join(["page=" + str(k + 1),
      pagesize, dates, site, filtro_answers])
    answers_list.append(get_answers_df(url_answers))

  answers = pd.concat(answers_list)
  answers = answers.set_index("question_id")

  answers.to_csv(output_path, encoding = "UTF-8")

if __name__ == '__main__':
  main()
