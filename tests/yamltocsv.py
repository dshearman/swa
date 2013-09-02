#!/opt/local/bin/python

import sys
import yaml

seperator = '\t'

def main():

    yaml_file = sys.argv[1]

    stream = open(yaml_file,"r")
    data =  yaml.load(stream)

    for question in data['questions']:
        if question['type'] == "MC":
            print_mc(question)
        if question['type'] == "TF":
            print_tf(question)
        if question['type'] == "NUM":
            print_num(question)


def print_mc(question):
    print question['type'] + seperator + question['question'],
    sys.stdout.softspace=False
    pos = 0
    for choice in question['choices']:
        pos += 1
        print seperator + choice,
        sys.stdout.softspace=False;
        if question['correct_answer'] == pos:
            print seperator + "Correct",
        else:
            print seperator + "Incorrect",
        sys.stdout.softspace=False;
    print "\n",

def print_tf(question):
    print question['type'] + seperator + question['question'] + seperator + question['answer']

def print_num(question):
    print question['type'] + seperator + question['question'] + seperator + str(question['answer']) + seperator + str(question['tolerance'])




if __name__ == "__main__":
    main()


