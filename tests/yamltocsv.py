#!/opt/local/bin/python

import sys
import yaml


def main():

    yaml_file = sys.argv[1]

    stream = open(yaml_file,"r")
    data =  yaml.load(stream)

    for question in data['questions']:
        if question['type'] == "MC":
            print_mc(question)


def print_mc(question):
    print question['type'] + "," + question['question'],
    sys.stdout.softspace=False
    pos = 0
    for choice in question['choices']:
        pos += 1
        print "," + choice,
        sys.stdout.softspace=False;
        if question['correct_answer'] == pos:
            print ",Correct",
        else:
            print ",Incorrect",
        sys.stdout.softspace=False;
    print "\n",




if __name__ == "__main__":
    main()


