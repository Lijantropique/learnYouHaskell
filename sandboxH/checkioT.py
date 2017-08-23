from t import TESTS
from string import ascii_letters as l

with open ("T.hs","w") as hs:
    cont = 0
    # TODO: add module header
    for item in zip(l,TESTS.keys()):
        group = TESTS[item[1]]
        hs.write("{}=[".format(item[0]))
        for idx,test in enumerate(group):
            hs.write("(\"{}\",".format(test["input"]))
            hs.write("'{}'), ".format(test["answer"]))
        hs.write("]\n")
        cont+=1
        if cont==2: break
    # todo = put all keys in list
