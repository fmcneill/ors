:- dynamic fact/1,class/2.

class(plantZero,agent).
class(plantOne,agent).
class(plantTwo,agent).
class(alpha,contaminant).
class(beta,contaminant).
class(60,percentage).
class(start,sitVar).


fact(treats(plantZero,alpha,60)).
fact(used(plantOne,200)).
fact(max(plantOne,400)).
fact(at(plantTwo,alpha)).
fact(at(plantTwo,beta)).
