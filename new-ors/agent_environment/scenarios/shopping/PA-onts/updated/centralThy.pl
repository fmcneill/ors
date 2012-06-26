:- dynamic fact/1,class/2.

class(ourMutualFriend ,book).
class(bookShopGroup,shoppingGroup).
class(aiGroup,academicGroup).
class(shoppingAgent,agent).
class(dollars,currency).
class(pseudoVar,confirmationNumber).
class(start,sitVar).
class(start,sitVar).


fact(cost(ourMutualFriend,9)).
fact(registeredMember(shoppingAgent,aiGroup)).
fact(chooseThing(shoppingAgent,ourMutualFriend)).
fact(registeredMember(shoppingAgent,bookShopGroup)).
fact(inBasket(shoppingAgent,ourMutualFriend,47899)).
fact(money(shoppingAgent,MetaVar,100)).
