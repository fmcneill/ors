
:- dynamic experienceDatabase/2.

experienceDatabase(index,value).
experienceDatabase([putInBasketAgent,putInBasket(shoppingAgent,bookShopGroup,ourMutualFriend)],[[[chooseItem(shoppingAgent,ourMutualFriend)],[chooseItem(shoppingAgent,ourMutualFriend),registeredMember(shoppingAgent,bookShopGroup),class(bookShopGroup,shoppingGroup),request]],[chooseItem(shoppingAgent,ourMutualFriend,123),chooseItem(shoppingAgent,ourMutualFriend),[propositionalA,[chooseItem,3]],success]]).

experienceDatabase([joinGroupAgentTwo,joinGroup(shoppingAgent,bookShopGroup)],[[[interest(bookShopGroup,shoppingAgent)],[interest(bookShopGroup,shoppingAgent),request]],[interest(shoppingAgent,bookShopGroup),interest(bookShopGroup,shoppingAgent),[switchArgs,interest],success]]).

experienceDatabase([joinGroupAgentOne,joinGroup(shoppingAgent,bookShopGroup)],[[[],[request]],[none,meta,[incorrectAgent,_10351],success]]).

experienceDatabase([putInBasketAgent,putInBasket(shoppingAgent,aiGroup,ourMutualFriend)],[[[class(aiGroup,shoppingGroup)],[class(aiGroup,shoppingGroup),request]],[group,shoppingGroup,[precondAA,class],success]]).
