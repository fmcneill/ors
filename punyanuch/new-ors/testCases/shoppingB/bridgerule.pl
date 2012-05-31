bridgeRule([original,update],[repairType,[repairInfo]]).

bridgeRule([interest(bookShopGroup,shoppingAgent),interest(shoppingAgent,bookShopGroup)],[switchArgs,[interest,2,[shoppingGroup,agent],[[agent,shoppingGroup],[shoppingGroup,agent]]]]).

bridgeRule([chooseThing(shoppingAgent,ourMutualFriend),chooseThing(shoppingAgent,titchmarshMemoirs)],[domainA,[chooseThing,2,[agent,book],gardeningBook,subclass(gardeningBook,book)]]).

bridgeRule([money(shoppingAgent,dollars,100),money(shoppingAgent,100)],[propositionalA,[money,3,[agent,currency,uninstantiated],currency,2]]).
