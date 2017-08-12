# Ironside

Ironside is a figment of my imagination. It tries to address an itch I have had for a
long time.

I love Erlang and its runtime the BEAM. I miss a type system similar to that of SML.

So Ironside tries to provide a language that is based on the actor model with a
static type system based on type inference like SML.

The idea is that all execution will be handled by actors which has addresses that
you can send messages to.

Computation will be described by a simple functional language with semantics close to
SML, but with a curry syntax á la Haskell.

# Example program

To test my idea I have written a small program in Ironside. It is a vending machine
that can dispense chocolates or toffees.


```Ironside
data Coin = TenPence | FiftyPence

data Candy = Choc  | Toffee

data Choice = BuyCandy Candy | Abort

data Return = Message String 
            | CandyChange Candy [Coin]
            | Coins [Coin]
            
type StateData = [Coin]


actor VendingMachine () =
  Ironside.named_async_address coin;
  Ironside.named_sync_address choice;
  loop []
    
state loop coins:
  @coin ? coin ->
    loop coin::coins
  @@choice ? BuyCandy candy => from ->
     case total_paid > candy_price of
       False -> 
         from ! Message "Not enough paid!";
         loop coins
       True ->
         let
           change = calc_change candy_price total_paid
         in
           from ! CandyChange candy change;
           loop []
         end
    where
      candy_price = price candy
      total_paid = sum coins
  @@choice ? Abort => from ->
    from ! Coins coins;
    loop []
    
    
price Choc = 30
price Toffee = 50

value TenPence = 10
value FiftyPence = 50

sum coins = 
  sum' coins 0
  where
    sum' [] total = total
    sum' c:cs total = sum' cs (total + value c)
    
calc_change price total = 
  calc_change' (total - price) []
    where
      calc_change' p change | p >= 50 =
        calc_change' (p - 50) FiftyPence:change
      calc_change' p change | p > 0 = 
        calc_change' (p - 10) TenPence:change
```

First of all there is a number of type declarations that allows us to have nicely
type addresses.

Our vending machine has two addresses: `@coin` and `@@choice`. `@coin` is an
asynchronous address where the sender will not get a reply. `@@choice` is synchronous
address where there will be a reply sent back. The default behaviour of the
synchronous addresses is that the caller will block and the return value of the send
is the reply from the receiving actor. 

This also implies that synchronous channels have two types: one for the incoming
messages and one for the replies.

```Ironside
# will be created for the calling actor
named_async_address: Name -> AsyncAddress a
named_sync_address: Name -> SyncAddress a b

# un-name addresses:
async_address: () -> AsyncAddress a
sync_address: () -> SyncAdress a b
```

## Actor Interface 

The above declaration of the `VendingMachine` actor is actually a shorthand for a
full specification like this:

```Ironside
...

actor VendingMachine () :: VMInterface =
  Ironside.new_async_address coin;
  Ironside.new_sync_address choice;
  loop []
    
    
 interface VMInterface = 
   interf
     @coin : AsyncAddress Coin
     @@choice : SyncAddress Choice Return
   end
```

The `interf` will be derived just like the types of functions will.

When one is sending to a synchronous address what is really happening is this (all
syntactic sugar):

```Ironside
vm@@choice ! Choc
==
let 
  reply_addr = Ironside.new_async_address()
  reply_ref = vm@@choice ! Choc => reply_addr
in
  reply_addr ~ reply_ref ? reply -> reply
end
```

And this shows you what you have to do if you want to send a message to a synchronous
address, but you wan to receive the reply it at your leisure, i.e., you don't need
the synchronisation in this particular instance.

