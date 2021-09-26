# Trinkets2Gold

## Developed by Dorien, Habib, Kyle

*cabal build may have taken beyond deadline to submit so some default imports may have compilation error.

## The Challenge: Concurrency
You are writing a game where users trade the trinkets they gather for gold tokens. There are hundreds of users performing this simultaneously. Write a plutus program to process as many script inputs as possible in a particular block.
* Concurrency concept
* Architecture to implement concurrency
* Write or audit implementations

## The Planning/Setup
We started by minting two types of Tokens:

1. Trinkets
2. Gold

Our goal is to preemptively make these Tokens available, with the assumption that "the game" would properly distribute the Trinkets to the players.


## Concurrency Options:
### Batching UTXOs (our choice)
![Alt text](/Batch.jpg)

The idea is to batch together all the desired Trinket-to-Gold exchanges and process them at once.

This design depends on some sort of off-chain checker to periodically run the script to smelt the Trinket into Gold.  In theory, this checker could either operate using some kind of timer (for example, once every block), based on the number of Trinket input UTXOs to the script (for example, once it has exceeded 10), or some combination of the two.

Compared to the other approach that we considered, batching reduces cost due to the lower number of UTXOs and data involved in a transaction.  The tradeoff is that we have to wait a block for the Trinkets to be sent to the script address before the swap can be finalized by the checker.

Ultimately, we decided to attempt this approach due to the limited amount of time for the challenge and because of the unknown of possible complexity for the other approach.


### Pre-Grouped/Cash Register UTXOs
![Alt text](/Parallel.PNG)

The idea is to preemptively mint all the Tokens and then to break up the Gold Tokens into separate UTXOs.

In theory, you could separate each individual Token into it's own UTXO, which could potentially allow the maximum amount of parallelism and guarantee that the exchange would occur within the timeframe of a single block.

There is also the possibility of pre-portioning the groups similarly to how a cash register might work (for example: 10 groups of 100 Gold, 10 groups of 50 Gold, 10 groups of 10 Gold, 10 groups of 1 Gold)

Possible challenges of this approach include:

1. You need to somehow increase the probability that the players are grabbing unique, unconsumed UTXOs; otherwise you will run into a lot of conflicts.
2. There are increased costs involved as you increase the number of UTXOs in a transaction (for example, if each UTXO had a single Gold and the player was requesting 500 Gold for their Trinkets)
   1. A transaction would have at least one Gold UTXO and Trinket UTXO as inputs for every exchange
   2. We were informed by a mentor that you also have to keep a minimum amount of ADA along with each Token's UTXO.
3. If taking the cash register approach, there may be a desire to optimize the exchange by finding the largest amount Gold in a single UTXO that satisfies the requested amount.


###  Improvements and To-Dos

We intended to do the following, but weren't able to due to the time constraints:

1. Define the actual swap functionality.
2. Integrate the front-end fully.
3. Develop a hybrid approach of the two methodologies discussed above.

Things that needes improvement within our project include:

1. Refining our code to be robust in the face of real-world scenarios, like there being a finite amount to mint of currencies, users erroneously sending in multiple transactions or double-spending, and assuming users would have some of each currency instead of the either-or scenario we went for.
2. Enhancing the frontend to allow for smoother operation of the game.
3. Moving away from the fixed ratio of trinkets to gold to allow for an algorithm-based fine-tuning of the ratio.
