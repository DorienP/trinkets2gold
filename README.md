# Trinkets2Gold

## Developed by Dorien, Habib, Kyle

## The planning
* Creating two types of NFTs 
  * Trinket NFT
  * Gold NFT

* Designing a way to demonstrate concurrency
## Concurrency Choices:
#### Batch UTXO
![Alt text](/Batch.png)

* This design depends on some sort of off-chain checker to run the script to smelt the Trinket into Gold
* * It reduces cost with less UTXOs as well as data

#### Parallel UTXO
![Alt text](/Parallel.PNG)


