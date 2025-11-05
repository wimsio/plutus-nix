Follow the steps below to setup and run the demeter inbuilt cardano node and begin interacting with your smart contracts using the cardano cli commands:
nix develop, 
cd code, 
cd workspace, 
create a directory called db in your workspace directory, 
run the below command in your workspace directory to run the cardano node from your demeter environment: 
nohup cardano-node run \
  --config config/testnet-config.json \
  --topology config/testnet-topology.json \
  --database-path db \
  --socket-path db/node.socket \
  > node.log 2>&1 &
run the below command to monitor sync progress to know when the cardano node is fully ready for building, and submitting transactions using the cardano cli commands (sync progress must be 100%):
cardano-cli query tip --testnet-magic 1 --socket-path db/node.socket
run the cli command below to build your transaction:
cardano-cli babbage transaction build \
  --testnet-magic 1 \
  --tx-in e3ab6090236e1ef39bd85ecd41dd42abd63b11544f5e722a14e4c1183852e164#0 \
  --tx-out "addr_test1wrlvp5amfskax5wurj59gqhuajudk940976uckxpxd5zrvqpdl3zp + 20000000 lovelace" \
  --tx-out-datum-file datum.json \
  --change-address addr_test1qpjmef8lnqmjq38ckr6weaatpmxdew9v25tvmkuxunn6depa0ya7lrwn8h84yfa2dlfdv3qgppfnkz3lgulk8d9lmawqcwcv20 \
  --out-file tx.lock.raw \
  --socket-path ~/cardano-node/db/node.socket

