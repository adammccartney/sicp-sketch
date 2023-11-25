select kubectl config file
==================================

Over-engineered solution to the problem of selecting from multiple KUBECONFIG
variables that are defined in an environment. For the general use case, the
built in facilities offered by [kubectl](https://kubernetes.io/docs/concepts/configuration/organize-cluster-access-kubeconfig/) are probably enough.


## Requirements

+ guile scheme (3.0)
+ Something like
  [pmsg](https://github.com/adammccartney/cscratch/tree/main/adlpi), the
  binaries must be installed and reachable on the PATH


## Usage

+ install the dependencies and make sure they are on the PATH.
+ clone this repository and run `make` to link the cli script to an executable in `$HOME/bin`
