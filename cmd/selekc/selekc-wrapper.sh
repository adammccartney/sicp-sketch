#!/usr/bin/bash

pmsg_create -cm 1 /aq
./cli.scm /aq
export KUBECONFIG=$(pmsg_receive /aq | head -1)
pmsg_unlink /aq
