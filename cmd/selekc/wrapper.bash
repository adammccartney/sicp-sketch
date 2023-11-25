
function selekc () {
    # select a kubeconfig file from available options and set the selected
    # version as the KUBECONFIG environment variable
    pmsg_create -cm 1 /aq
    selkc.cli "/aq"
    KCPATH=$(pmsg_receive /aq | head -1)
    export KUBECONFIG=${KCPATH}
    pmsg_unlink /aq
}
