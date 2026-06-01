# UM DSRI

To run this repository on UM DSRI.

Use code below to sync between local folder and DSRI storage.

```zsh
# local to remote
oc rsync --progress . "$(oc get pod --selector app=rstudio2 -o jsonpath='{.items[0].metadata.name}'):root/persistent/paper-forecast-projection/" --exclude=.git --exclude=_targets --exclude=renv --exclude=output
# remote to local
oc rsync --progress  "$(oc get pod --selector app=rstudio2 -o jsonpath='{.items[0].metadata.name}'):root/persistent/paper-forecast-projection/" . --exclude=.git --exclude=_targets --exclude=renv --exclude=.Rproj.user --exclude=log --exclude=log_metrics --exclude=main_process.txt
```
