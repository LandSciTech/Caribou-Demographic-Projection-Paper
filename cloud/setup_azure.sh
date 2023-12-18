#!/bin/bash
#############################################
#####Login etc##################

az login
az batch account login -g EcDc-WLS-rg -n ecdcwlsbatch
az batch pool list


####Move files to container##############
end=`date -u -d "20 days" '+%Y-%m-%dT%H:%MZ'`
sastoken=`az storage container generate-sas --account-name ecdcwls --expiry $end --name sendicott --permissions racwdli -o tsv --auth-mode login --as-user`

# I actually created the SASURL manually bc it wouldn't let me do more than 7 days through CLI
sasurl=https://ecdcwls.blob.core.windows.net/sendicott/?$sastoken

nohup Rscript --vanilla "cloud/make_batch_scripts.R"

az storage copy -d $sasurl -s cloud/task_scripts --recursive

az batch pool create --json-file cloud/pool_json/caribou_add_pool1.json
az batch job create --pool-id sendicott_caribouDemo_s7 --id "sendicott_job"

for i in {1..90}
do
	echo "setting up task" $i
	az batch task create --json-file cloud/task_jsons/caribouDemo$i.json --job-id sendicott_job
done

az batch task show --job-id sendicott_job --task-id caribou-demog_sens_batch1 --query "{state: state, executionInfo: executionInfo}" --output yaml


# See here for making fancy queries https://jmespath.org/tutorial.html
az batch task list --job-id sendicott_job --query "{tasks: [].[id, state][]}" --output json


az storage blob list -c sendicott --account-name ecdcwls --sas-token $sastoken --query "[].{name:name}"


az storage copy -s https://ecdcwls.blob.core.windows.net/sendicott/s7/?$sastoken -d results --recursive


az storage remove -c sendicott --account-name ecdcwls --sas-token $sastoken --recursive

az batch job delete --job-id sendicott_job
az batch pool delete --pool-id sendicott_caribouDemo_s7
