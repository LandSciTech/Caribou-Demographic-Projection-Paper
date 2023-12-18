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
az batch job create --pool-id test_pool_json_cli --id "sendicott_job"

for i in 1 2
do
	echo "setting up task" $i
	az batch task create --json-file cloud/task_jsons/caribouDemo$i.json --job-id sendicott_job
done


