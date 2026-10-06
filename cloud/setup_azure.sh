#!/bin/bash
#############################################

#### Parameters to change
setName="s3"

username="sendicott"

# TODO: error in caribouMetrics install spatRaster not exported by terra, only
# happens on the cloud on linux. I removed the importClass call in caribouMetrics
# NAMESPACE as a work around

#####Login etc##################
# Accept the default tenant by pressing enter when prompted
az login
az batch account login -g EcDc-WLS-rg -n ecdcwlsbatch
az batch pool list \
--query "[].{name:id, vmSize:vmSize,  curNodes: currentDedicatedNodes,
tarNodes: targetDedicatedNodes, taskSlotsPerNode:taskSlotsPerNode,
state:state, enableAutoScale:enableAutoScale,
allocState:allocationState}" \
--output table


#### Move files to container ##############
end=`date -u -d "7 days" '+%Y-%m-%dT%H:%MZ'`
sastoken=`az storage container generate-sas --account-name ecdcwls --expiry $end --name $username --permissions racwdli -o tsv --auth-mode login --as-user`

subnetid=$(az network vnet subnet list --resource-group EcDc-WLS-rg --vnet-name EcDc-WLS-vnet \
--query "[?name=='EcDc-WLS_compute-cluster-snet'].id" --output tsv)
#
sasurl=https://ecdcwls.blob.core.windows.net/$username/?$sastoken

jobName=$username"_job_"$setName
poolName=$username"_caribouDemo_"$setName
# delete old versions of scripts
rm -r cloud/task_scripts
rm -r cloud/task_jsons
rm -r cloud/pool_json

# Sets PAT, sasurl, setName, nNodes, nSlots, and vmSize in files and makes a
# separate file for each batch
nBatches=$(Rscript --vanilla "cloud/make_batch_scripts.R" $setName $sasurl $username)

sed 's,<sastoken>,'${sastoken//&/\\&}',g' cloud/template_task_monitor.json > cloud/task_to_use.json

# Check that container is empty
az storage blob list -c $username --account-name ecdcwls --sas-token $sastoken \
--query "[].{name:name}" --output yaml

# Upload scripts to use in tasks
az storage copy -d $sasurl -s cloud/task_scripts --recursive
az storage copy -d $sasurl -s cloud/monitor_cpu.sh
#### Create pool, job, tasks ##########################
az batch pool create --json-file cloud/pool_json/caribou_add_pool1.json
az batch job create --pool-id $poolName --id $jobName

# all batches
for ((i=1;i<=nBatches;i++))
do
	echo "setting up task" $i
	az batch task create --json-file cloud/task_jsons/caribouDemo$i.json --job-id $jobName
done

# And in the other slot on the same node run a task that monitors usage
az batch task create --json-file cloud/task_to_use.json --job-id $jobName

az batch pool list \
--query "[].{name:id, vmSize:vmSize,  curNodes: currentDedicatedNodes,
tarNodes: targetDedicatedNodes, taskSlotsPerNode:taskSlotsPerNode,
state:state, enableAutoScale:enableAutoScale,
allocState:allocationState}" \
--output table

# List number of tasks running on each node
az batch node list --pool-id $poolName --query "[].{vmSize: vmSize,
state: state, running: runningTasksCount, succeeded:totalTasksSucceeded, allocationTime:allocationTime}" \
--output table

#### Monitor tasks ############################

# Summary of task counts by state
az batch job task-counts show --job-id $jobName

# added taskslotspernode # doesn't seem to recognize needs more nodes when slots full
az batch pool autoscale enable --pool-id $poolName --auto-scale-evaluation-interval "PT5M"\
 --auto-scale-formula 'percentage = 70;
 span = TimeInterval_Second * 15;
 $samples = $ActiveTasks.GetSamplePercent(span);
 $tasks = $samples < percentage ? max(0,$ActiveTasks.GetSample(1)) : max( $ActiveTasks.GetSample(1), avg($ActiveTasks.GetSample(span)));
 multiplier = 1;
 $cores = $TargetDedicatedNodes*$TaskSlotsPerNode;
 $extraVMs = (($tasks - $cores) + 0) * multiplier / $TaskSlotsPerNode;
 $targetVMs = ($TargetDedicatedNodes + $extraVMs);
 $TargetDedicatedNodes = max(0, min($targetVMs, 50));
 $NodeDeallocationOption = taskcompletion;'

# If autoscale not working as desired, disable set target nodes, then re-enable
# once tasks are running
az batch pool autoscale disable --pool-id $poolName
az batch pool resize --pool-id $poolName --target-dedicated-nodes 12

# details for a single task filtered by query
az batch task show --job-id $jobName \
--task-id caribou-demog_sens_batch1 \
--query "{state: state, executionInfo: executionInfo}" --output yaml

# download output file for a task
taskNum=10

az batch task file download --task-id caribou-demog_sens_batch$taskNum \
--job-id $jobName --file-path "wd/nohup_"$taskNum".out" \
--destination "./nohup_"$taskNum".out"

tail -n 30 "./nohup_"$taskNum".out"

rm "./nohup_"$taskNum".out"

# to delete a folder on the task
# az batch task file delete --task-id caribou-demog_sens_batch$taskNum \
# --job-id $jobName --file-path "wd/s3" \
# --recursive true

# List of all tasks and their state
# See here for making fancy queries https://jmespath.org/tutorial.html
az batch task list --job-id $jobName --query "{tasks: [?state == 'completed'].[id, state][]}" --output json

# az batch task reactivate --task-id caribou-demog_sens_batch86 --job-id $jobName

#### Monitor memory ##########################

fileName=stdout.txt

rm "cloud/$fileName"
rm mem.png
rm cpu.png
az batch task file download --task-id monitor_task --job-id $jobName \
--file-path $fileName --destination "cloud/$fileName"

# print the file to console
cat "cloud/$fileName"

Rscript -e 'library(tidyverse);read.table("cloud/stdout.txt", sep = ":", col.names = c("var", "value")) %>% mutate(var = str_remove(var, "^\\\\r"), value = str_remove_all(value, "\\\\") %>% str_remove("bb|GB  bb")) %>% filter(var == "MEM") %>% ggplot(aes(1:nrow(.), as.numeric(value)))+geom_point()+labs(y = "Available memory (GB)");ggsave("mem.png")'

open mem.png

Rscript -e 'library(tidyverse);read.table("cloud/stdout.txt", sep = ":", col.names = c("var", "value")) %>% mutate(var = str_remove(var, "^\\\\r"), value = str_remove_all(value, "\\\\") %>% str_remove("bb|GB  bb")%>% str_remove("\\%")) %>% filter(var == "CPU") %>% ggplot(aes(1:nrow(.), as.numeric(value)))+geom_point()+labs(y = "% CPU Usage");ggsave("cpu.png")'

open cpu.png


#### Download results ##########################

# Check what results have been added to the storage container
az storage blob list -c $username --account-name ecdcwls --sas-token $sastoken \
--query "[].{name:name}" --prefix $setName --output yaml

#### Download results and remove from storage ################################
az storage copy -s https://ecdcwls.blob.core.windows.net/$username/$setName/?$sastoken \
-d results/snew --recursive

# NOTE removes ***everything*** from the storage container
az storage remove -c $username --account-name ecdcwls --sas-token $sastoken --recursive

#### Delete pool and job ##########################
az batch job delete --job-id $jobName --y
az batch pool delete --pool-id $poolName --y

# downloading and resizing by hand because upload failed
for ((i=1;i<=50;i++))
do
	echo "getting file for task" $i
	az batch task file download --task-id caribou-demog_sens_batch$i \
	--job-id $jobName --file-path "wd/s3/rTest"$i".Rds" --destination "./results/s3/rTest"$i".Rds"
done

# reduce pools to 1 on task completion
az batch pool resize --pool-id $poolName --target-dedicated-nodes 1 \
--node-deallocation-option "taskcompletion"

az storage copy -d https://ecdcwls.blob.core.windows.net/$username/s3/?$sastoken \
-s results/s3 --recursive

