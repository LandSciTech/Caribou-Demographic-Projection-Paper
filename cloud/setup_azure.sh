#!/bin/bash
#############################################
#####Login etc##################

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
sastoken=`az storage container generate-sas --account-name ecdcwls --expiry $end --name sendicott --permissions racwdli -o tsv --auth-mode login --as-user`

# I actually created the SASURL manually bc it wouldn't let me do more than 7 days through CLI
sasurl=https://ecdcwls.blob.core.windows.net/sendicott/?$sastoken

setName="s10"

nBatches=10

jobName="sendicott_job_"$setName
poolName="sendicott_caribouDemo_"$setName
# delete old versions of scripts
rm -r cloud/task_scripts
rm -r cloud/task_jsons
rm -r cloud/pool_json

# Updates sasurl and setName in files, might need to change pool slots and target nodes
Rscript --vanilla "cloud/make_batch_scripts.R" $setName $sasurl

az storage copy -d $sasurl -s cloud/task_scripts --recursive

#### Create pool, job, tasks ##########################
az batch pool create --json-file cloud/pool_json/caribou_add_pool1.json
az batch job create --pool-id $poolName --id $jobName

for ((i=1;i<=nBatches;i++))
do
	echo "setting up task" $i
	az batch task create --json-file cloud/task_jsons/caribouDemo$i.json --job-id $jobName
done

az batch pool list \
--query "[].{name:id, vmSize:vmSize,  curNodes: currentDedicatedNodes,
tarNodes: targetDedicatedNodes, taskSlotsPerNode:taskSlotsPerNode,
state:state, enableAutoScale:enableAutoScale,
allocState:allocationState}" \
--output table

# List number of tasks running on each node
az batch node list --pool-id $poolName --query "[].{vmSize: vmSize,
state: state, running: runningTasksCount, succeeded:totalTasksSucceeded}" \
--output table

#### Monitor tasks ############################

# details for a single task filtered by query
az batch task show --job-id $jobName \
--task-id caribou-demog_sens_batch85 \
--query "{state: state, executionInfo: executionInfo}" --output yaml

# download output file for a task
taskNum=1
az batch task file download --task-id caribou-demog_sens_batch$taskNum \
--job-id $jobName --file-path "wd/nohup_"$taskNum".out" --destination "./nohup_"$taskNum".out"

tail -n 20 "./nohup_"$taskNum".out"

rm "./nohup_"$taskNum".out"

# List of all tasks and their state
# See here for making fancy queries https://jmespath.org/tutorial.html
az batch task list --job-id $jobName --query "{tasks: [?state == 'completed'].[id, state][]}" --output json

# az batch task reactivate --task-id caribou-demog_sens_batch86 --job-id $jobName

# Summary of task counts by state
az batch job task-counts show --job-id $jobName

# added taskslotspernode
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



# Check what results have been added to the storage container
az storage blob list -c sendicott --account-name ecdcwls --sas-token $sastoken \
--query "[].{name:name}" --prefix $setName --output yaml

#### Download results and remove from storage ################################
az storage copy -s https://ecdcwls.blob.core.windows.net/sendicott/$setName/?$sastoken \
-d results --recursive

# NOTE removes ***everything*** from the storage container
az storage remove -c sendicott --account-name ecdcwls --sas-token $sastoken --recursive

#### Delete pool and job ##########################
az batch job delete --job-id $jobName
az batch pool delete --pool-id $poolName


# downloading and resizing by hand because upload failed
for ((i=1;i<=30;i++))
do
	echo "getting file for task" $i
	az batch task file download --task-id caribou-demog_sens_batch$i \
	--job-id $jobName2 --file-path "wd/s8/rTest"$i".Rds" --destination "./results/s8/rTest"$i".Rds"
done

# reduce pools to 1 on task completion
az batch pool resize --pool-id $poolName --target-dedicated-nodes 1 \
--node-deallocation-option "taskcompletion"

az storage copy -d https://ecdcwls.blob.core.windows.net/sendicott/s8/?$sastoken \
-s results/s8 --recursive

