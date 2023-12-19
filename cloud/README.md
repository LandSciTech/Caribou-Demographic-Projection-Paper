# Running analysis in the cloud

We had trouble with running in parallel on one VM seemingly related to uncaught errors or failures. So we are setting up a pool with many small VMs. Using DS2_v3 because had memory issues with small ones. 
There is also a limit of 27 nodes running at once so to run more than 27 different tasks in parallel will need to run with more taskSlotsPerNode and a larger vmSize. 

Steps:

1) Make sure the Caribou-Demographic-Projection-Paper is correct and has been pushed to GitHub. If doing a new set need to change in sensitivityMinimal and make_batch_scripts
3) In make_batch_scripts, make sure that the SASURL for the Azure storage container is up-to-date. You may need to modify the vmSize in the pool JSON template and the number of taskSlotsPerNode and the targetNumberNodes.  
4) Step through the setup_azure.sh script interactively. You will need to edit the total number of tasks and pool name as needed. 
5) Tasks will run and when complete the results should be copied to the Azure storage container. This should take ~1 day. When complete use the script to ensure that all results are successfully copied and then delete the pool. We will continue paying until the pool is deleted to try to do it ASAP after completion. 

*n = number of pageIds in set


