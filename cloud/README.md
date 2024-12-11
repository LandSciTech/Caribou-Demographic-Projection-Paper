# Running analysis in the cloud

We had trouble with running in parallel on one VM seemingly related to uncaught errors or failures. So we are setting up a pool with many small VMs. Using DS2_v3 because had memory issues with small ones. 
There is also a limit of 27 nodes running at once so to run more than 27 different tasks in parallel will need to run with more taskSlotsPerNode and a larger vmSize. 

Steps:

1) Make sure the Caribou-Demographic-Projection-Paper repo is correct and has been 
   pushed to GitHub. 
2) Make sure gh::gh_token() returns your GitHub PAT because it is used in the script
3) In setup_azure.sh change setName. 

4) Run a test: Do this when ever significant changes have been made to ensure it works as expected
4.1) Change the name of the script called in run_caribou.sh to sensitivityMinimalTest.R
4.2) Follow all the steps below and ensure that it works and files are correctly copied to Azure
4.3) Change the name of the script called in run_caribou.sh back to sensitivityMinimal.R

4) Step through the setup_azure.sh script interactively until you get to Monitor Tasks section. 
5) Once all tasks are running enable autoscaling. If you don't turn on autoscaling we 
   will continue paying until the pool is deleted.
6) Tasks will run and when complete the results should be copied to the Azure 
   storage container. Autoscaling should should remove nodes once they are not in use.
7) When complete use the script to download results from Azure and then delete
   the data from Azure and delete the pool.  




