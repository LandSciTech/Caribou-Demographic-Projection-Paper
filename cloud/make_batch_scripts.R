# Uses secrets stored in the local credential store with the keyring package
if(0){
  # prompts you to enter the secret
  keyring::key_set("Azure_SASURL")
  keyring::key_set("Azure_subnetId")
  # retrieves the secret
  keyring::key_get("Azure_SASURL")
}
cargs <- commandArgs(trailingOnly = TRUE)
setName = cargs[1]
allScns = read.csv(paste0("tabs/",setName,".csv"))
scn_nums <- unique(allScns$pageId)

nBatches <- length(scn_nums)
sasurl <- cargs[2]

# Choose best vmSize and number of slots per node to avoid extra vCPUs

# set of possible vmSizes
vmSizes <- c(2,4,8,16,32)

# assuming 12 is the max nodes of 27 for all LERS that we want to use at a time
nSlotsPerNodeOpts <- nBatches/1:12

wchnSlotsPerNode <- sapply(nSlotsPerNodeOpts,
                           function(x) min((vmSizes - x)[(vmSizes - x) >= 0]))|>
  which.min()

nSlotsPerNode <- ceiling(nSlotsPerNodeOpts[wchnSlotsPerNode])

# requiring vmSize that is 2 times nBatches per node cores ensures there is enough RAM
wchVM <- vmSizes - nSlotsPerNode *2

vmSizeUse <- vmSizes[which(wchVM == min(wchVM[wchVM >= 0]))]

nNodes <- ceiling(nBatches/nSlotsPerNode)

# write a version of run_caribou.sh and task json for each batch
dir.create("cloud/task_scripts")
dir.create("cloud/task_jsons")
dir.create("cloud/pool_json")

make_files <- function(batch){
  batch <- as.character(batch)
  # need this to make it have unix line endings
  outfile <-  file(paste0("cloud/task_scripts/run_caribou", batch, ".sh"), "wb")
  readLines("cloud/run_caribou.sh") |>
    stringr::str_replace_all("<batch>", batch)|>
    stringr::str_replace_all("<setName>", paste0('"', setName, '"'))|>
    stringr::str_replace_all("<PAT>", gh::gh_token()) |>
    writeLines(con = outfile)
  close(outfile)
}

purrr::walk(scn_nums, make_files)


make_task <- function(batch){
  batch <- as.character(batch)
  # need this to make it have unix line endings
  outfile <-  file(paste0("cloud/task_jsons/caribouDemo", batch, ".json"), "wb")
  readLines("cloud/caribouDemo.json") |> stringr::str_replace_all("<batch>", batch)|>
    stringr::str_replace_all("<SASURL>", sasurl)|>
    writeLines(con = outfile)
  close(outfile)
}
purrr::walk(scn_nums, make_task)

# Add secrets to pool
# need this to make it have unix line endings
outfile <-  file(paste0("cloud/pool_json/caribou_add_pool1.json"), "wb")
readLines("cloud/caribou_add_pool.json") |>
  stringr::str_replace_all("<subnetId>", keyring::key_get("Azure_subnetId")) |>
  stringr::str_replace_all("<id>", paste0("sendicott_caribouDemo_", setName)) |>
  stringr::str_replace_all("<n_nodes>", as.character(nNodes)) |>
  stringr::str_replace_all("<n_slots>", as.character(nSlotsPerNode)) |>
  stringr::str_replace_all("<vmSize>", as.character(vmSizeUse)) |>
  writeLines(con = outfile)
close(outfile)

cat(as.character(nBatches))
