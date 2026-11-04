import ee
import os
from pandas import read_csv
import time

# get configs from yml file
yml = read_csv("b_pull_Landsat_SRST_poi/mid/yml.csv")
# assign proj
# LAKESR_EE_PROJ overrides ee_proj, as in run_GEE_per_pathrow.py
eeproj = os.environ.get("LAKESR_EE_PROJ") or yml["ee_proj"][0]
#initialize GEE with proj
ee.Initialize(project = eeproj)

# only track tasks created by this pipeline's run (see description in
# run_GEE_per_pathrow.py: <proj>_..._v<run_date>), so other projects' tasks
# (e.g. siteSR Pekel) don't block us
task_prefix = str(yml["proj"][0]) + "_"
task_suffix = "_v" + str(yml["run_date"][0])

def count_active_tasks():
  # ee.batch.Task.list() returns all tasks for the project; filter by description
  # and only count those that are still queued or running
  n = 0
  for task in ee.batch.Task.list():
    desc = task.config.get("description", "")
    if (desc.startswith(task_prefix) and desc.endswith(task_suffix)
        and task.state in ("READY", "RUNNING")):
      n += 1
  return n

n_active = count_active_tasks()
# loop to track if n_active is greater than zero
while (n_active > 0):
  # if it is, wait 2 minutes
  time.sleep(120)
  # and then repeat!
  n_active = count_active_tasks()

print('All tasks completed')
