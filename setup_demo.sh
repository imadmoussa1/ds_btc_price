#!/bin/bash

GREEN=$(tput setaf 2)
NORMAL=$(tput sgr0)

pip3 install -r requirements.txt
# Airflow
# Set this to avoid the GPL version; no functionality difference either way
printf "${GREEN}Preparing environment for Airflow${NORMAL}\n"
export SLUGIFY_USES_TEXT_UNIDECODE=yes
printf "${GREEN}Initializing Airflow database${NORMAL}\n"
airflow db init
airflow db check

airflow users create --username admin --firstname admin --lastname admin --role Admin --email admin@text.com

# Adjust configuration
printf "${GREEN}Adjusting Airflow config${NORMAL}\n"
sed -i'.orig' 's/dag_dir_list_interval = 300/dag_dir_list_interval = 1/g' ~/airflow/airflow.cfg
sed -i'.orig' 's/job_heartbeat_sec = 5/job_heartbeat_sec = 1/g' ~/airflow/airflow.cfg
sed -i'.orig' 's/scheduler_heartbeat_sec = 5/scheduler_heartbeat_sec = 1/g' ~/airflow/airflow.cfg
sed -i'.orig' 's/dag_default_view = tree/dag_default_view = graph/g' ~/airflow/airflow.cfg
sed -i'.orig' 's/load_examples = True/load_examples = False/g' ~/airflow/airflow.cfg
sed -i'.orig' 's/max_threads = 2/max_threads = 1/g' ~/airflow/airflow.cfg

printf "${GREEN}Refreshing Airflow to pick up new config${NORMAL}\n"
airflow db reset --yes
airflow db init

# Copy Dags to ~/airflow/dags
rm -r ~/airflow/dags
mkdir -p ~/airflow/dags

cp pipeline_solution.py ~/airflow/dags/
cp utils_solution.py ~/airflow/dags/

# Copy data to ~/airflow/data
cp -R data ~/airflow

printf "\n${GREEN}TFX workshop installed${NORMAL}\n"
