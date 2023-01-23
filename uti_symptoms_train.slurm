#!/bin/bash
#SBATCH --partition=gpu
#SBATCH --requeue
#SBATCH --job-name=uti_noise
#SBATCH --out="%x-%j.out"
#SBATCH --mem=85G
#SBATCH --gpus=1
#SBATCH --nodes=1
#SBATCH --time=00-08:00:00
#SBATCH --mail-type=ALL


# do something
echo "I'm echoing to stdout"
echo "I'm echoing to stderr" 1>&2
echo "My JobID is ${SLURM_JOBID}"
echo "I have ${SLURM_CPUS_ON_NODE} CPUs on node $(hostname -s)"

# module restore condacuda101
module load CUDA/10.2.89
source activate prodigy
cd /home/vs428/Documents/UTI_Noise

export PRODIGY_HOME="/home/vs428/Documents/UTI_Noise"

python -m prodigy train uti_symtoms_model_gpu --ner UTI_symptoms-iscoe  --label-stats --base-model en_core_web_md --verbose --training.max_steps 8000  --gpu-id 0 # --config uti_symptoms_config.cfg --gpu-id 0  

# ~/Documents/n2c2_2022/annotations/n2c2_planSubsection_train_merged_v2.jsonl
# python -m prodigy train assessment_model --ner n2c2_assessment --label-stats --base-model en_core_web_trf --gpu-id 0
