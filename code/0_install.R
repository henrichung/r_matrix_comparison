#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
set -e

# Create mamba environment from file
mamba env create -f code/r_matrix_comparison.yml

# Activate environment
source activate r_matrix_comparison

# Install R packages if they are not available via conda
Rscript -e 'install.packages("GPUmatrix", repos = "https://cloud.r-project.org")'
Rscript -e 'install.packages("bench", repos = "https://cloud.r-project.org")'
Rscript -e 'install.packages("gridExtra", repos = "https://cloud.r-project.org")'


# Update system packages and install gfortran
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install gfortran
sudo apt install gcc

# Compile Fortran and C code with error handling
compile_and_check() {
    $1
    if [ $? -ne 0 ]; then
        echo "Compilation failed with command: $1"
        exit 1
    fi
}

compile_and_check "gfortran -fopenmp -shared -fPIC -o code/functions/fortran_euclidean_distance_parallel.so code/functions/fortran_euclidean_distance_parallel.f90"
compile_and_check "gcc -fopenmp -c -fPIC code/functions/Cwrapper_parallel.c -o code/functions/Cwrapper_parallel.o $(R CMD config --cppflags)"
compile_and_check "gcc -fopenmp -shared -o code/functions/Cwrapper_parallel.so code/functions/fortran_euclidean_distance_parallel.so code/functions/Cwrapper_parallel.o $(R CMD config --ldflags)"

compile_and_check "gfortran -fopenmp -shared -fPIC -o code/functions/fortran_euclidean_distance_serial.so code/functions/fortran_euclidean_distance_serial.f90"
compile_and_check "gcc -fopenmp -c -fPIC code/functions/Cwrapper_serial.c -o code/functions/Cwrapper_serial.o $(R CMD config --cppflags)"
compile_and_check "gcc -fopenmp -shared -o code/functions/Cwrapper_serial.so code/functions/fortran_euclidean_distance_serial.so code/functions/Cwrapper_serial.o $(R CMD config --ldflags)"

# Install NVIDIA drivers
sudo add-apt-repository ppa:graphics-drivers/ppa
sudo apt update
sudo apt-get install nvidia-384

# Install CUDA 11.8
wget https://developer.download.nvidia.com/compute/cuda/repos/wsl-ubuntu/x86_64/cuda-wsl-ubuntu.pin
sudo mv cuda-wsl-ubuntu.pin /etc/apt/preferences.d/cuda-repository-pin-600
wget https://developer.download.nvidia.com/compute/cuda/11.8.0/local_installers/cuda-repo-wsl-ubuntu-11-8-local_11.8.0-1_amd64.deb
sudo dpkg -i cuda-repo-wsl-ubuntu-11-8-local_11.8.0-1_amd64.deb
sudo cp /var/cuda-repo-wsl-ubuntu-11-8-local/cuda-*-keyring.gpg /usr/share/keyrings/
sudo apt-get update
sudo apt-get -y install cuda-11-8

# Install cuDNN
wget https://developer.download.nvidia.com/compute/cudnn/8.9.7.29/local_installers/cudnn-local-repo-ubuntu2204-8.9.7.29_1.0-1_amd64.deb
sudo dpkg -i cudnn-local-repo-ubuntu2204-8.9.7.29_1.0-1_amd64.deb
sudo cp /var/cudnn-local-repo-ubuntu2204-8.9.7.29/cudnn-*-keyring.gpg /usr/share/keyrings/
sudo apt-get update
sudo apt-get -y install cudnn

# Check if GPU is available in R
Sys.setenv(RETICULATE_PYTHON="~/anaconda3/envs/r_matrix_comparison/bin/python3")
library(tensorflow)
use_condaenv("r_matrix_comparison")
tf$constant("Hello, TensorFlow!")
tf$config$list_physical_devices("GPU")

# Check if GPU is available in python
python3 -c "import tensorflow as tf; print(tf.config.list_physical_devices('GPU'))"
