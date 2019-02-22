# This template relies on the Python gRPC Docker image below
FROM continuumio/anaconda3

RUN /opt/conda/bin/conda install jupyter -y --quiet
RUN mkdir /opt/notebooks
RUN conda create --name python3
RUN /bin/bash -c "source activate python3"
WORKDIR /opt/notebooks

# COPY requirements.txt /var/app
RUN conda update -n base -c defaults conda
RUN conda install matplotlib
RUN conda install numpy
RUN conda install tensorflow 
RUN conda install pandas
RUN conda install plotly
RUN conda install keras
RUN conda install scikit-learn
RUN conda install scipy
RUN conda install statsmodels
RUN conda install -c r r-essentials 
RUN conda install r-base
RUN conda install -c anaconda fonts-anaconda

# COPY . /opt/notebooks

CMD ["/opt/conda/bin/jupyter", "notebook", "--notebook-dir=/opt/notebooks", "--ip='0.0.0.0'", "--port=8888", "--no-browser", "--allow-root"]
