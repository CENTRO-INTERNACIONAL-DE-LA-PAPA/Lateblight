# Use the rocker/shiny-verse image as base
FROM rocker/shiny-verse:latest

# Set timezone
ENV TZ=America/Lima
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install system dependencies
RUN apt-get update -qq && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    gcc \
    gfortran \
    libatlas-base-dev \
    libbz2-dev \
    libcairo2 \
    libcurl4-openssl-dev \
    libicu-dev \
    liblzma-dev \
    libpango-1.0-0 \
    libpangocairo-1.0-0 \
    libpcre3-dev \
    libsodium-dev \
    libtiff5-dev \
    zlib1g-dev \
    wget \
    python3-venv && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install uv (Python packaging) and a dedicated Python for the project
ENV UV_INSTALL_DIR=/usr/local/bin
RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ARG UV_PYTHON=3.13
RUN uv python install ${UV_PYTHON} && uv venv --python ${UV_PYTHON} /opt/venv
ENV PATH="/opt/venv/bin:$PATH" \
    PORT=3838

# Install Quarto
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.5.57/quarto-1.5.57-linux-amd64.deb && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y ./quarto-1.5.57-linux-amd64.deb && \
    rm quarto-1.5.57-linux-amd64.deb

# Install additional R packages
RUN R -e "install.packages(c('remotes', 'png', 'wordcloud', 'tidytext', 'stopwords', 'tinytex'), repos = 'https://cloud.r-project.org')"

# Set working directory to /app
WORKDIR /app

# Copy dependency manifests first for better layer caching
COPY pyproject.toml uv.lock /app/

# Install Python dependencies with uv (locked, no dev extras)
RUN uv sync --frozen --no-dev --python /opt/venv/bin/python

# Ensure runtime uses the project virtual environment created by uv sync
ENV VIRTUAL_ENV=/app/.venv
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

# Copy the rest of the application code
COPY . /app

# Expose the Shiny app port
EXPOSE 3838

# Run the Shiny for Python app
CMD ["sh", "-c", "python -m shiny run --host 0.0.0.0 --port ${PORT} /app/app2.py"]
