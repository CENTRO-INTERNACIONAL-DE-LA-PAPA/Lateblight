# Use the rocker/shiny-verse image as base
# Pinned: the "latest" tag moved to Ubuntu 26.04 (R 4.6.2) on 2026-10-29 and dropped
# libpcre3-dev / libatlas-base-dev, which broke the Jenkins build. 4.5.2 = Ubuntu 24.04.
FROM rocker/shiny-verse:4.5.2

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
    libopenblas-dev \
    libbz2-dev \
    libcairo2 \
    libcurl4-openssl-dev \
    libicu-dev \
    liblzma-dev \
    libpango-1.0-0 \
    libpangocairo-1.0-0 \
    libpcre2-dev \
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
