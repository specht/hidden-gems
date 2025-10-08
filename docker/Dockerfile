FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 LC_ALL=C.UTF-8 TZ=Europe/Berlin \
    PYTHONDONTWRITEBYTECODE=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    BUNDLE_JOBS=4 \
    NPM_CONFIG_FUND=false \
    NPM_CONFIG_AUDIT=false

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      ca-certificates curl git tini tzdata locales \
      build-essential make pkg-config \
      python3 python3-venv python3-pip \
      ruby-full bundler \
      nodejs npm \
      default-jdk \
      lua5.4 \
      libc6 libstdc++6 libgcc-s1 zlib1g libssl3 \
      rsync wget golang nodejs php \
 && rm -rf /var/lib/apt/lists/*

# Install PowerShell
RUN apt-get update && apt-get install -y wget apt-transport-https software-properties-common gnupg \
    && wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | gpg --dearmor -o /usr/share/keyrings/microsoft.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/microsoft.gpg] \
       https://packages.microsoft.com/debian/12/prod bookworm main" > /etc/apt/sources.list.d/microsoft.list \
    && apt-get update \
    && apt-get install -y powershell \
    && rm -rf /var/lib/apt/lists/*

# Install Dart
RUN wget https://storage.googleapis.com/dart-archive/channels/stable/release/3.8.2/linux_packages/dart_3.8.2-1_amd64.deb \
    && dpkg -i dart_3.8.2-1_amd64.deb \
    && rm dart_3.8.2-1_amd64.deb

# # Install .NET SDK
# RUN apt-get update \
#     && apt-get install -y dotnet-sdk-9.0

# --- Julia (prebuilt, pinned) ---
ARG JULIA_VERSION=1.11.7
ENV JULIA_DEPOT_PATH=/usr/local/julia

RUN set -eux; \
    apt-get update && apt-get install -y --no-install-recommends ca-certificates curl xz-utils && rm -rf /var/lib/apt/lists/*; \
    curl -fsSL "https://julialang-s3.julialang.org/bin/linux/x64/1.11/julia-${JULIA_VERSION}-linux-x86_64.tar.gz" \
      -o /tmp/julia.tgz; \
    tar -xzf /tmp/julia.tgz -C /opt; \
    ln -s "/opt/julia-${JULIA_VERSION}/bin/julia" /usr/local/bin/julia; \
    rm -f /tmp/julia.tgz; \
    mkdir -p "$JULIA_DEPOT_PATH"; \
    julia -e 'using Pkg; Pkg.add("JSON"); using JSON; Pkg.precompile()'

RUN sed -i 's/# \(en_US.UTF-8 UTF-8\)/\1/' /etc/locale.gen \
 && locale-gen

COPY launch.sh /launch.sh
RUN chmod +x /launch.sh

ARG USER_ID=1000
ARG GROUP_ID=1000
RUN groupadd -g ${GROUP_ID} runner && useradd -m -u ${USER_ID} -g ${GROUP_ID} -s /bin/bash runner

WORKDIR /app
RUN chown -R runner:runner /app
RUN chown -R runner:runner /usr/local/julia
USER runner

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
RUN mkdir -p /home/runner/.cargo && echo "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> /home/runner/.cargo/config

ENTRYPOINT ["/usr/bin/tini","--"]
CMD ["/launch.sh"]
