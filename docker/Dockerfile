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
      rsync wget nodejs php \
      fpc fp-units-fcl libjansson-dev \
 && rm -rf /var/lib/apt/lists/*

ARG GO_VERSION=1.25.4
ENV GOTOOLCHAIN=local PATH="/usr/local/go/bin:${PATH}"

RUN set -eux; \
    curl -fsSL "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz" -o /tmp/go.tgz; \
    echo "9fa5ffeda4170de60f67f3aa0f824e426421ba724c21e133c1e35d6159ca1bec  /tmp/go.tgz" | sha256sum -c -; \
    rm -rf /usr/local/go; \
    tar -C /usr/local -xzf /tmp/go.tgz; \
    rm /tmp/go.tgz

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

# Install cmake
RUN wget https://github.com/Kitware/CMake/releases/download/v4.0.4/cmake-4.0.4-linux-x86_64.sh \
    && sh cmake-4.0.4-linux-x86_64.sh --skip-license --prefix=/usr/local \
    && rm cmake-4.0.4-linux-x86_64.sh

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
    julia -e 'using Pkg; Pkg.add("JSON"); using JSON; Pkg.precompile()' \
    julia -e 'using Pkg; Pkg.add("DataStructures"); using JSON; Pkg.precompile()'

# Install .NET
RUN apt-get update && apt-get install -y wget gnupg ca-certificates \
 && apt-get update \
 # install .NET 9 runtime (STS). Use 8.0 for LTS.
 && apt-get install -y dotnet-runtime-9.0 dotnet-sdk-9.0 unzip \
 && rm -rf /var/lib/apt/lists/*

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
RUN python3 -m pip install --break-system-packages numpy networkx

# Install zig 0.15.2
ARG ZIG_VERSION=0.15.2

RUN curl -LO https://ziglang.org/download/${ZIG_VERSION}/zig-x86_64-linux-${ZIG_VERSION}.tar.xz && \
    tar -xf zig-x86_64-linux-${ZIG_VERSION}.tar.xz && \
    mv zig-x86_64-linux-${ZIG_VERSION} /opt/zig && \
    ln -s /opt/zig/zig /usr/local/bin/zig && \
    rm zig-x86_64-linux-${ZIG_VERSION}.tar.xz

USER runner

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
RUN mkdir -p /home/runner/.cargo && echo "[source.crates-io]\nreplace-with = \"vendored-sources\"\n\n[source.vendored-sources]\ndirectory = \"vendor\"" >> /home/runner/.cargo/config.toml

# Install Bun
RUN curl -fsSL https://bun.com/install | bash
ENV PATH="/home/runner/.bun/bin:$PATH"

ENV DOTNET_CLI_TELEMETRY_OPTOUT=1

ENTRYPOINT ["/usr/bin/tini","--"]
CMD ["/launch.sh"]
