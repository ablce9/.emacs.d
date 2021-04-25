#! /bin/bash

set -ex

base_min() {
    apt-get update&&apt-get -y upgrade
    apt-get install -y bash-completion \
	    ca-certificates coreutils \
	    curl dnsutils file findutils \
	    gcc git gnupg gnupg2 \
	    gnupg-agent grep gzip hostname \
	    indent iptables jq less libc6-dev \
	    make mount net-tools \
	    unzip \
	    xz-utils zip netcat-openbsd apparmor \
	    bridge-utils cgroupfs-mount libapparmor-dev \
	    build-essential rpm  \
	    linux-headers-$(uname -r) \
	    --no-install-recommends;
    apt autoremove;apt autoclean;apt clean
}

install_docker() {
    local user="$1"
    if [ -z $user ]; then
	echo "Need a username."
	exit 1
    fi
    cat <<-EOF > /etc/apt/sources.list.d/docker.list
	deb https://apt.dockerproject.org/repo debian-stretch main
	deb https://apt.dockerproject.org/repo debian-stretch testing
	deb https://apt.dockerproject.org/repo debian-stretch experimental
	EOF
    apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
    sudo groupadd docker||true
    sudo gpasswd -a "$user" docker||true
    mkdir -p /etc/bash_completion.d
    curl -sSL -o /etc/bash_completion.d/docker \
	 https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/bash/docker
    local tmp_tar=/tmp/docker.tgz
    local binary_uri="https://download.docker.com/linux/static/edge/x86_64"
    local docker_version
    docker_version=$(curl -sSL "https://api.github.com/repos/docker/docker-ce/releases/latest" |jq --raw-output .tag_name)
    docker_version=${docker_version#v}
    #local docker_sha256
    #docker_sha256=$(curl -sSL "${binary_uri}/docker-${docker_version}.tgz.sha256" | awk '{print $1}')
    (
	set -x
	curl -fSL "${binary_uri}/docker-${docker_version}.tgz" -o "${tmp_tar}"
	#echo "${docker_sha256} ${tmp_tar}" | sha256sum -c -
	tar -C /usr/local/bin --strip-components 1 -xzvf "${tmp_tar}"
	rm "${tmp_tar}"
    )
    chmod +x /usr/local/bin/docker*
    cat <<EOF > /etc/systemd/system/docker.service
[Unit]
Description=Docker Application Container Engine
Documentation=http://docs.docker.com
After=network.target docker.socket firewall.service
Requires=docker.socket

[Service]
Type=notify
ExecStart=/usr/local/bin/dockerd -D \
		  -s overlay2 \
		  -H fd://
ExecReload=/bin/kill -s HUP $MAINPID
LimitNOFILE=1048576
# Having non-zero Limit*s causes performance problems due to accounting overhead
# in the kernel. We recommend using cgroups to do container-local accounting.
LimitNPROC=infinity
LimitCORE=infinity
# Uncomment TasksMax if your systemd version supports it.
# Only systemd 226 and above support this version.
#TasksMax=infinity
TimeoutStartSec=0
# set delegate yes so that systemd does not reset the cgroups of docker containers
Delegate=yes
# kill only the docker process, not all processes in the cgroup
KillMode=process
# restart the docker process if it exits prematurely
Restart=on-failure
StartLimitBurst=3
StartLimitInterval=60s

[Install]
WantedBy=multi-user.target
EOF
cat <<EOF > /etc/systemd/system/docker.socket
[Unit]
Description=Docker Socket for the API
PartOf=docker.service

[Socket]
ListenStream=/var/run/docker.sock
SocketMode=0660
SocketUser=root
SocketGroup=docker

[Install]
WantedBy=sockets.target
EOF
    systemctl daemon-reload;systemctl enable docker.service;systemctl enable docker.socket;
    systemctl start docker.socket;systemctl start docker.service;systemctl status docker
}

base_min
install_docker tangerine
