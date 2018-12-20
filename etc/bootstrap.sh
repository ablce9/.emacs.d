#! /bin/bash
set -xe
PROJECT_ROOT=$(dirname "${BASH_SOURCE}")
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@ DO NOT ADD .bash_env @@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
DOT_FILES=(.aliases .bash_profile .bashrc .functions \
		    .gdbinit .gitconfig .git-prompt.sh .pep.cfg)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

base_min() {
    apt-get update&&apt-get -y upgrade
    apt-get install -y adduser automake \
	    bash-completion bc bzip2 \
	    ca-certificates coreutils \
	    curl dnsutils file findutils \
	    gcc git gnupg gnupg2 \
	    gnupg-agent grep gzip hostname \
	    indent iptables jq less libc6-dev \
	    libimobiledevice6 locales lsof \
	    make mount net-tools pinentry-curses \
	    rxvt-unicode-256color scdaemon \
	    silversearcher-ag ssh strace sudo \
	    tar tree tzdata usbmuxd unzip \
	    xz-utils zip netcat-openbsd apparmor \
	    bridge-utils cgroupfs-mount libapparmor-dev \
	    libltdl-dev libseccomp-dev network-manager \
	    s3cmd build-essential rpm ninja-build \
	    libncurses-dev emacs25-nox \
	    linux-headers-$(uname -r) \
	    --no-install-recommends;
    apt autoremove;apt autoclean;apt clean
}

install_golang() {
    local user="$1"
    if [ -z $user ]; then
	echo "Need a username."
	exit 1
    fi
    export GO_VERSION
    GO_VERSION=$(curl -sSL "https://golang.org/VERSION?m=text")
    export GO_SRC=/usr/local/go
    if [[ ! -z "$1" ]]; then
	GO_VERSION=$1
    fi
    if [[ -d "$GO_SRC" ]]; then
	sudo rm -rf "$GO_SRC"
	sudo rm -rf "$GOPATH"
    fi
    GO_VERSION=${GO_VERSION#go}
    (
	kernel=$(uname -s | tr '[:upper:]' '[:lower:]')
	curl -sSL "https://storage.googleapis.com/golang/go${GO_VERSION}.${kernel}-amd64.tar.gz" | sudo tar -v -C /usr/local -x # z
	sudo chown -R "${user}" /usr/local/go/pkg
	CGO_ENABLED=0 go install -a -installsuffix cgo std
    )
}

install_scripts() {
    curl -sSL https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py  > /usr/local/bin/speedtest
    chmod +x /usr/local/bin/speedtest
    curl -sSL https://raw.githubusercontent.com/jeffkaufman/icdiff/master/icdiff > /usr/local/bin/icdiff
    curl -sSL https://raw.githubusercontent.com/jeffkaufman/icdiff/master/git-icdiff > /usr/local/bin/git-icdiff
    chmod +x /usr/local/bin/icdiff
    chmod +x /usr/local/bin/git-icdiff
    curl -sSL https://raw.githubusercontent.com/tehmaze/lolcat/master/lolcat > /usr/local/bin/lolcat
    chmod +x /usr/local/bin/lolcat
    local scripts=( have light )
    for script in "${scripts[@]}"; do
	curl -sSL "https://misc.j3ss.co/binaries/$script" > "/usr/local/bin/${script}"
	chmod +x "/usr/local/bin/${script}"
    done
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
    curl -sSL https://raw.githubusercontent.com/jessfraz/dotfiles/master/etc/systemd/system/docker.service > \
	 /etc/systemd/system/docker.service
    curl -sSL https://raw.githubusercontent.com/jessfraz/dotfiles/master/etc/systemd/system/docker.socket > \
	 /etc/systemd/system/docker.socket
    systemctl daemon-reload;systemctl enable docker.service;systemctl enable docker.socket;
    systemctl start docker.socket;systemctl start docker.service;systemctl status docker
}

install_wireguard ()
{
    local user="$1"
    if [ -z $user ]; then
	echo "Need a username."
	exit 1
    fi
    (set e; test -d /usr/src/linux-headers-$(uname -r)||echo "need linux header")
    sudo apt-get install libmnl-dev libelf-dev pkg-config --no-install-recommends -y
    sudo mkdir /opt/wireguard && cd /opt/wireguard && sudo chown "$user:$user" /opt/wireguard;
    wget https://git.zx2c4.com/WireGuard/snapshot/WireGuard-0.0.20181119.tar.xz && \
	unxz WireGuard-0.0.20181119.tar.xz && tar xvf ./WireGuard-0.0.20181119.tar && rm WireGuard-0.0.20181119.tar
}

setup_apt()
{
    base_min
    apt update;apt install -y apt-transport-https ca-certificates curl dirmngr lsb-release \
		   --no-install-recommends
    cat <<-EOF > /etc/apt/sources.list.d/git-core.list
	deb http://ppa.launchpad.net/git-core/ppa/ubuntu xenial main
	deb-src http://ppa.launchpad.net/git-core/ppa/ubuntu xenial main
	EOF
    apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 \
	    --recv-keys E1DD270288B4E6030699E45FA1715D88E1DF1F24
    mkdir -p /etc/apt/apt.conf.d
    echo 'Acquire::Languages "none";' > /etc/apt/apt.conf.d/99translations
    cat <<-EOF > /etc/apt/sources.list
	deb http://httpredir.debian.org/debian stretch main contrib non-free
	# deb-src http://httpredir.debian.org/debian/ stretch main contrib non-free

	deb http://httpredir.debian.org/debian/ stretch-updates main contrib non-free
	# deb-src http://httpredir.debian.org/debian/ stretch-updates main contrib non-free

	deb http://security.debian.org/ stretch/updates main contrib non-free
	# deb-src http://security.debian.org/ stretch/updates main contrib non-free

	deb http://httpredir.debian.org/debian experimental main contrib non-free
	# deb-src http://httpredir.debian.org/debian experimental main contrib non-free

	deb http://ftp.debian.org/debian stretch-backports main
	EOF
    apt-get update;apt-get -t stretch-backports upgrade
    # And do whatever you want... like installing the latest kernel!
}

setup_sudo()
{
    local user="$1"
    if [ -z $user ]; then
	echo "Need username. $user"
	exit 1
    fi
    if [[ "$EUID" -ne 0 ]]; then
	echo "Run me as as root"
	exit 1
    fi
    gpasswd -a "$user" systemd-journal
    { \
      echo -e "$user ALL=(ALL) NOPASSWD:ALL"; \
      echo -e "$user ALL=NOPASSWD: /usr/bin/ping /sbin/ifconfig, /sbin/ifup, /sbin/ifdown, /sbin/ifquery"; \
      } >> /etc/sudoers
}

install_dotfiles()
{
    for file in "${DOT_FILES[@]}"; do
	 -f "${PROJECT_ROOT}/$file" ~
    done
    if [[ -f "/usr/lib/git-core/git-sh-prompt" ]]; then
	echo "cp /usr/lib/git-core/git-sh-prompt"
	cp -f /usr/lib/git-core/git-sh-prompt ~/.git-prompt.sh
    fi
}

usage()
{
    echo "usage:"
    echo " setup_sudo        need a \$user"
    echo " install_dotfiles  install .bashrc and others to \$HOME"
    echo " setup_apt         setup for debian9"
    echo " base_min          minimum installation"
    echo " install_scripts   install useful scripts"
    echo " install_golang    install_golang \$user"
    echo " install_docker    install_docker \$user"
    echo " install_wireguard install Wireguard as it says"
    exit 1
}

main()
{
    if [[ -z "$1" ]]; then
	usage
    fi
    case "$1" in
	"setup_sudo")
	    setup_sudo "$2"
	    ;;
	"install_dotfiles")
	    install_dotfiles
	    ;;
	"setup_apt")
	    setup_apt
	    ;;
	"base_min")
	    base_min
	    ;;
	"install_scripts")
	    install_scripts
	    ;;
	"install_golang")
	    install_golang "$2"
	    ;;
	"install_docker")
	    install_docker "$2"
	    ;;
	"install_wireguard")
	    install_wireguard "$2"
	    ;;
	"username")
	    check_username "$2"
	    ;;
    esac
}

main "$@"
echo $?
