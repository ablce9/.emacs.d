#! /bin/bash

set -xe

PROJECT_ROOT=$(dirname "${BASH_SOURCE}")
DOT_FILES=(.aliases .bash_profile .bashrc .functions .gdbinit .gitconfig .git-prompt.sh .pep.cfg
)

base_min() {
    apt update;apt -y upgrade;apt install -y adduser automake bash-completion bc bzip2 ca-certificates coreutils curl dnsutils file findutils gcc	git gnupg gnupg2 gnupg-agent grep gzip hostname indent iptables jq less libc6-dev libimobiledevice6 locales lsof make mount net-tools pinentry-curses rxvt-unicode-256color scdaemon silversearcher-ag ssh strace	sudo tar tree tzdata usbmuxd unzip xclip xcompmgr xz-utils zip netcat-openbsd --no-install-recommends;apt autoremove;apt autoclean;apt clean
}

# install/update golang from source
install_golang() {
    local user="$1"

    if [ -z $user ]; then
	echo "Need username. $user"
	exit 1
    fi

    export GO_VERSION
    GO_VERSION=$(curl -sSL "https://golang.org/VERSION?m=text")
    export GO_SRC=/usr/local/go

    # if we are passing the version
    if [[ ! -z "$1" ]]; then
	GO_VERSION=$1
    fi

    # purge old src
    if [[ -d "$GO_SRC" ]]; then
	sudo rm -rf "$GO_SRC"
	sudo rm -rf "$GOPATH"
    fi

    GO_VERSION=${GO_VERSION#go}

    # subshell
    (
	kernel=$(uname -s | tr '[:upper:]' '[:lower:]')
	curl -sSL "https://storage.googleapis.com/golang/go${GO_VERSION}.${kernel}-amd64.tar.gz" | sudo tar -v -C /usr/local -xz
	# rebuild stdlib for faster builds
	sudo chown -R "${user}" /usr/local/go/pkg
	CGO_ENABLED=0 go install -a -installsuffix cgo std
    )
}

# install custom scripts/binaries
install_scripts() {
    # install speedtest
    curl -sSL https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py  > /usr/local/bin/speedtest
    chmod +x /usr/local/bin/speedtest

    # install icdiff
    curl -sSL https://raw.githubusercontent.com/jeffkaufman/icdiff/master/icdiff > /usr/local/bin/icdiff
    curl -sSL https://raw.githubusercontent.com/jeffkaufman/icdiff/master/git-icdiff > /usr/local/bin/git-icdiff
    chmod +x /usr/local/bin/icdiff
    chmod +x /usr/local/bin/git-icdiff

    # install lolcat
    curl -sSL https://raw.githubusercontent.com/tehmaze/lolcat/master/lolcat > /usr/local/bin/lolcat
    chmod +x /usr/local/bin/lolcat


    local scripts=( have light )

    for script in "${scripts[@]}"; do
	curl -sSL "https://misc.j3ss.co/binaries/$script" > "/usr/local/bin/${script}"
	chmod +x "/usr/local/bin/${script}"
    done
}

install_docker() {
    local user="$@"

    if [ -z $user ]; then
	echo "Need username. $USER"
	exit 1
    fi

    echo "+setup_docker for: $USER"

    # add docker apt repo
    cat <<-EOF > /etc/apt/sources.list.d/docker.list
	deb https://apt.dockerproject.org/repo debian-stretch main
	deb https://apt.dockerproject.org/repo debian-stretch testing
	deb https://apt.dockerproject.org/repo debian-stretch experimental
	EOF

    # add docker gpg key
    apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
    # create docker group
    sudo groupadd docker
    sudo gpasswd -a "$USER" docker

    # Include contributed completions
    mkdir -p /etc/bash_completion.d
    curl -sSL -o /etc/bash_completion.d/docker https://raw.githubusercontent.com/docker/docker-ce/master/components/cli/contrib/completion/bash/docker

    # get the binary
    local tmp_tar=/tmp/docker.tgz
    local binary_uri="https://download.docker.com/linux/static/edge/x86_64"
    local docker_version
    docker_version=$(curl -sSL "https://api.github.com/repos/docker/docker-ce/releases/latest" | jq --raw-output .tag_name)
    docker_version=${docker_version#v}
    # local docker_sha256
    # docker_sha256=$(curl -sSL "${binary_uri}/docker-${docker_version}.tgz.sha256" | awk '{print $1}')
    (
	set -x
	curl -fSL "${binary_uri}/docker-${docker_version}.tgz" -o "${tmp_tar}"
	# echo "${docker_sha256} ${tmp_tar}" | sha256sum -c -
	tar -C /usr/local/bin --strip-components 1 -xzvf "${tmp_tar}"
	rm "${tmp_tar}"
	docker -v
    )
    chmod +x /usr/local/bin/docker*
    curl -sSL https://raw.githubusercontent.com/jessfraz/dotfiles/master/etc/systemd/system/docker.service > /etc/systemd/system/docker.service
    curl -sSL https://raw.githubusercontent.com/jessfraz/dotfiles/master/etc/systemd/system/docker.socket > /etc/systemd/system/docker.socket
    systemctl daemon-reload
    systemctl enable docker
    echo 'done'
}

# apt for debian9.
setup_apt()
{
    base_min

    apt update
    apt install -y \
	apt-transport-https \
	ca-certificates \
	curl \
	dirmngr \
	lsb-release \
	--no-install-recommends

    # hack for latest git (don't judge)
    cat <<-EOF > /etc/apt/sources.list.d/git-core.list
	deb http://ppa.launchpad.net/git-core/ppa/ubuntu xenial main
	deb-src http://ppa.launchpad.net/git-core/ppa/ubuntu xenial main
	EOF

    # add the git-core ppa gpg key
    apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys E1DD270288B4E6030699E45FA1715D88E1DF1F24

    # turn off translations, speed up apt update
    mkdir -p /etc/apt/apt.conf.d
    echo 'Acquire::Languages "none";' > /etc/apt/apt.conf.d/99translations

    cat <<-EOF > /etc/apt/sources.list
	deb http://httpredir.debian.org/debian stretch main contrib non-free
	deb-src http://httpredir.debian.org/debian/ stretch main contrib non-free

	deb http://httpredir.debian.org/debian/ stretch-updates main contrib non-free
	deb-src http://httpredir.debian.org/debian/ stretch-updates main contrib non-free

	# deb http://security.debian.org/ stretch/updates main contrib non-free
	# deb-src http://security.debian.org/ stretch/updates main contrib non-free

	#deb http://httpredir.debian.org/debian/ jessie-backports main contrib non-free
	#deb-src http://httpredir.debian.org/debian/ jessie-backports main contrib non-free

	deb http://httpredir.debian.org/debian experimental main contrib non-free
	deb-src http://httpredir.debian.org/debian experimental main contrib non-free
	EOF
}

# Careful here. This could mess with sudo command.
setup_sudo()
{
    local user="$1"

    if [ -z $user ]; then
	echo "Need username. $user"
	exit 1
    fi

    echo "+setup_sudo for: $user"

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
    echo '+install_dotfiles'

    for file in "${DOT_FILES[@]}"; do
	cp -f "${PROJECT_ROOT}/$file" ~
    done

    # If we have it, use it
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
	    install_docker
	    ;;
    esac
}

main "$@"
