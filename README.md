# Compile Emacs from scratch
These are some short instructions about compiling Emacs from scratch. Resources:

1. http://ergoemacs.org/emacs/building_emacs_from_git_repository.html
2. http://irreal.org/blog/?p=7096

Install dependencies (from the first article, probably outdated):

```shell
# install essential build tools
sudo apt-get install build-essential

# get all dependencies of a previous emacs version
sudo apt-get build-dep emacs23
```

Instructions to compile (or recompile) the source code:

```shell
git clone https://github.com/mirrors/emacs.git
cd emacs
git reset --hard
git clean -xdf
git pull
./autogen.sh
./configure
make bootstrap
make
sudo make install
sudo make install-info
```

Actually, we probably would not like to build the bleeding egde version. To list tags:

```shell
git tag
```

Choose the last "stable" tag and then:

```shell
git checkout <selected tag>
```