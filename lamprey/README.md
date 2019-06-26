Notes relating to installation/support of lamprey population genomics project.

1. Installing [EEMS](https://github.com/dipetkov/eems)

Needs specific versions of Boost and Eigen - installed them using easybuild
```
# Example of Boost
cd /nesi/nobackup/uoo00110/bin

export NESI_EASYBUILD_PROJECT_ID=<projectID> # export NESI_EASYBUILD_PROJECT_ID=uoo00110
module load project

export EASYBUILD_INSTALLPATH_SOFTWARE=/nesi/nobackup/uoo00110/bin/Boost
export EASYBUILD_INSTALLPATH_MODULES=/nesi/nobackup/uoo00110/bin/modulefiles
export EASYBUILD_PREFIX=$EASYBUILD_INSTALLPATH_SOFTWARE/easybuild-resources  

mkdir easybuildfiles
cd easybuildfiles
# You figure out the path to the easybuild file by loading the new version of the module
# and then checking either your $PATH or $LIBRARY_PATH etc.
# Make sure you remove these module(s) before trying the eb command
cp /opt/nesi/CS400_centos7_bdw/Boost/1.69.0-GCCcore-7.4.0/easybuild/Boost-1.69.0-GCCcore-7.4.0.eb ./
mv Boost-1.69.0-GCCcore-7.4.0.eb Boost-1.57.0-GCCcore-7.4.0.eb
vi Boost-1.57.0-GCCcore-7.4.0.eb # change the version number within the file
eb Boost-1.57.0-GCCcore-7.4.0.eb

# You need to do the following before you can load the newly installed modules
module use /nesi/nobackup/uoo00110/bin/modulefiles/all
# Then...
module load Boost

# Example of Eigen
cd /nesi/nobackup/uoo00110/bin/easybuildfiles
cp /opt/nesi/CS400_centos7_bdw/Eigen/3.3.4/easybuild/Eigen-3.3.4.eb ./
mv Eigen-3.3.4.eb Eigen-3.2.2.eb
vi Eigen-3.2.2.eb # Change 3.4.4 to 3.2.2
export EASYBUILD_INSTALLPATH_SOFTWARE=/nesi/nobackup/uoo00110/bin/Eigen
export EASYBUILD_INSTALLPATH_MODULES=/nesi/nobackup/uoo00110/bin/modulefiles
export EASYBUILD_PREFIX=$EASYBUILD_INSTALLPATH_SOFTWARE/easybuild-resources 
eb Eigen-3.2.2.eb 

```
