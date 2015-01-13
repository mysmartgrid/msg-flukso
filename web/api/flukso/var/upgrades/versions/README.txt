
#####################################
# How to organize version directories

Each firmware version must have a directory here named as the exact version name, such as 2.0.3-1. Such directory must contain a script called ./install.sh, with read/execute permissions, whose purpose is to upgrade the Flukso firmware from the previous version to one in question.

Any additional files or subdirectories needed by install.sh must be placed in this directory. The alphabetical order must match the release order.
