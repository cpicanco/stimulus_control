# install instructions

# make sure system is up to date
sudo apt-get update
sudo apt-get upgrade

# install database dependencies
mysql-server
mysql-client
mysql-workbench

# check if database version is compatible with lazarus SQLdb connection component

TMySQLXXConnection

# downgrade if necessary. SQLdb component version must match mysql-client version.

mysql -V # client version

# on windows, check if libMySQL.dll is necessary...


