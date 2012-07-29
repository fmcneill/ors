#!/bin/sh
# The user can set the environment variable SP_PATH to override
# the SICStus path used

echo Starting Java GUI client
echo You need to start Prolog server separately using something like:
echo "startpa."

java -classpath "$SP_PATH/bin/prologbeans.jar:." Client
