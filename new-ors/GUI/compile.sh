#!/bin/sh
# The user can set the environment variable SP_PATH to override
# the SICStus path used

javac -classpath "$SP_PATH/bin/prologbeans.jar:." Client.java
