#!/bin/sh
find ~/.m2/repository -name "_remote.repositories" | xargs rm
find ~/.m2/repository -name "*.lastUpdated" | xargs rm
