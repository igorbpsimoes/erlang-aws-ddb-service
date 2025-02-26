#!/bin/bash

if [ "$1" == "deploy-vpc" ]; then
  aws cloudformation deploy --template-file my-vpc.yaml --stack-name my-vpc-stack
fi
