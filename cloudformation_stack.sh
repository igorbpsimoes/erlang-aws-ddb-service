#!/bin/bash

if [ "$1" == "deploy" ]; then
  case "$2" in
    vpc)
      TEMPLATE_FILE="my-vpc.yaml"
      STACK_NAME="my-vpc-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME
      ;;
    dynamodb)
      TEMPLATE_FILE="my-dynamodb.yaml"
      STACK_NAME="my-dynamodb-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --capabilities CAPABILITY_NAMED_IAM \
        --parameter-overrides EnvironmentName=dev \
        KmsKeyArn="arn:aws:kms:eu-north-1:831926581541:key/41c1ee0f-af04-4344-8633-a17d1e5ee188"
      ;;
    *)
      echo "Unknown template shorthand: $2"
      exit 1
      ;;
  esac
else
  echo "Usage: $0 deploy <template-shorthand>"
  exit 1
fi
