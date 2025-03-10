#!/bin/bash

ENVIRONMENT_NAME=${ENV_NAME}

if [ "$1" == "deploy" ]; then
  case "$2" in
    vpc)
      TEMPLATE_FILE="my-vpc.yaml"
      STACK_NAME="my-vpc-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME
      ;;
    dynamodb)
      TEMPLATE_FILE="dynamodb_template.yaml"
      STACK_NAME="${ENVIRONMENT_NAME}-dynamodb-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --capabilities CAPABILITY_NAMED_IAM \
        --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
        KmsKeyArn="arn:aws:kms:eu-north-1:831926581541:key/41c1ee0f-af04-4344-8633-a17d1e5ee188"
      ;;
    cluster)
      TEMPLATE_FILE="cluster_template.yaml"
      STACK_NAME="${ENVIRONMENT_NAME}-cluster-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
          --disable-rollback \
          --capabilities CAPABILITY_NAMED_IAM \
          --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
            VpcId=vpc-0aad473ad61e24e93 \
            SubnetIds="subnet-03578933c0eaa04e9" \
            LocalIp=79.169.138.229
      ;;
    service)
      TEMPLATE_FILE="my-service.yaml"
      STACK_NAME="my-service-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
          --disable-rollback \
          --capabilities CAPABILITY_NAMED_IAM \
          --parameter-overrides ClusterName="MyErlangCluster" \
            TaskDefinition="arn:aws:ecs:eu-north-1:831926581541:task-definition/task-definition-erlang-service:4" \
      ;;
    test)
      TEMPLATE_FILE="my-service.yaml"
      STACK_NAME="my-test-service-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
          --disable-rollback \
          --capabilities CAPABILITY_NAMED_IAM \
          --parameter-overrides ClusterName="MyErlangCluster" \
            TaskDefinition="arn:aws:ecs:eu-north-1:831926581541:task-definition/task-definition-erlang-service:4" \
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
