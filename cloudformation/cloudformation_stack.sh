#!/bin/bash

ENVIRONMENT_NAME=${ENV_NAME}
SCRIPT_DIR=$(dirname "$0")

if [ "$1" == "deploy" ]; then
  case "$2" in
    dynamodb)
      TEMPLATE_FILE="$SCRIPT_DIR/dynamodb_template.yaml"
      STACK_NAME="${ENVIRONMENT_NAME}-dynamodb-stack"

      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --capabilities CAPABILITY_NAMED_IAM \
        --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}"
      ;;
    cluster)
      TEMPLATE_FILE="$SCRIPT_DIR/cluster_template.yaml"
      STACK_NAME="${ENVIRONMENT_NAME}-cluster-stack"
      LOCAL_IP=$(dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com)

      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
          --disable-rollback \
          --capabilities CAPABILITY_NAMED_IAM \
            --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
            VpcId=vpc-0aad473ad61e24e93 \
            SubnetIds="subnet-03578933c0eaa04e9" \
            LocalIp="${LOCAL_IP}"
      ;;
    task)
      TEMPLATE_FILE="$SCRIPT_DIR/task_template.yaml"
      STACK_NAME="my-test-service-stack"
      aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
          --disable-rollback \
          --capabilities CAPABILITY_NAMED_IAM \
          --parameter-overrides ClusterName="MyErlangCluster" \
            TaskDefinition="arn:aws:ecs:eu-north-1:831926581541:task-definition/task-definition-erlang-service:4" \
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
    *)
      echo "Unknown template shorthand: $2"
      exit 1
      ;;
  esac
else
  echo "Usage: $0 deploy <template-shorthand>"
  exit 1
fi
