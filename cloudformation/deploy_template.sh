#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <template-name>"
  exit 1
fi

TEMPLATE_NAME="$1"

ENVIRONMENT_NAME=${ENVIRONMENT_NAME:-$(read -p 'Enter ENVIRONMENT_NAME: ' input && echo $input)}
SCRIPT_DIR=$(dirname "$0")

case $TEMPLATE_NAME in
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
    LOCAL_IP=$(dig -4 TXT +short o-o.myaddr.l.google.com @ns1.google.com | tr -d '"')

    aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --capabilities CAPABILITY_NAMED_IAM \
        --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
        VpcId=vpc-0aad473ad61e24e93 \
        SubnetIds="subnet-03578933c0eaa04e9" \
        LocalIp=${LOCAL_IP}
    ;;
  task)
    TEMPLATE_FILE="$SCRIPT_DIR/task_template.yaml"
    STACK_NAME="${ENVIRONMENT_NAME}-task-stack"

    REPOSITORY_NAME="${REPOSITORY_NAME:-$(read -p 'Enter REPOSITORY_NAME: ' input && echo $input)}"
    ENV_REPO_NAME="${ENVIRONMENT_NAME}-${REPOSITORY_NAME}"

    IMAGE_TAG="$(git rev-parse --short HEAD)"

    aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --capabilities CAPABILITY_NAMED_IAM \
        --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
          DockerRepository="${ENV_REPO_NAME}" \
          ImageTag="${IMAGE_TAG}"
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
