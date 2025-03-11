#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <template-name>"
  exit 1
fi

TEMPLATE_NAME="$1"

ENVIRONMENT_NAME=${ENVIRONMENT_NAME:-$(read -p 'Enter ENVIRONMENT_NAME: ' input && echo $input)}
SCRIPT_DIR=$(dirname "$0")

VPC_ID=vpc-0aad473ad61e24e93
SUBNET_IDS="subnet-03578933c0eaa04e9"
SECURITY_GROUP_IDS="sg-0f5a70027c5af299c"

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
        VpcId=${VPC_ID} \
        SubnetIds=${SUBNET_IDS} \
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
    TEMPLATE_FILE="$SCRIPT_DIR/service_template.yaml"
    STACK_NAME="${ENVIRONMENT_NAME}-service-stack"

    aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --disable-rollback \
        --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
          VpcId=${VPC_ID} \
          SubnetIds=${SUBNET_IDS} \
          SecurityGroupIds=${SECURITY_GROUP_IDS} \
    ;;
  *)
    echo "Unknown template shorthand: $2"
    exit 1
    ;;
esac
