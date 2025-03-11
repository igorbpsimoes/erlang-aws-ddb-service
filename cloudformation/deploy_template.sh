#!/bin/bash

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 <template-name> [options]"
  exit 1
fi

TEMPLATE_NAME="$1"

SCRIPT_DIR=$(dirname "$0")

ENVIRONMENT_NAME=${ENVIRONMENT_NAME:-$(read -p 'Enter ENVIRONMENT_NAME: ' input && echo $input)}
VPC_ID="${VPC_ID:-$(read -p 'Enter VPC_ID: ' input && echo $input)}"
SUBNET_IDS="${SUBNET_IDS:-$(read -p 'Enter SUBNET_IDS: ' input && echo $input)}"

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

    VPC_CIDR_IP="${VPC_CIDR_IP:-$(read -p 'Enter VPC_CIDR_IP: ' input && echo $input)}"

    aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --capabilities CAPABILITY_NAMED_IAM \
        --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
        VpcId=${VPC_ID} \
        VpcCidrIp=${VPC_CIDR_IP} \
        SubnetIds=${SUBNET_IDS} \
        LocalIp=${LOCAL_IP}
    ;;
  task)
    TEMPLATE_FILE="$SCRIPT_DIR/task_template.yaml"
    STACK_NAME="${ENVIRONMENT_NAME}-task-stack"

    REPOSITORY_NAME="${REPOSITORY_NAME:-$(read -p 'Enter REPOSITORY_NAME: ' input && echo $input)}"
    IMAGE_TAG="$(git rev-parse --short HEAD)"

    aws cloudformation deploy --template-file $TEMPLATE_FILE --stack-name $STACK_NAME \
        --capabilities CAPABILITY_NAMED_IAM \
        --parameter-overrides EnvironmentName="${ENVIRONMENT_NAME}" \
          DockerRepository="${REPOSITORY_NAME}" \
          ImageTag="${IMAGE_TAG}"

    if [ "$2" = "--build-and-push-docker-image" ]; then
      echo "Building and pushing Docker image..."

      $SCRIPT_DIR/../erlang_dynamodb_service/build_and_push_docker_image.sh "$REPOSITORY_NAME" "$ENVIRONMENT_NAME"
    fi
    ;;
  service)
    TEMPLATE_FILE="$SCRIPT_DIR/service_template.yaml"
    STACK_NAME="${ENVIRONMENT_NAME}-service-stack"

    SECURITY_GROUP_IDS="${SECURITY_GROUP_IDS:-$(read -p 'Enter SECURITY_GROUP_IDS: ' input && echo $input)}"

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
