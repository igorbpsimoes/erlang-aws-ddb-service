#!/bin/bash

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <aws-region> <account_id> <repository-name>"
  exit 1
fi

AWS_REGION="$1"
ACCOUNT_ID="$2"
REPOSITORY_NAME="$3"
DOCKER_TAG="$(git rev-parse --short HEAD)"

ENVIRONMENT_NAME="${ENVIRONMENT_NAME:-$(read -p 'Enter ENVIRONMENT_NAME: ' input && echo $input)}"

ECS_TASK_ROLE_ARN="$(read -p 'Enter ECS_TASK_ROLE_ARN: ' input && echo $input)"
ROLE_SESSION_NAME="$(read -p 'Enter ROLE_SESSION_NAME: ' input && echo $input)"
TASK_ROLE_PROFILE="$(read -p 'Enter TASK_ROLE_PROFILE: ' input && echo $input)"

ENV_REPO_NAME="${ENVIRONMENT_NAME}-${REPOSITORY_NAME}"
ECR_URL="$ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com"

docker buildx build \
  --build-arg ECS_TASK_ROLE_ARN="$ECS_TASK_ROLE_ARN" \
  --build-arg ROLE_SESSION_NAME="$ROLE_SESSION_NAME" \
  --build-arg TASK_ROLE_PROFILE="$TASK_ROLE_PROFILE" \
  --platform linux/amd64,linux/arm64 \
  -o type=docker \
  --target runner \
  --tag "$ENV_REPO_NAME:$DOCKER_TAG" .

# Authenticate Docker to AWS ECR
aws --region "$AWS_REGION" ecr get-login-password | \
  docker login --username AWS --password-stdin "$ECR_URL"

# Tag the image
docker tag "$ENV_REPO_NAME:$DOCKER_TAG" \
  "$ECR_URL/$ENV_REPO_NAME:$DOCKER_TAG"

# Push the image
docker push "$ECR_URL/$ENV_REPO_NAME:$DOCKER_TAG"
