#!/bin/bash

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <aws-region> <ecr-url> <repository-name>"
  exit 1
fi

AWS_REGION="$1"
ECR_URL="$2"
REPOSITORY_NAME="$3"
DOCKER_TAG="$(git rev-parse --short HEAD)"

ASSUME_ROLE_ARN="${ASSUME_ROLE_ARN:-$(read -p 'Enter ASSUME_ROLE_ARN: ' input && echo $input)}"
ROLE_SESSION_NAME="${ROLE_SESSION_NAME:-$(read -p 'Enter ROLE_SESSION_NAME: ' input && echo $input)}"
TASK_ROLE_PROFILE="${TASK_ROLE_PROFILE:-$(read -p 'Enter TASK_ROLE_PROFILE: ' input && echo $input)}"

docker buildx build \
  --build-arg ASSUME_ROLE_ARN="$ASSUME_ROLE_ARN" \
  --build-arg ROLE_SESSION_NAME="$ROLE_SESSION_NAME" \
  --build-arg TASK_ROLE_PROFILE="$TASK_ROLE_PROFILE" \
  --platform linux/amd64,linux/arm64 \
  -o type=docker \
  --target runner \
  --tag "$REPOSITORY_NAME:$DOCKER_TAG" .

# Authenticate Docker to AWS ECR
aws --region "$AWS_REGION" ecr get-login-password | \
  docker login --username AWS --password-stdin "$ECR_URL"

# Tag the image
docker tag "$REPOSITORY_NAME:$DOCKER_TAG" \
  "$ECR_URL/$REPOSITORY_NAME:$DOCKER_TAG"

# Push the image
docker push "$ECR_URL/$REPOSITORY_NAME:$DOCKER_TAG"
