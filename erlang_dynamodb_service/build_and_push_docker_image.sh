#!/bin/bash

if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <repository-name> <environment-name>"
  exit 1
fi

REPOSITORY_NAME="$1"
ENVIRONMENT_NAME="$2"
DOCKER_TAG="$(git rev-parse --short HEAD)"

AWS_REGION=${AWS_REGION:-$(read -p 'Enter AWS_REGION: ' input && echo $input)}
ACCOUNT_ID=${ACCOUNT_ID:-$(read -p 'Enter ACCOUNT_ID: ' input && echo $input)}

ECS_TASK_ROLE_ARN="arn:aws:iam::${ACCOUNT_ID}:role/${ENVIRONMENT_NAME}-EcsTaskRole"
ROLE_SESSION_NAME="${ENVIRONMENT_NAME}-task-role-session"
TASK_ROLE_PROFILE="${ENVIRONMENT_NAME}-task-role-profile"
ENV_REPO_NAME="${ENVIRONMENT_NAME}-${REPOSITORY_NAME}"
ECR_URL="$ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCKERFILE_PATH="$SCRIPT_DIR/Dockerfile"

docker buildx build \
  --build-arg ECS_TASK_ROLE_ARN="$ECS_TASK_ROLE_ARN" \
  --build-arg ROLE_SESSION_NAME="$ROLE_SESSION_NAME" \
  --build-arg TASK_ROLE_PROFILE="$TASK_ROLE_PROFILE" \
  --build-arg ENVIRONMENT_NAME="$ENVIRONMENT_NAME" \
  --build-arg AWS_REGION="$AWS_REGION" \
  --platform linux/amd64,linux/arm64 \
  -o type=docker \
  --target runner \
  --tag "$ENV_REPO_NAME:$DOCKER_TAG" \
  -f "$DOCKERFILE_PATH" "$SCRIPT_DIR"

# Authenticate Docker to AWS ECR
aws --region "$AWS_REGION" ecr get-login-password | \
  docker login --username AWS --password-stdin "$ECR_URL"

# Tag the image
docker tag "$ENV_REPO_NAME:$DOCKER_TAG" \
  "$ECR_URL/$ENV_REPO_NAME:$DOCKER_TAG"

# Push the image
docker push "$ECR_URL/$ENV_REPO_NAME:$DOCKER_TAG"
