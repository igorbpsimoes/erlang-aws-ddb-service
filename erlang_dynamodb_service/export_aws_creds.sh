#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <profile-name>"
  exit 1
fi

PROFILE_NAME="$1"

AWS_OUTPUT=$(aws configure export-credentials --profile $PROFILE_NAME --format env-no-export)

SCRIPT_AWS_ACCESS_KEY_ID=$(echo "$AWS_OUTPUT" | grep '^AWS_ACCESS_KEY_ID=' | cut -d '=' -f2)
SCRIPT_AWS_SECRET_ACCESS_KEY=$(echo "$AWS_OUTPUT" | grep '^AWS_SECRET_ACCESS_KEY=' | cut -d '=' -f2)
SCRIPT_AWS_SECURITY_TOKEN=$(echo "$AWS_OUTPUT" | grep '^AWS_SESSION_TOKEN=' | cut -d '=' -f2)

cat <<EOF >> ~/.aws/credentials
[$PROFILE_NAME]
aws_access_key_id = $SCRIPT_AWS_ACCESS_KEY_ID
aws_secret_access_key = $SCRIPT_AWS_SECRET_ACCESS_KEY
aws_security_token = $SCRIPT_AWS_SECURITY_TOKEN
EOF
