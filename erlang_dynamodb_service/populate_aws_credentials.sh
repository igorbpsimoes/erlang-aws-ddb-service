#!/bin/bash

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <role-arn> <role-session-name> <profile-name>"
  exit 1
fi

ROLE_ARN="$1"
ROLE_SESSION_NAME="$2"
PROFILE_NAME="$3"

AWS_OUTPUT=$(aws sts assume-role \
  --role-arn "$ROLE_ARN" \
  --role-session-name "$ROLE_SESSION_NAME" \
  --query "Credentials.[AccessKeyId,SecretAccessKey,SessionToken]" \
  --output text)

read SCRIPT_AWS_ACCESS_KEY_ID SCRIPT_AWS_SECRET_ACCESS_KEY SCRIPT_AWS_SECURITY_TOKEN <<< "$AWS_OUTPUT"

cat <<EOF >> ~/.aws/credentials
[$PROFILE_NAME]
aws_access_key_id = $SCRIPT_AWS_ACCESS_KEY_ID
aws_secret_access_key = $SCRIPT_AWS_SECRET_ACCESS_KEY
aws_security_token = $SCRIPT_AWS_SECURITY_TOKEN
EOF
