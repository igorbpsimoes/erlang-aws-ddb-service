# Release
Make sure to have credentials for AWS set before starting the release.

The script `deploy_template` in the cloudformation folder has everything you need to make all CloudFormation deployments.

You'll be prompted for some variables during script runs, if you want to export them prior it might be easier.
Here are the needed env variables:
- **ENVIRONMENT_NAME:** string
- **VPC_ID:** string
- **SUBNET_IDS:** string (comma delimited without spaces)
- **REPOSITORY_NAME:** string (docker repository - dynamodb_service suggested)
- **AWS_REGION:** string (eu-north-1 recommended)
- **ACCOUNT_ID:** string (aws account id - fetchable with `aws sts get-caller-identity`)
- **VPC_CIDR_IP:** string (fetchable with `aws ec2 describe-vpcs --vpc-ids $VPC_ID --query 'Vpcs[].CidrBlock' --output text`)

Don't export SECURITY_GROUP_IDS, as you should fetch it before running `./deploy_template service`

Run in the following order:
- `./cloudformation/deploy_template.sh dynamodb`
- `./cloudformation/deploy_template.sh cluster`

The task script will attempt to fetch your LOCAL_IP when it's run so you have permission to make requests to the NLB via test_client.
- `./cloudformation/deploy_template.sh task --build-and-push-docker-image`

You can know export SECURITY_GROUP_IDS (fetchable with aws ec2 describe-security-groups - find it by name according to the ENVIRONMENT_NAME provided)
Or just provide when requested:
- `./cloudformation/deploy_template.sh service`

You now should have everything set up.

# Test Client
1. Cd to erlang_dynamodb_service.
2. Fetch the NLB's DNS with `aws elbv2 describe-load-balancers`
3. Paste it in `test_client.erl`
3. Run `rebar3 as test compile`
4. Run `rebar3 as test shell`

In the shell, you can make requests by calling:
- `test_client:interactive().`
    - Accepts the following commands in a loop
    - set mysamplekey mysamplevalue
    - get mysamplekey
    - quit

- `test_client:set_kv("mysamplekey", "mysamplevalue").`
- `test_client:get_kv("mysamplekey").`
