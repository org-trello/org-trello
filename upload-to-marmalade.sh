#!/usr/bin/env bash
# depends on curl

if [ $# -ne 1 ]; then
    cat <<EOF
Use: $0 <VERSION>"
- VERSION    version to release (0.1.6 for example)

EOF
    exit 1;
fi

VERSION=$1

uploadFile () {
    local username=$1
    local tokenFile=$(cat $2)
    local packageFile=$3
    local marmaladeUrl=${4:-"https://marmalade-repo.org"}
    curl --insecure \
         -F "name=${username}" \
         -F "token=$tokenFile" \
         -F "package=@${packageFile};filename=$(basename ${packageFile})" \
        $marmaladeUrl/v1/packages
}

uploadFile ardumont ~/.marmalade/token ./org-trello-$VERSION.tar
