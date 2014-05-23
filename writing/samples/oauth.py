request_token_url = 'http://twitter.com/oauth/request_token'
access_token_url = 'http://twitter.com/oauth/access_token'
authorize_url = 'http://twitter.com/oauth/authorize'

request_token = get_request_token(request_token_url)

# last three steps
access_token = request_access_token((get_pin(auth_url(request_token)))
