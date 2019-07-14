-record(request, {
        method, url, http_version,
        user_agent,
        content_length=0,
        body
       }).

-record(response, {
        status_code=200,
        content_type="text/html",
        content=""
       }).
