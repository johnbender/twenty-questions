import os
from werkzeug.wrappers import Request, Response
from werkzeug.wsgi import SharedDataMiddleware

def app(environ, start_response):
    request = Request(environ)
    response = Response("Four oh Four", status=404, mimetype='text/plain')
    return response(environ, start_response)

app = SharedDataMiddleware(app, {
    '/': os.path.join(os.path.dirname(__file__), 'static/')
})

if __name__ == '__main__':
    from werkzeug.serving import run_simple
    run_simple('127.0.0.1', 5000, app, use_debugger=True, use_reloader=True)
