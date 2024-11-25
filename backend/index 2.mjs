import debug from 'debug';
import http from 'http';
import { argv, env } from 'process';
import { loadApp } from './app.mjs';

// Create logger for debugging
// (Better console.log with colours and does not show any output in production)
const log = debug('backend:server');

const exercise = argv.length === 3 ? argv[2] : 'Undefined';
exercise && log(`Exercise: ${exercise}`);
const app = loadApp(exercise);

// Get port from environment and store in Express.
const serverListenPort = normalizePort(8080);
app.set('port', serverListenPort);

// Create HTTP server.
const server = http.createServer(app);
server.on('error', onError);
server.on('listening', onListening);
server.listen(serverListenPort);

function normalizePort(val) {
  const port = Number.parseInt(val, 10);

  if (isNaN(port)) {
    // named pipe
    return val;
  }

  if (port >= 0) {
    // port number
    return port;
  }

  return false;
}

function onError(error) {
  if (error.syscall !== 'listen') {
    throw error;
  }

  const port = app.get('port');
  const bind = typeof port === 'string' ? `Pipe ${port}` : `Port ${port}`;

  // handle specific listen errors with friendly messages
  if (error.code === 'EACCES') {
    log(`${bind} requires elevated privileges`);
    process.exit(1);
  } else if (error.code === 'EADDRINUSE') {
    log(`${bind} is already in use`);
    process.exit(1);
  } else {
    throw error;
  }
}

function onListening() {
  const addr = server.address();
  const bind = typeof addr === 'string' ? `pipe ${addr}` : `port ${addr.port}`;
  log(`Listening on ${bind}`);
}
