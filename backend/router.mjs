import debug from 'debug';
import ApiRouter from './routes/api.mjs';
import UsersRouter from './routes/users.mjs';

// Create logger for debugging
// (Better console.log with colours and does not show any output in production)
const log = debug('backend:router');

export const setupRoutes = app => {
  log('Setting up API routes');
  app.use('/api', ApiRouter);

  const config = app.get('config');
  if (config?.enableAuthentication) {
    log('Setting up user registration route');
    app.use('/api/users', UsersRouter);
  }
};
