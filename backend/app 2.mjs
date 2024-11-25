import cors from 'cors';
import debug from 'debug';
import express from 'express';
import helmet from 'helmet';
import morgan from 'morgan';
import { dirname, resolve } from 'path';
import { fileURLToPath } from 'url';
import { getConfig } from './config.mjs';
import db from './db/db.mjs';
import { setupRoutes } from './router.mjs';

export const loadApp = (exercise) => {
  const dir = dirname(fileURLToPath(import.meta.url));
  const publicDir = resolve(dir, 'public');

  // Create logger for debugging
  // (Better console.log with colours and does not show any output in production)
  const log = debug('backend:app');
  const app = express();
  app.set('config', getConfig(exercise));

  // save db to the app
  app.set('db', db);

  // middleware & routes
  log('Loading middleware');
  app.use(morgan('dev'));
  app.use(cors());
  app.use(
    helmet({
      contentSecurityPolicy: false
    })
  );
  app.use(express.static(publicDir));
  app.use(express.json());
  setupRoutes(app);

  return app;
};
