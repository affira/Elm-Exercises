import debug from 'debug';
import { resetDb } from './db.mjs';

debug.enable('backend:*');
await resetDb();
