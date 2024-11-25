import debug from 'debug';
import { JSONFile, Low } from 'lowdb';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';
import data from './reset/data.mjs';

// Create logger for debugging
// (Better console.log with colours and does not show any output in production)
const log = debug('backend:database');

// Use JSON file for storage
log('Loading database')
const currentDir = dirname(fileURLToPath(import.meta.url));
const file = join(currentDir, 'db.json');
const adapter = new JSONFile(file);
const db = new Low(adapter);
await db.read();

// If file.json doesn't exist, db.data will be null
// Set default data
db.data = db.data || data;

export const resetDb = async () => {
  log('Resetting database')
  db.data = data;
  await db.write();
};

export default db;
