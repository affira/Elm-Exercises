import bcrypt from 'bcryptjs';
import Joi from 'joi';
import db from '../db/db.mjs';

const schema = Joi.object({
  id: Joi.number()
    .integer()
    .min(1)
    .optional()
    .strip(),
  username: Joi.string()
    .trim()
    .normalize()
    .alphanum()
    .min(1)
    .max(20)
    .required(),
  password: Joi.string()
    .trim()
    .normalize()
    .min(1)
    .required()
});

export class UserValidationError extends Error {
  constructor (message, details) {
    super(message);
    this.name = 'UserValidationError';
    this.details = { ...details };
  }
}

export class UserLoginError extends Error {
  constructor (message) {
    super(message);
    this.name = 'UserLoginError';
  }
}

const validateUser = data => {
  const result = schema.validate({ ...data }, { convert: true, abortEarly: false });

  if (result.error) {
    const error = result.error.details.map(d => d.message);
    throw new UserValidationError('User validation failed', error);
  }

  return result.value;
};

const getUser = username => {
  const user = db.data?.users.find(user => user.username === username);
  if (!user) throw new UserLoginError('Login failed. Check username and password.');
  return { ...user };
};

const checkUsernameAvailability = username => {
  if (db.data?.users.some(user => user.username === username)) {
    const message = 'Username is already in use!';
    throw new UserValidationError('Username is already in use!', { username: message });
  }
};

const hashPassword = async password => {
  const hash = await bcrypt.hash(password, 10);
  return hash;
};

const verifyPassword = async (password, hashedPassword) => {
  const success = await bcrypt.compare(password, hashedPassword);
  return success;
};

export const registerUser = async data => {
  const result = validateUser(data);
  checkUsernameAvailability(result.username);

  if (!('metaData' in db.data)) db.data.metaData = { users: { nextUserId: 1 } };
  if (!('users' in db.data.metaData)) db.data.metaData.users = { nextUserId: 1 };
  result.id = db.data.metaData.users.nextUserId;
  result.password = await hashPassword(result.password);

  if (!('users' in db.data)) db.data.users = [];
  db.data.metaData.users.nextUserId += 1;
  db.data.users.push({ ...result });
  await db.write();

  const user = { ...result };
  delete user.password;
  return user;
};

export const loginUser = async ({ username, password }) => {
  const user = getUser(username);
  const success = await verifyPassword(password, user.password);
  if (!success) throw new LoginError('Login failed. Check username and password.');

  delete user.password;
  return user;
};
