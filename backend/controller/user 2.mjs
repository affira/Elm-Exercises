import debug from 'debug';
import { registerUser, UserValidationError } from '../model/user.mjs';

// Create logger for debugging
// (Better console.log with colours and does not show any output in production)
const log = debug('backend:user-controller');

export const register = async (req, res, next) => {
  try {
    const user = await registerUser(req.body);
    return res.status(201).json(user);
  } catch (err) {
    log('Registration failed');
    if (err instanceof UserValidationError) {
      res.status(400);
      return res.json(err.details);
    }

    res.status(500);
    res.json({ error: err.message });
  }
};
