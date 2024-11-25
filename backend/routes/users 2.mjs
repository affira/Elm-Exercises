import express from 'express';
import { register } from '../controller/user.mjs';
import requireJson from '../middleware/requireJson.mjs';

const router = express.Router();
router.use(requireJson);
router.route('/').post(register);

export default router;
