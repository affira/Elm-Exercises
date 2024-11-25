import express from 'express';
import {
  addPlayer,
  modifyPlayer,
  removePlayer,
  showAllPlayers,
  showPlayer
} from '../controller/player.mjs';
import requireAuthenticated from '../middleware/requireAuthenticated.mjs';
import requireJson from '../middleware/requireJson.mjs';

const router = express.Router();

// middleware
router.use(requireJson);

router
  .route('/players/:id([0-9]+)')
  .all(requireAuthenticated)
  .get(showPlayer)
  .put(modifyPlayer)
  .delete(removePlayer);

router
  .route('/players')
  .all(requireAuthenticated)
  .get(showAllPlayers)
  .post(addPlayer);

export default router;
