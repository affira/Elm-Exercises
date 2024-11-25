/** @format */

import debug from "debug";
import {
  createPlayer,
  deletePlayer,
  getAllPlayers,
  getPlayer,
  PlayerNotFoundError,
  PlayerValidationError,
  updatePlayer,
} from "../model/player.mjs";

// Create logger for debugging
// (Better console.log with colours and does not show any output in production)
const log = debug("backend:player-controller");

export const addPlayer = async (req, res, next) => {
  log("Creating player...");

  try {
    const player = await createPlayer(req.body);
    res.status(201);
    return res.json(player);
  } catch (err) {
    log("Player creation failed");
    if (err instanceof PlayerValidationError) {
      res.status(400);
      return res.json(err.details);
    }

    res.status(500);
    res.json({ error: err.message });
  }
};

export const removePlayer = async (req, res, next) => {
  try {
    const player = await deletePlayer(req.params.id);
    return res.json(player);
  } catch (err) {
    log("Player deletion failed");
    if (err instanceof PlayerNotFoundError) {
      res.status(404);
      return res.json({ error: err.message });
    }

    res.status(500);
    res.json({ error: err.message });
  }
};

export const showAllPlayers = (req, res, next) => {
  const config = req.app.get("config");
  let players = getAllPlayers();

  if (config?.allPlayersHideIsActive) {
    players = players.reduce((results, player) => {
      results.push({
        id: player.id,
        name: player.name,
      });
      return results;
    }, []);
  }

  // send after a timeout to simulate a slow connection
  console.log("Sending player list after 1 second delay...");

  setTimeout(() => res.json(players), 1000);
  // res.json(players);
};

export const modifyPlayer = async (req, res, next) => {
  try {
    const player = await updatePlayer(req.params.id, req.body);
    res.json(player);
  } catch (err) {
    log("Player update failed");
    if (err instanceof PlayerNotFoundError) {
      res.status(404);
      return res.json({ error: err.message });
    } else if (err instanceof PlayerValidationError) {
      res.status(400);
      return res.json(err.details);
    }

    res.status(500);
    res.json({ error: err.message });
  }
};

export const showPlayer = (req, res, next) => {
  try {
    console.log("Sending 1 player after 1 second delay...");
    const player = getPlayer(req.params.id);
    // 1 second delay
    setTimeout(() => res.json(player), 1000);
  } catch (err) {
    log("Player fetching failed");
    if (err instanceof PlayerNotFoundError) {
      res.status(404);
      return res.json({ error: err.message });
    }

    res.status(500);
    res.json({ error: err.message });
  }
};
