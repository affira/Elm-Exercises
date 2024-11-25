/** @format */

import Joi from 'joi';
import db from '../db/db.mjs';

const schema = Joi.object({
	id: Joi.number().integer().min(1).optional().strip(),
	name: Joi.string()
		.trim()
		.normalize()
		.min(1)
		.max(100)
		.alter({
			update: (schema) => schema.optional(),
			create: (schema) => schema.required(),
		}),
	isActive: Joi.boolean()
		.falsy('N', 'No', 'F', 'False')
		.truthy('Y', 'Yes', 'T', 'True')
		.sensitive(false)
		.optional(),
}).or('name', 'isActive');

export class PlayerValidationError extends Error {
	constructor(message, details) {
		super(message);
		this.name = 'PlayerValidationError';
		this.details = Array.isArray(details) ? [...details] : { ...details };
	}
}

export class PlayerNotFoundError extends Error {
	constructor(message) {
		super(message);
		this.name = 'PlayerNotFoundError';
	}
}

const validatePlayer = (data, mode = 'update') => {
	const validator = schema.tailor(mode);
	const result = validator.validate(
		{ ...data },
		{ convert: true, abortEarly: false }
	);

	if (result.error) {
		const error = result.error.details.map((d) => d.message);
		throw new PlayerValidationError('Player validation failed', error);
	}

	return result.value;
};

const getPlayerIndex = (id) => {
	if (typeof id !== 'number') id = Number.parseInt(id, 10);
	if (!('players' in db.data))
		throw new PlayerNotFoundError('Player does not exist!');

	const index = db.data.players.findIndex((player) => player.id === id);
	if (index === -1) throw new PlayerNotFoundError('Player does not exist!');

	return index;
};

export const createPlayer = async (data) => {
	const result = validatePlayer(data, 'create');

	if (!('isActive' in result)) result.isActive = false;
	if (!('metaData' in db.data))
		db.data.metaData = { players: { nextPlayerId: 1 } };
	if (!('players' in db.data.metaData))
		db.data.metaData.players = { nextPlayerId: 1 };
	result.id = db.data.metaData.players.nextPlayerId;

	if (!('players' in db.data)) db.data.players = [];
	db.data.metaData.players.nextPlayerId += 1;
	db.data.players.push({ ...result });

	await db.write();
	return { ...result };
};

export const updatePlayer = async (id, data) => {
	const result = validatePlayer(data, 'update');
	const index = getPlayerIndex(id);

	const player = { ...db.data.players[index], ...result };
	db.data.players[index] = player;
	await db.write();
	return { ...player };
};

export const deletePlayer = async (id) => {
	const index = getPlayerIndex(id);
	const deletedPlayers = db.data.players.splice(index, 1);

	await db.write();
	return { ...deletedPlayers[0] };
};

export const getPlayer = (id) => {
	const index = getPlayerIndex(id);
	const player = { ...db.data.players[index] };
	return player;
};

export const getAllPlayers = () => {
	// Send after delay to simulate network latency

	// setTimeout(() => {
	if (!('players' in db.data)) return [];
	return db.data.players.reduce((players, player) => {
		players.push({ ...player });
		return players;
	}, []);
	// }, 1000);
};
