/** @format */

describe('Players CRUD2', function () {
	const playersData = [
		{
			id: 1,
			name: 'Player One',
			isActive: false,
		},
		{
			id: 2,
			name: 'Player Two',
			isActive: true,
		},
		{
			id: 3,
			name: 'Player Three',
			isActive: false,
		},
	];

	let server;

	const delay = (delay, cb = null) => {
		return new Promise((resolve) =>
			setTimeout(() => {
				if (typeof cb === 'function') return resolve(cb());
				resolve();
			}, delay)
		);
	};

	const initElm = () => {
		// Init the Elm application
		const app = Elm.Main.init({
			node: document.getElementById('app'),
		});

		return app;
	};

	const setupServer = () => {
		// Fake GET one player
		server.respondWith('GET', /\/api\/players\/(\d+)$/, (xhr, id) => {
			const player = playersData.find(
				(player) => player.id === Number.parseInt(id)
			);

			if (!player) {
				xhr.respond(404);
				return;
			}

			xhr.respond(
				200,
				{ 'Content-Type': 'application/json' },
				JSON.stringify(player)
			);
		});

		// Fake PUT player
		server.respondWith('PUT', /\/api\/players\/(\d+)$/, (xhr, id) => {
			const player = playersData.find(
				(player) => player.id === Number.parseInt(id)
			);

			if (!player) {
				xhr.respond(404);
				return;
			}

			xhr.respond(
				200,
				{ 'Content-Type': 'application/json' },
				JSON.stringify({ ...player, isActive: !player.isActive })
			);
		});

		// Fake DELETE player
		server.respondWith('DELETE', /\/api\/players\/(\d+)$/, (xhr, id) => {
			const player = playersData.find(
				(player) => player.id === Number.parseInt(id)
			);
			const status = player ? 204 : 404;
			xhr.respond(status);
		});

		// Fake GET all players
		server.respondWith('GET', /\/api\/players\/$/, (xhr) => {
			xhr.respond(
				200,
				{ 'Content-Type': 'application/json' },
				JSON.stringify(playersData)
			);
		});

		// Fake POST new player
		server.respondWith('POST', /\/api\/players\/$/, (xhr) => {
			xhr.respond(
				201,
				{ 'Content-Type': 'application/json' },
				JSON.stringify({ id: 4, name: 'Player Four', isActive: false })
			);
		});
	};

	beforeEach(function () {
		// Fake XMLHttpRequest
		server = sinon.fakeServer.create({
			autoRespond: false,
			autoResponseAfter: 10,
			respondImmediately: false,
			fakeHTTPMethods: false,
		});

		setupServer();
	});

	afterEach(function () {
		// Restore original XMLHttpRequest
		server.restore();
		const wrapper = document.querySelector('#wrapper');
		wrapper.innerHTML = '<div id="app"></div>';
	});

	it('should fetch player data from the backend with a GET request', async function () {
		const app = initElm();

		chai
			.expect(server.requests.length)
			.to.equal(1, 'Did not receive a request to the backend API');
		chai
			.expect(server.requests[0].url)
			.to.match(/\/api\/players\/$/, 'Incorrect API address');

		chai
			.expect(server.requests[0].method)
			.to.equal('GET', 'Player data should fetched with GET request');

		const requestStatus = document.querySelector('#request-status');
		chai.expect(requestStatus).to.not.be.null;
		chai
			.expect(requestStatus.textContent.trim().toLowerCase())
			.to.equal(
				'loading...',
				'Incorrect request status: should show "Loading..." while waiting for data'
			);

		server.respond();
		chai.expect(server.requests[0].status).to.equal(200);
	});

	it('should populate #players-list with fetched players', async function () {
		server.autoRespond = true;
		const app = initElm();
		// await delay(400);

		function waitForTextContent(timeout = 1000, selector, value) {
			const element = document.querySelector(selector);
			return new Promise((resolve, reject) => {
				const timeoutId = setTimeout(() => {
					clearInterval(intervalId);
					reject(
						new Error(
							'Timed out waiting for textContent to change in ' + selector
						)
					);
				}, timeout);
				const intervalId = setInterval(() => {
					if (element.textContent === value) {
						clearInterval(intervalId);
						clearTimeout(timeoutId);
						resolve();
					}
				}, 20);
			});
		}

		await waitForTextContent(1000, '#request-status', '')
			.then(() => {
				// Check the current value of the textContent
				const requestStatus = document.querySelector('#request-status');

				const listedPlayers = document.querySelectorAll('#players-list li');

				chai
					.expect(listedPlayers.length)
					.to.equal(
						playersData.length,
						`#players-list has too ${
							listedPlayers.length > playersData.length + 1 ? 'many' : 'few'
						} players`
					);

				listedPlayers.forEach((playerItem, i) => {
					chai.expect(playerItem.id).to.exist;
					chai
						.expect(playerItem.id.toLowerCase())
						.to.be.equal(
							`player-${playersData[i].id}`,
							'Id attribute of the list item should be of the form "player-<ID>"'
						);

					const nameDiv = playerItem.querySelector('.player-name');
					const statusInput = playerItem.querySelector('label .player-status');

					chai.expect(nameDiv).to.not.be.null;
					chai.expect(statusInput).to.not.be.null;

					chai
						.expect(nameDiv.textContent.trim())
						.to.equal(playersData[i].name, 'Player name is incorrect.');

					chai.expect(statusInput.type.toLowerCase()).to.equal('checkbox');
					chai.expect(statusInput.checked).to.equal(playersData[i].isActive);
				});
			})
			.catch((error) => {
				// The textContent of element did not change to 'new value' within timelimit
				chai.expect.fail(error, null, error.message);
			});
	});

	it('should send new player data to the backend in a POST request', async function () {
		server.autoRespond = true;
		const app = initElm();
		await delay(100);
		server.autoRespond = false;

		const playerInput = document.querySelector('#input-player');
		const addButton = document.querySelector('#btn-add');

		chai.expect(playerInput).to.not.be.null;
		chai.expect(addButton).to.not.be.null;

		playerInput.value = 'Player Four';
		const inputEvt = new Event('input', { bubbles: true, cancelable: true });
		playerInput.dispatchEvent(inputEvt);

		// allow inputEvt to be dispatched before clicking button
		await delay(10, () => addButton.click());

		chai
			.expect(server.requests.length)
			.to.equal(2, 'Did not receive a request to the backend API');
		chai
			.expect(server.requests[1].url)
			.to.match(/\/api\/players\/$/, 'Incorrect API address');

		chai
			.expect(server.requests[1].method)
			.to.equal('POST', 'New player should be added with POST request');

		chai
			.expect(JSON.parse(server.requests[1].requestBody))
			.to.be.an('object')
			.that.includes(
				{ name: 'Player Four' },
				'Request body is not a valid JSON object or the name of the player is missing/incorrect'
			);

		server.respond();
		chai.expect(server.requests[1].status).to.equal(201);
		await delay(100);
	});

	it('should add new player to the end of #players-list with status "not active"', async function () {
		server.autoRespond = true;
		const app = initElm();

		const playerInput = document.querySelector('#input-player');
		const addButton = document.querySelector('#btn-add');

		chai.expect(playerInput).to.not.be.null;
		chai.expect(addButton).to.not.be.null;

		playerInput.value = 'Player Four';
		const inputEvt = new Event('input', { bubbles: true, cancelable: true });
		playerInput.dispatchEvent(inputEvt);

		// allow inputEvt to be dispatched before clicking button
		await delay(10, () => addButton.click());

		// wait for the DOM to update
		await delay(100);
		const listedPlayers = document.querySelectorAll('#players-list li');

		chai
			.expect(listedPlayers.length)
			.to.equal(
				playersData.length + 1,
				`Added one new player but #players-list has too ${
					listedPlayers.length > playersData.length + 1 ? 'many' : 'few'
				} players`
			);

		const lastPlayer = listedPlayers[playersData.length];

		chai.expect(lastPlayer.id).to.exist;

		chai
			.expect(lastPlayer.id.toLowerCase())
			.to.be.equal(
				'player-4',
				'Id attribute of the list item should be of the form "player-<ID>"'
			);

		const nameDiv = lastPlayer.querySelector('.player-name');
		const statusInput = lastPlayer.querySelector('label .player-status');

		chai.expect(nameDiv).to.not.be.null;
		chai.expect(statusInput).to.not.be.null;

		chai
			.expect(nameDiv.textContent.trim())
			.to.equal('Player Four', 'Player name is incorrect.');

		chai.expect(statusInput.type.toLowerCase()).to.equal('checkbox');
		chai.expect(statusInput.checked).to.be.false;
	});

	it('should send player status change to the backend in a PUT request', async function () {
		server.autoRespond = true;
		const app = initElm();
		await delay(100);

		const listedPlayers = document.querySelectorAll('#players-list li');
		chai
			.expect(listedPlayers.length)
			.to.equal(
				playersData.length,
				'#players-list should include all players which were fetched from the backend'
			);

		// Select Player Two
		const statusInput = listedPlayers[1].querySelector('label .player-status');

		chai.expect(statusInput).to.not.be.null;
		chai.expect(statusInput.type.toLowerCase()).to.equal('checkbox');
		chai.expect(statusInput.checked).to.be.true;

		server.autoRespond = false;
		statusInput.checked = false;
		const changeEvent = new Event('change', {
			bubbles: true,
			cancelable: true,
		});
		statusInput.dispatchEvent(changeEvent);
		await delay(10); // wait for the event

		chai
			.expect(server.requests.length)
			.to.equal(2, 'Did not receive a request to the backend API');

		chai
			.expect(server.requests[1].url)
			.to.match(/\/api\/players\/\d+$/, 'Incorrect API address');

		chai
			.expect(server.requests[1].method)
			.to.equal('PUT', 'Player editing should use PUT request');

		chai
			.expect(JSON.parse(server.requests[1].requestBody))
			.to.be.an('object')
			.that.includes(
				{ isActive: !playersData[1].isActive },
				'Request body is not a valid JSON object or the status of the player is missing/incorrect'
			);

		server.respond();
		chai.expect(server.requests[1].status).to.equal(200);
	});

	it('should send DELETE request to backend when "Delete" button is clicked!', async function () {
		server.autoRespond = true;
		const app = initElm();
		await delay(100);

		const listedPlayers = document.querySelectorAll('#players-list li');
		chai
			.expect(listedPlayers.length)
			.to.equal(
				playersData.length,
				'#players-list should include all players which were fetched from the backend'
			);

		// Select Player Two
		const statusInput = listedPlayers[1].querySelector('label .player-status');

		chai.expect(statusInput).to.not.be.null;
		chai.expect(statusInput.type.toLowerCase()).to.equal('checkbox');
		chai.expect(statusInput.checked).to.be.true;

		server.autoRespond = false;
		const deleteButton = listedPlayers[1].querySelector('.btn-delete');

		chai.expect(deleteButton).to.not.be.null;
		deleteButton.click();

		chai
			.expect(server.requests.length)
			.to.equal(2, 'Did not receive a request to the backend API');

		chai
			.expect(server.requests[1].url)
			.to.match(/\/api\/players\/\d+$/, 'Incorrect API address');

		chai
			.expect(server.requests[1].method)
			.to.equal('DELETE', 'Player deleting should use DELETE request');

		server.respond();
		chai.expect(server.requests[1].status).to.equal(204);
	});

	it('should delete player from #players-list when "Delete" button is clicked', async function () {
		server.autoRespond = true;
		const app = initElm();
		await delay(100);

		const deleteButton = document.querySelector(
			'#players-list li#player-2 .btn-delete'
		);
		deleteButton.click();
		await delay(100);

		const listedPlayers = document.querySelectorAll('#players-list li');

		chai
			.expect(listedPlayers.length)
			.to.equal(
				playersData.length - 1,
				'Clicking "Delete" button should delete player from #players-list'
			);

		listedPlayers.forEach((playerItem) => {
			chai.expect(playerItem.id.toLowerCase()).to.not.equal('player-2');
			const nameDiv = playerItem.querySelector('.player-name');
			chai.expect(nameDiv).to.not.be.null;
			chai
				.expect(nameDiv.textContent.trim())
				.to.not.equal(
					playersData[1].name,
					'Wrong player was deleted from the list'
				);
		});
	});
});
