/** @format */

describe('Fetch players', function () {
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

	const sendResponse = (fail = false) => {
		if (fail) {
			server.respondWith('GET', /\/api\/players\/$/, (xhr) => {
				xhr.respond(500);
			});

			server.respond();
			return;
		}

		// Fake GET all players
		server.respondWith('GET', /\/api\/players\/$/, (xhr) => {
			xhr.respond(
				200,
				{ 'Content-Type': 'application/json' },
				JSON.stringify(playersData)
			);
		});

		server.respond();
	};

	beforeEach(function () {
		// Fake XMLHttpRequest
		server = sinon.fakeServer.create({
			autoRespond: false,
			autoResponseAfter: 10,
			respondImmediately: false,
			fakeHTTPMethods: false,
		});
	});

	afterEach(function () {
		// Restore original XMLHttpRequest
		server.restore();
		const wrapper = document.querySelector('#wrapper');
		wrapper.innerHTML = '<div id="app"></div>';
	});

	it('should fetch player data from the backend with a GET request', async function () {
		const app = initElm();

		// Before response is received
		chai
			.expect(server.requests.length)
			.to.equal(1, 'Did not receive a request to the backend API');
		chai
			.expect(server.requests[0].url)
			.to.match(/\/api\/players\/$/, 'Incorrect API address');

		const requestStatus = document.querySelector('#request-status');
		chai.expect(requestStatus).to.not.be.null;
		chai
			.expect(requestStatus.textContent.trim().toLowerCase())
			.to.equal(
				'loading...',
				'Incorrect request status: should show "Loading..." while waiting for data'
			);

		sendResponse();

		// after response
		chai.expect(server.requests[0].status).to.equal(200);
		await delay(100);

		chai
			.expect(requestStatus.textContent.trim())
			.to.equal(
				'',
				'Incorrect request status: should be empty after successful request'
			);
	});

	it('should populate #players-list with fetched players', async function () {
		const app = initElm();
		sendResponse();
		await delay(100);

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
	});

	it('should show error status when request fails', async function () {
		const app = initElm();
		sendResponse(true);
		await delay(100);

		const requestStatus = document.querySelector('#request-status');
		chai.expect(requestStatus).to.not.be.null;
		chai
			.expect(requestStatus.textContent.trim().toLowerCase())
			.to.equal(
				'an error has occurred!!!',
				'Incorrect request status: should show "An error has occurred!!!" after failed request'
			);
	});
});
