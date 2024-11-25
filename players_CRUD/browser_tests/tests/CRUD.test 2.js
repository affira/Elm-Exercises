/** @format */

describe('Players CRUD', function () {
	const players = ['Player One', 'Player Two', 'Player Three'];

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

	beforeEach(function () {
		initElm();
	});

	afterEach(function () {
		const wrapper = document.querySelector('#wrapper');
		wrapper.innerHTML = '<div id="app"></div>';
	});

	it('should start with empty player listing', function () {
		const playersList = document.querySelector('#players-list');
		chai.expect(playersList).to.not.be.null;
		chai
			.expect(playersList.querySelectorAll('li').length)
			.to.equal(0, 'Players listing is not empty');
	});

	it('should add new player to the end of #players-list with status "Not active"', async function () {
		const playerInput = document.querySelector('#input-player');
		const addButton = document.querySelector('#btn-add');

		chai.expect(playerInput).to.not.be.null;
		chai.expect(addButton).to.not.be.null;

		// Add players to the list
		for (const name of players) {
			playerInput.value = name;
			const inputEvt = new Event('input', { bubbles: true, cancelable: true });
			playerInput.dispatchEvent(inputEvt);

			// allow inputEvt to be dispatched before clicking button
			await delay(10, () => addButton.click());
		}

		// wait for the DOM to update after last player addition
		await delay(100);
		const listedPlayers = document.querySelectorAll('#players-list li');

		chai
			.expect(listedPlayers.length)
			.to.equal(
				players.length,
				`Added ${players.length} players but #players-list has ${listedPlayers.length} players`
			);

		listedPlayers.forEach((playerItem, i) => {
			const nameDiv = playerItem.querySelector('.player-name');
			const statusInput = playerItem.querySelector('label .player-status');

			chai.expect(nameDiv).to.not.be.null;
			chai.expect(statusInput).to.not.be.null;

			chai
				.expect(nameDiv.textContent.trim())
				.to.equal(players[i], 'Player name is incorrect.');

			chai.expect(statusInput.type.toLowerCase()).to.equal('checkbox');
			chai.expect(statusInput.checked).to.be.false;
		});
	});

	it('should delete player from #players-list when "Delete" button is clicked', async function () {
		const playerInput = document.querySelector('#input-player');
		const addButton = document.querySelector('#btn-add');

		// Add players to the list
		for (const name of players) {
			playerInput.value = name;
			const inputEvt = new Event('input', { bubbles: true, cancelable: true });
			playerInput.dispatchEvent(inputEvt);

			// allow inputEvt to be dispatched before clicking button
			await delay(10, () => addButton.click());
		}

		// wait for the DOM to update after last player addition
		await delay(100);

		const deleteButton = document.querySelector(
			'#players-list li:nth-child(2) .btn-delete'
		);
		deleteButton.click();
		await delay(100);

		const listedPlayers = document.querySelectorAll('#players-list li');

		chai
			.expect(listedPlayers.length)
			.to.equal(
				players.length - 1,
				'Clicking "Delete" button should delete player from #players-list'
			);

		listedPlayers.forEach((playerItem) => {
			const nameDiv = playerItem.querySelector('.player-name');
			chai.expect(nameDiv).to.not.be.null;
			chai
				.expect(nameDiv.textContent.trim())
				.to.not.equal(players[1], 'Wrong player was deleted from the list');
		});
	});
});
