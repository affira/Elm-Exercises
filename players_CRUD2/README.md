# Instructions

Here are the setup instructions. Read this file **carefully**. 

## Starting server backend

The backend for all the exercises is located inside the directory `backend` at the root of the repository.

1. Setup/install backend
   - **this stage needs to be performed only once**
   - **you can skip this stage if the backend is already installed**
   - change working directory to `backend`
   - inside `backend` directory run `npm install`

2. Starting the backend server
   - change working directory to `backend`
   - inside `backend` directory run `npm run elm-crud2`
   - server listens on http://localhost:3001/

3. Stopping the backend server
   - press <`Ctrl-C`> while the server is running

### Resetting database back to its initial state

1. change working directory to `backend`
2. inside `backend` directory run `npm run reset-db`

## Setup the exercise

- change working directory to `elm/players_CRUD2`
- run `npm install` to install all dependencies

## Run the exercise

- start the server backend (`npm run elm-crud2`) (inside the `backend`-directory)
- in another terminal run `npm run dev` to compile the app in dev (watch) mode
  (or run `npm run compile` to build the app in unwatch mode)
- open [index.html](./index.html) file in a browser to view the app

## Test

- open [browser_tests/players_CRUD2.test.html](./browser_tests/players_CRUD2.test.html)
  in a browser to view **Browser** test results
