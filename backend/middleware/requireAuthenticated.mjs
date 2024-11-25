import debug from "debug";
import { loginUser } from "../model/user.mjs";

// Create logger for debugging
// (Better console.log with colours and does not show any output in production)
const log = debug("backend:auth-middleware");

const getCredentials = (authorizationHeader) => {
  log("Getting credentials");
  const base64credentials = authorizationHeader.split(" ")[1];
  const credentials = Buffer.from(base64credentials, "base64").toString("utf8");
  return credentials.split(":");
};

const requireAuthenticated = async (req, res, next) => {
  log("Checking authentication");
  const config = req.app.get("config");

  if (config?.enableAuthentication) {
    const authorization = req.get("authorization");
    if (!authorization || !authorization.toLowerCase().startsWith("basic ")) {
      log("Missing or invalid Authorization header");
      return res.sendStatus(401);
    }

    const [username, password] = getCredentials(authorization);

    try {
      const user = await loginUser({ username, password });
      req.user = user;
    } catch {
      log("Authentication failed: check username and password");
      return res.sendStatus(403);
    }
  }

  next();
};

export default requireAuthenticated;
