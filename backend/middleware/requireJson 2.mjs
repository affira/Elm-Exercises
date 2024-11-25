import debug from "debug";

// Create logger for debugging
// (Better console.log with colours and does not show any output in production)
const log = debug("backend:json-middleware");

const requireJson = (req, res, next) => {
  log("Checking JSON content type");
  if (!req.accepts("json")) {
    log("Accept header is missing or client does not accept JSON");
    return res.sendStatus(406);
  }

  if (["POST", "PUT", "PATCH"].includes(req.method.toUpperCase())) {
    if (!req.is("json")) {
      log("Missing Content-Type header or content type is not JSON");
      return res.sendStatus(415);
    }
  }

  next();
};

export default requireJson;
