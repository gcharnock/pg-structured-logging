import {Logger} from "./index";

const logger = new Logger({
    host: "docker",
    port: 30000,
    username: "postgres",
    password: "dev",
    database: "postgres",

    poolSize: 5,
    idleTimeoutMillis: 30000,
    connectionTimeoutMillis: 5000,
    connectionName: "example-logger"
});

(async () => {
    await logger.init();

    logger.info("app start");
    logger.withEvent("App", () => {
        logger.info("in app context");
        return {returnValue: undefined}
    });
    logger.info("app finished");
})();
