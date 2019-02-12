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

    const count = 1000;
    const start = new Date();
    /*for(let i = 0; i < count; i++) {
        logger.trace("benchmark");
    }*/
    const finish = new Date();
    logger.info("put items on the queue at a rate of ", count * 1000/(finish.getTime() - start.getTime()), " per sec");

    logger.info("app finished");
})();
