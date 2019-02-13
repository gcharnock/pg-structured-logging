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
    const count = 1000;

    logger.info("app start");
    logger.error("Error log");
    logger.warning("Warning log");
    logger.info("info log");
    logger.trace("trace log");

    const start = logger.withEvent("App", () => {
        logger.info("in app context");


        try {
            logger.withEvent("Failing", () => {
                throw new Error("Some error message");
            });
        } catch(e) {
            logger.error("crashed with error:", e);
        }


        const start = new Date();
        for(let i = 0; i < count; i++) {
            logger.trace("benchmark");
        }
        const finish = new Date();
        logger.info("put items on the queue at a rate of ", count * 1000/(finish.getTime() - start.getTime()), " per sec");

        logger.info("app finished");

        return {returnValue: start}
    });
    await logger.flush();
    const finish = new Date();
    logger.info("flushed all items to the database. Rate: ", (count + 2) * 1000/(finish.getTime() - start.getTime()), " per sec");
    await logger.flush();


})();
