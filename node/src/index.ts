import {AsyncHook} from "async_hooks";
import uuid = require("uuid");
import * as util from "util";
import {Pool} from "pg";
import {wrapCallSite} from "source-map-support";

const asyncHooks = require('async_hooks');
type EventId = string & { __brand: EventId };

export type PostgresConfig = {
    host: string,
    port: number,
    username: string,
    password: string,
    database: string,

    poolSize: number,
    idleTimeoutMillis: number,
    connectionTimeoutMillis: number,
    connectionName: string
};

export type RawMessage = {
    tag: "RawMessage",
    message: string,
    level: string,
    eventId: EventId | undefined,
    timestamp: Date,
    data: any,
    filename: string,
    line: number,
    col: number
}

export type RawEventStart = {
    tag: "RawEventStart",
    eventId: EventId,
    timestampStart: Date,
    parent: EventId | undefined,
    eventType: string,
    data: any
}

export type RawEventEnd = {
    tag: "RawEventEnd",
    eventId: EventId,
    timestampEnd: Date,
    error: any,
    result: any
}

//============================================================

function sleep(deplay: number): Promise<void> {
    return new Promise(resolve => {
        setTimeout(resolve, deplay);
    });
}

//============================================================

export class Logger {
    private contexts = new Map<number, EventId>();
    private asyncHook: AsyncHook;
    private pgConfig: PostgresConfig;
    private pgPool: Pool = undefined as unknown as Pool;
    private msgQueue: (RawMessage | RawEventStart | RawEventEnd)[] = [];

    constructor(pgConfig: PostgresConfig) {
        this.pgConfig = pgConfig;
        this.asyncHook = asyncHooks.createHook({
            init: this.initAsync.bind(this),
            after: this.afterAsync.bind(this)
        });
        this.asyncHook.enable();
    }

    async init() {
        let i = 0;
        while (true) {
            try {
                this.pgPool = new Pool({
                    ...this.pgConfig,
                    user: this.pgConfig.username,
                    max: this.pgConfig.poolSize,
                    application_name: this.pgConfig.connectionName,
                    idleTimeoutMillis: this.pgConfig.idleTimeoutMillis,
                    connectionTimeoutMillis: this.pgConfig.connectionTimeoutMillis
                });
                return;
            } catch (e) {
                if (i < 10) {
                    i++;
                    await sleep(2000);
                } else {
                    process.exit(1);
                }
            }
        }
    }

    newEventId(): EventId {
        const contextId = uuid.v4().toString() as EventId;
        const eid = asyncHooks.executionAsyncId();
        this.contexts.set(eid, contextId);
        return contextId;
    }

    getEventId(): EventId | undefined {
        const eid = asyncHooks.executionAsyncId();
        return this.contexts.get(eid);
    }

    private setEventId(eventId: EventId | undefined) {
        const eid = asyncHooks.executionAsyncId();
        if(eventId) {
            return this.contexts.set(eid, eventId);
        } else {
            this.contexts.delete(eid);
        }
    }

    info(item: any, ...logItems: any[]) {
        const message = util.format(item, ...logItems);
        this.rawLog(message, "INFO", undefined);
    }

    trace(item: any, ...logItems: any[]) {
        const message = util.format(item, ...logItems);
        this.rawLog(message, "TRACE", undefined);
    }

    withEvent<T>(eventType: string, wrapped: () => {returnValue: T, result?: any}): T {
        const oldId = this.getEventId();
        const newEventId = this.newEventId();
        try {
            const rawEventStart: RawEventStart = {
                tag: "RawEventStart",
                eventId: newEventId,
                parent: oldId,
                timestampStart: new Date(),
                eventType,
                data: null
            };
            this.msgQueue.push(rawEventStart);

            const result = wrapped();
            this.setEventId(oldId);
            const rawEventEnd: RawEventEnd = {
                tag: "RawEventEnd",
                eventId: newEventId,
                timestampEnd: new Date(),
                error: null,
                result: result.result
            };
            this.msgQueue.push(rawEventEnd);
            return result.returnValue;
        } catch (e) {
            this.setEventId(oldId);
            const rawEventEnd: RawEventEnd = {
                tag: "RawEventEnd",
                eventId: newEventId,
                timestampEnd: new Date(),
                error: e,
                result: null
            };

            this.msgQueue.push(rawEventEnd);
            throw e;
        }
    }

    rawLog(message: string, level: string, data: any) {

        let holder: { stack: NodeJS.CallSite[] } = {} as any;
        const old = Error.prepareStackTrace;
        Error.prepareStackTrace = (_, s) => s;
        Error.captureStackTrace(holder);
        const stack = holder.stack.map(wrapCallSite);
        Error.prepareStackTrace = old;
        const prunedStack = stack.slice(2);

        const rawMessage: RawMessage = {
            tag: "RawMessage",
            eventId: this.getEventId(),
            message,
            level,
            timestamp: new Date(),
            filename: prunedStack[0].getFileName(),
            line: prunedStack[0].getLineNumber(),
            col: prunedStack[0].getColumnNumber(),
            data
        };

        this.msgQueue.push(rawMessage);
        this.doInserts().catch(console.error);
    }

    private async doInserts() {
        const queue = this.msgQueue;
        this.msgQueue = [];

        for(const msg of queue) {
            if(msg.tag === "RawMessage") {
                await this.insertMessage(msg);
            } else if(msg.tag === "RawEventStart") {
                await this.insertEvent(msg);
            } else if(msg.tag === "RawEventEnd") {
                await this.endEvent(msg);
            } else {
                throw new Error("Not implemented");
            }
        }
    }

    private async insertMessage(msg: RawMessage) {
        const query = "INSERT INTO message(message, level, event_id, timestamp, data, filename, line, col) VALUES($1, $2, $3, $4, $5, $6, $7, $8)";
        await this.pgPool.query(query, [
            msg.message,
            msg.level,
            msg.eventId,
            msg.timestamp,
            msg.data,
            msg.filename,
            msg.line,
            msg.col
        ]);
    }

    private async endEvent(eventEnd: RawEventEnd) {
        const query = "UPDATE event SET timestamp_end=$1, error=$2, result=$3 WHERE event_id=$4";
        await this.pgPool.query(query, [
            eventEnd.timestampEnd,
            eventEnd.error,
            eventEnd.result,
            eventEnd.eventId
        ])
    }

    private async insertEvent(event: RawEventStart) {
        const query = "INSERT INTO event(event_id, timestamp_start, parent, event_type, data) VALUES($1, $2, $3, $4, $5)";
        await this.pgPool.query(query, [
            event.eventId,
            event.timestampStart,
            event.parent,
            event.eventType,
            event.data
        ])
    }

    private initAsync(asyncId: number, type: string, triggerAsyncId: number) {
        const parentContext = this.contexts.get(triggerAsyncId);
        if (parentContext) {
            this.contexts.set(asyncId, parentContext);
        }
    }

    private afterAsync(asyncId: number) {
        this.contexts.delete(asyncId);
    }

}

//============================================================


