const asyncHooks = require('async_hooks');
const fs = require("fs");


const asyncHook = asyncHooks.createHook({init, after});
asyncHook.enable();

const contexts = new Map<number, any>();
contexts.set(asyncHooks.executionAsyncId(), "context 1");

function forkContext(context: any, makePromise: () => Promise<any>) {
  fs.writeSync(1, `forkContext called\n`);
  return new Promise((resolve, reject) => {
    const eid = asyncHooks.executionAsyncId();
    fs.writeSync(1, `setting context[${eid}] from ${contexts.get(eid)} to ${context}\n`);
    contexts.set(eid, context);
    makePromise().then(resolve).catch(reject);
  });
}

function getContext() {
  const eid = asyncHooks.executionAsyncId();
  return contexts.get(eid);
}

function init(asyncId: number, type: string, triggerAsyncId: number) {
  fs.writeSync(1, `init ${type}(${asyncId}): trigger: ${triggerAsyncId}\n`);
  contexts.set(asyncId, contexts.get(triggerAsyncId));
}

function after(asyncId: number) {
  fs.writeSync(1, `after ${asyncId}\n`);
  contexts.delete(asyncId);
}

//============================================================

export class Logger {

}

//============================================================

async function two() {
  fs.writeSync(1, `two() Context=${getContext()}\n`)
}

async function one() {
  fs.writeSync(1, `one()-- before two(). Context=${getContext()}\n`)
  await two();
  fs.writeSync(1, `one()-- after two(). Context=${getContext()}\n`)
}

(async () => {
  fs.writeSync(1, `Running async. Context=${getContext()}\n`);

  setTimeout(() => {
    fs.writeSync(1, `Running timer. Context=${getContext()}\n`);
    forkContext("context 2", async() => {
      one().then(
        ()=> fs.writeSync(1, `one() returned. Context=${getContext()}\n`)
      ).catch();
    });
  }, 1)

  fs.writeSync(1, `Done running async. Context=${getContext()}\n`);
})();