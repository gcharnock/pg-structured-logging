const asyncHooks = require('async_hooks');
const fs = require("fs");


const asyncHook = asyncHooks.createHook({init, before, after, destory, promiseResolve});
asyncHook.enable();

const contexts = new Map();
contexts.set(asyncHooks.executionAsyncId(), "context 1");

function forkContext(context, makePromise) {
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

function init(asyncId, type, triggerAsyncId, resource) {
  fs.writeSync(1, `init ${type}(${asyncId}): trigger: ${triggerAsyncId}\n`);
  contexts.set(asyncId, contexts.get(triggerAsyncId));
}

function before(asyncId) {
  fs.writeSync(1, `before ${asyncId}\n`);
}

function after(asyncId) {
  fs.writeSync(1, `after ${asyncId}\n`);
  contexts.delete(asyncId);
}

function destory(asyncId) {
  fs.writeSync(1, `destory ${asyncId}\n`);
}

function promiseResolve(asyncId) {
  fs.writeSync(1, `promiseResolve ${asyncId}\n`)
}

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