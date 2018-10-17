async function delay(delayInms) {
    return new Promise(resolve  => {
        setTimeout(() => {
            resolve(2);
        }, delayInms);
    });
}
async function sample(d) {
    console.log('a');
    console.log('waiting...')
    let delayres = await delay(d);
    console.log('b');
}
