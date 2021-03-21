const Mapscii = require("mapscii/src/Mapscii");

exports.drawMapImpl = (options) => () => {
    const headlessOpts = Object.assign(options, { headless: true });
    const m = new Mapscii(headlessOpts);
    return m.init();
};