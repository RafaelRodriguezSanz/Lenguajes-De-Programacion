const {StatusEnum} = require("./settledEnum");

class SettledResult {
    constructor(status, value, reason) {
        if (!Object.values(StatusEnum).includes(status)) {
            throw new Error(`El valor de "status" debe ser uno de: ${Object.values(StatusEnum).join(', ')}`);
        }
        this.status = status;
        this.value = value;
        this.reason = reason===null ? null : `¡Error: ${reason.message}!`;
    }
}

module.exports = {SettledResult}


