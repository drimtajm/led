function colId(col) {
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZ".charAt(col);
}

function rowId(row) {
    return "" + (row + 1);
}

function pixelId(col, row) {
    return colId(col) + rowId(row);
}

function Picture() {
    this.colors = {};
    this.listeners = [];
    this._clear();
}

Picture.prototype._clear = function () {
    var x, y, elementId;
    for (x = 0; x < COLUMNS; x++) {
        for (y = 0; y < ROWS; y++) {
            elementId = pixelId(x, y);
            this.colors[elementId] = 0;
        }
    }
}

Picture.prototype.observe = function (observer) {
    this.listeners.push(observer);
}

Picture.prototype.unobserve = function (observer) {
    for (var i = 0, len = this.listeners.length; i < len; i++) {
        if (this.listeners[i] === observer) {
            this.listeners.splice(i, 1);
            return true;
        }
    }
    return false;
}

Picture.prototype.notify = function (what, who, value) {
    for (var i = 0; i < this.listeners.length; i++) {
        this.listeners[i].pictureChange(this, what, who, value);
    }
}

Picture.prototype.setPixel = function (pid, value) {
    if (this.colors[pid] != value) {
        this.colors[pid] = value;
        this.notify('PIXEL', pid, value);
    }
}

Picture.prototype.clear = function (pid, value) {
    this._clear();
    this.notify('ALL');
}

function createTestImage(picture) {
    var x, y, elementId;
    for (x = 0; x < COLUMNS; x++) {
        for (y = 0; y < ROWS; y++) {
            elementId = pixelId(x, y);
            picture.setPixel(elementId, (x + y) % (COLORS.length));
        }
    }
}
