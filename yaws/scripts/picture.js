function colId(col) {
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZ".charAt(col);
}

function rowId(row) {
    return "" + (row + 1);
}

function pixelId(col, row) {
    return colId(col) + rowId(row);
}

var nextPictureId = 1;
function Picture() {
    this.id = 'Bild' + nextPictureId;
    nextPictureId ++;
    this.colors = {};
    this.order = [];
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
    this.order = [];
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
        for (var i = 0, len = this.order.length; i < len; i++) {
            if (this.order[i] === pid) {
                this.order.splice(i, 1);
                break;
            }
        }
        if (value != 0)
            this.order.push(pid);
        this.notify('PIXEL', pid, value);
    }
}

Picture.prototype.clear = function (pid, value) {
    this._clear();
    this.notify('ALL');
}

Picture.prototype.duplicate = function () {
    var newPic = new Picture();
    for (var pid in this.colors) {
        newPic.colors[pid] = this.colors[pid];
    }
    for (var len = this.order.length, i=0; i<len; ++i) {
        newPic.order.push(this.order[i]);
    }

    return newPic;
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
