function colId(col) {
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZ".charAt(col);
}

function rowId(row) {
    return "" + (row + 1);
}

function pixelId(col, row) {
    return colId(col) + rowId(row);
}

function rowCol(pid) {
    var col = pid.charCodeAt(0) - "A".charCodeAt(0);
    var row = pid.charCodeAt(1) - "1".charCodeAt(0);
    return [col, row];
}


var nextPictureId = 1;

function new_picture(temporary) {
    var p = new Picture();
    if (temporary) {
        p.id = "temp";
    } else {
        p.id = 'Bild' + nextPictureId;
        nextPictureId++;
    }
    return p;
}

function Picture() {
    this.id = 'Bild';
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

Picture.prototype._copy = function (picture) {
    this._clear();
    for (var pid in picture.colors) {
        this.colors[pid] = picture.colors[pid];
    }
    for (var len = picture.order.length, i = 0; i < len; ++i) {
        this.order.push(picture.order[i]);
    }
}

Picture.prototype.duplicate = function () {
    var newPic = new_picture(false);
    newPic._copy(this);
    return newPic;
}

Picture.prototype.copyFrom = function (picture) {
    this._copy(picture)
    this.notify('ALL');
}

Picture.prototype._rotate_help = function (newOrder) {
    this._clear();

    for (var len = newOrder.length, i = 0; i < len; ++i) {
        var pid = newOrder[i][0];
        this.order.push(pid);
        this.colors[pid] = newOrder[i][1];
    }

    this.notify('ALL');
}

Picture.prototype.rotateHor = function (steps) {
    var newOrder = []
    for (var len = this.order.length, i = 0; i < len; ++i) {
        var pid = this.order[i];
        var pos = rowCol(pid);
        if (steps < 0) steps += COLUMNS;
        var col = (pos[0] + steps) % COLUMNS;
        newOrder.push([pixelId(col, pos[1]), this.colors[pid]]);
    }
    this._rotate_help(newOrder);
}

Picture.prototype.rotateVer = function (steps) {
    var newOrder = []
    for (var len = this.order.length, i = 0; i < len; ++i) {
        var pid = this.order[i];
        var pos = rowCol(pid);
        if (steps < 0) steps += ROWS;
        var row = (pos[1] + steps) % ROWS;
        newOrder.push([pixelId(pos[0], row), this.colors[pid]]);
    }
    this._rotate_help(newOrder);
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