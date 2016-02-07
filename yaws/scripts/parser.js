"use strict";

function Tokenizer(input) {
    this.input = input;
    this.idx = 0;
    this.nextValue = null;
    this.nextKind = null;
    this.advance();
}

var regexps = [
    [/^(\s+)/g, 'WS'],
    [/^([A-Za-z][A-Za-z0-9_]+)/g, 'ID'],
    [/^"([^"]*)"/g, 'STR'],
    [/^(\S)/g, 'LIT']
    ];

Tokenizer.prototype.advance = function () {
    var restart = true;
    while (restart) {
        restart = false;
        for (var i = 0, l = regexps.length; i < l; i++) {
            var re = regexps[i][0];
            re.lastIndex = 0;
            var kind = regexps[i][1];
            var m = re.exec(this.input);
            if (m) {
                this.idx += re.lastIndex;
                this.input = this.input.substring(re.lastIndex);
                if (kind == 'WS') {
                    restart = true;
                    break;
                }
                //console.info("'" + m[0] + "' " + kind + ": rem: " + this.input);
                if (kind == 'LIT') kind = m[1];
                this.nextKind = kind;
                this.nextValue = m[1];
                return;
            }
        }
    }
    this.nextKind = 'EOF';
    this.nextValue = "";
    return;
}

function colorIndex(colname) {
    for (var i = 1; i < COLORS.length; i++) {
        if (colname.toUpperCase() == COLORS[i][1].toUpperCase()) {
            return i;
        }
    }
    return -1;
}

function parseImageDef(tok) {
    var pic = new_picture(true);
    if (tok.nextKind != 'ID') return null;
    var picId = tok.nextValue;
    pic.id = picId;
    tok.advance();
    if (tok.nextKind != '=') return null;
    tok.advance();
    if (tok.nextKind != '[') return null;
    tok.advance();
    while (tok.nextKind != ']') {
        switch (tok.nextKind) {
        case '{':
            tok.advance();
            if (tok.nextKind != 'STR') return null;
            var cell = tok.nextValue.toUpperCase();
            if (currentPicture.colors[cell] == undefined) return null;
            tok.advance();
            if (tok.nextKind != ',') return null;
            tok.advance();
            if (tok.nextKind != 'STR') return null;
            var color = tok.nextValue;
            color = colorIndex(color);
            if (color < 0) return null;
            tok.advance();
            if (tok.nextKind != '}') return null;
            tok.advance();
            console.info(cell + "->" + color);
            pic.setPixel(cell, 1);
            if (tok.nextKind != ',' && tok.nextKind != ']') return null;
            break;
        case ',':
            tok.advance();
            break;
        case 'STR':
            var cell = tok.nextValue.toUpperCase();
            if (currentPicture.colors[cell] == undefined) return null;
            console.info(cell + "->" + "1");
            pic.setPixel(cell, 1);
            tok.advance();
            if (tok.nextKind != ',' && tok.nextKind != ']') return null;
            break;
        default:
            return null;
        }
    }
    return pic;
}