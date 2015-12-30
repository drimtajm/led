"use strict"
var pictures = [
    new Picture(),
    new Picture()
]
var currentPicture = pictures[0];

function submit_code() {
    var code = $('#jqxTextArea').jqxTextArea('val');
    $.post("code.yaws", $('#form').serialize(), function (data) {
            //alert(data)
        },
        "html");
}

function shutdown_matrix() {
    $.post("code.yaws", "close", null,
        //function( data ) {
        //alert(data)
        //},
        "html");
}

function createProgram(picture) {
    var pgm = "Bild1 = [";
    var addComma = false;
    var colors = picture.colors;
    for (var id in colors) {
        if (colors[id] != 0) {
            if (addComma) pgm += ", ";
            if (MULTICOLOR) {
                pgm += "{\"" + id + "\"," + colors[id] + "}";
            } else {
                pgm += "\"" + id + "\"";
            }
            addComma = true;
        }
    }
    pgm += "], Visa(Bild1).";
    return pgm
}

function ProgramUpdater() {
    this.refresh = function(picture) {
        $('#jqxTextArea').jqxTextArea('val', createProgram(picture));
    }
    
    this.pictureChange = function(picture, what, who, value) {
        $('#jqxTextArea').jqxTextArea('val', createProgram(picture));
    }
}

function clear_all(renderer) {
    currentPicture.clear();
}

function changeColor(elementId) {
    var col = (currentPicture.colors[elementId] + 1) % COLORS.length;
    currentPicture.setPixel(elementId, col);
}

function setColor(elementId, color) {
    currentPicture.setPixel(elementId, color);
}
