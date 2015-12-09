var LEFT_MARGIN = 40;
var TOP_MARGIN = 40;

var circleColors = {};
var circleElements = {};

function submit_code() {
    var code = $('#jqxTextArea').jqxTextArea('val')
    $.post("code.yaws", $('#form').serialize(), function( data ) {
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

function initCircles() {
    for (var x = 0; x < COLUMNS; x++) {
        for (var y = 0; y < ROWS; y++) {
            var elementId = colId(x)+ rowId(y);
            circleColors[elementId] = (x + y) % (COLORS.length);
        }
    }
}

function createProgram(picture) {
    var pgm = "Bild1 = ["
    var addComma = false
    for (var id in circleColors) {
        if (circleColors[id] != 0) {
            if (addComma) pgm += ", ";
                if (MULTICOLOR) {
                    pgm += "{\"" + id + "\"," + circleColors[id] + "}";
                }
                else {
                    pgm += "\"" + id + "\"";
                }
            addComma = true;
        }
    }
    pgm += "], Visa(Bild1)."
    return pgm
}

function update_program()
{
    $('#jqxTextArea').jqxTextArea('val', createProgram(circleColors));
}

function updateAllCircles(renderer) {
    for (var id in circleElements) {
        var col = circleColors[id];
        var element = circleElements[id]
        renderer.attr(element, { fill: COLORS[col] });
    }
}

function clear_all(renderer) {
    for (var id in circleElements) {
        circleColors[id] = 0;
    }
    updateAllCircles(renderer)
    update_program()
}

function changeColor(renderer, elementId) {
    var col = (circleColors[elementId] + 1) % COLORS.length;
    circleColors[elementId] = col;
    var element = circleElements[elementId]
    renderer.attr(element, { fill: COLORS[col] });
    update_program();
}

function setColor(renderer, elementId, color) {
    circleColors[elementId] = color;
    var element = circleElements[elementId]
    renderer.attr(element, { fill: COLORS[color] });
    update_program()
}

function colId(col) {
    return "ABCDEFGHIJKLMNOPQRSTUVWXYZ".charAt(col);
}

function rowId(row) {
    return "" + (row + 1);
}
