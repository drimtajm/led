var freeline = false;
var freeline_color = 1

function getUpdater(renderer, elementId) {
    return function() {
        changeColor(renderer, elementId);
    }
}

function getColorSetter(renderer, elementId) {
    return function() {
        if (freeline) {
            $('#debug').text('mousemove');
            setColor(renderer, elementId, freeline_color);
        }
    }
}

function renderMatrix(renderer, matrixColors, showLabels) {
    var size = renderer.getSize();
    var margin = 0;
    var cells = 8;
    if (showLabels) {
        cells = 9;
    }
    var y_size = size.width / cells;
    var x_size = size.height / cells;
    var cell_size = Math.min(x_size, y_size);
    var circle_radius = cell_size * 0.45;
    var f_size = cell_size * 0.8;
    
    if (showLabels) {
        margin = cell_size;
    }
    
    var circleElements = {};
    for (var x = 0; x < COLUMNS; x++) {
        for (var y = 0; y < ROWS; y++) {
            var elementId = colId(x)+ rowId(y);
            var xpos = margin + cell_size/2 + x * cell_size;
            var ypos = margin + cell_size/2 + y * cell_size;
            // params: cx, cy, circle_radius, params
            var circleElement = renderer.circle(xpos, ypos, circle_radius, 
                                                {fill: COLORS[matrixColors[elementId]], 
                                                 stroke: 'darkblue' });
            circleElements[elementId] = circleElement;
            renderer.on(circleElement, 'click', getUpdater(renderer, elementId));
            renderer.on(circleElement, 'mousedown', function (event) {
                $('#debug').text('mousedown');
                freeline = true;
            });
            renderer.on(circleElement, 'mousemove', getColorSetter(renderer, elementId));
        }
    }
    if (showLabels) {
        for (var y = 0; y < ROWS; y++) {
            // params: text, x, y, width, height, angle, params, clip, halign, valign, rotateAround
            var ypos = margin + y * cell_size;
            renderer.text(rowId(y), 0, ypos, margin, cell_size, 0, 
                          {'class':'rowColumnText', 'font-size':f_size}, false, 'center', 'center', 'centermiddle');
        }
        for (var x = 0; x < COLUMNS; x++) {
            var xpos = margin + x * cell_size;
            renderer.text(colId(x), xpos, 0, cell_size, margin, 0, 
                          {'class':'rowColumnText', 'font-size':f_size}, false, 'center', 'center', 'centermiddle');
        }
    }
    renderer.refresh();
    return circleElements;
}