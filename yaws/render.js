function ImageUpdater(renderer, circleElements) {
    this.renderer = renderer;
    this.elements = circleElements;
}

ImageUpdater.prototype.refresh = function(picture) {
    for (var id in this.elements) {
        var col = picture.colors[id];
        var element = this.elements[id]
        this.renderer.attr(element, {
            fill: COLORS[col]
        });
    }
}

ImageUpdater.prototype.pictureChange = function(picture, what, who, value) {
    switch(what) {
        case 'ALL':
            this.refresh(picture);            
            break;
        case 'PIXEL':
            this.renderer.attr(this.elements[who], {
                fill: COLORS[value]
            });
            break;
    }
}

var freeline = false;
var freeline_color = 1

function getUpdater(elementId) {
    return function () {
        changeColor(elementId);
    }
}

function getColorSetter(elementId) {
    return function () {
        if (freeline) {
            $('#debug').text('mousemove');
            setColor(elementId, freeline_color);
        }
    }
}

function createMatrix(renderer, showLabels) {
    var size = renderer.getSize();
    var margin = 0;
    var x_cells = COLUMNS;
    var y_cells = ROWS;
    if (showLabels) {
        x_cells++;
        y_cells++;
    }
    var y_size = size.width / x_cells;
    var x_size = size.height / y_cells;
    var cell_size = Math.min(x_size, y_size);
    var circle_radius = cell_size * 0.45;
    var f_size = cell_size * 0.8;

    if (showLabels) {
        margin = cell_size;
    }

    var circleElements = {};
    for (var x = 0; x < COLUMNS; x++) {
        for (var y = 0; y < ROWS; y++) {
            var elementId = colId(x) + rowId(y);
            var xpos = margin + cell_size / 2 + x * cell_size;
            var ypos = margin + cell_size / 2 + y * cell_size;
            // params: cx, cy, circle_radius, params
            var circleElement = renderer.circle(xpos, ypos, circle_radius, {
                fill: COLORS[0],
                stroke: 'darkblue'
            });
            circleElements[elementId] = circleElement;
        }
    }
    if (showLabels) {
        for (var y = 0; y < ROWS; y++) {
            // params: text, x, y, width, height, angle, params, clip, halign, valign, rotateAround
            var ypos = margin + y * cell_size;
            renderer.text(rowId(y), 0, ypos, margin, cell_size, 0, {
                'class': 'rowColumnText',
                'font-size': f_size
            }, false, 'center', 'center', 'centermiddle');
        }
        for (var x = 0; x < COLUMNS; x++) {
            var xpos = margin + x * cell_size;
            renderer.text(colId(x), xpos, 0, cell_size, margin, 0, {
                'class': 'rowColumnText',
                'font-size': f_size
            }, false, 'center', 'center', 'centermiddle');
        }
    }
    renderer.refresh();
    return circleElements;
}

function addMouseHandlers(renderer, circleElements) {
    for(var elementId in circleElements) {
        var circleElement = circleElements[elementId];
        renderer.on(circleElement, 'mousedown', getUpdater(elementId));
        renderer.on(circleElement, 'mousedown', function (event) {
            $('#debug').text('mousedown');
            freeline = true;
        });
        renderer.on(circleElement, 'mousemove', getColorSetter(elementId));
    }
}