'use strict';

function ImageUpdater(renderer, circleElements, indexElements) {
    this.renderer = renderer;
    this.elements = circleElements;
    this.indexElements = indexElements
    this.showOrder = false;
}

ImageUpdater.prototype.refresh = function (picture) {
    for (var id in this.elements) {
        var col = picture.colors[id];
        var element = this.elements[id]
        this.renderer.attr(element, {
            fill: COLORS[col][0]
        });
    }
    this.refreshIndex(picture);
}

ImageUpdater.prototype.refreshIndex = function (picture) {
    if (this.indexElements == null)
        return;

    for (var id in this.elements) {
        var idx = picture.order.indexOf(id);
        if (this.showOrder && idx >= 0) {
            if (idx >= 9) {
                idx = '' + (idx + 1);

            } else if (idx >= 0) {
                idx = '&nbsp;' + (idx + 1);
            }
        } else {
            idx = '';
        }
        var element = this.indexElements[id];
        if (element != undefined)
            element.innerHTML = idx;
    }
}

ImageUpdater.prototype.pictureChange = function (picture, what, who, value) {
    switch (what) {
    case 'ALL':
        this.refresh(picture);
        break;
    case 'PIXEL':
        this.renderer.attr(this.elements[who], {
            fill: COLORS[value][0]
        });
        break;
    }
    this.refreshIndex(picture);
}

var freeline = false;
var freeline_color = 1


function getColorSetter(elementId) {
    return function () {
        if (freeline) {
            $('#debug').text('mousemove');
            setColor(elementId, freeline_color);
        }
    }
}

function createMatrix(renderer, showLabels, id, center, index_nums) {
    var size = renderer.getSize();
    var margin = 0;
    var x_offset = 0;
    var y_offset = 0;
    var x_cells = COLUMNS;
    var y_cells = ROWS;
    if (showLabels) {
        x_cells++;
        y_cells++;
    }
    if (id != null) {
        y_cells += 2;
    }

    var x_size = size.width / x_cells;
    var y_size = size.height / y_cells;
    var cell_size = Math.min(x_size, y_size);
    var circle_radius = cell_size * 0.45;
    var f_size = cell_size * 0.8;
    if (center) {
        x_offset = (size.width - cell_size * x_cells) / 2;
    }

    y_offset = (size.height - cell_size * y_cells) / 2;

    if (showLabels) {
        margin = cell_size;
    }

    var circleElements = {};
    var indexElements = null;
    if (index_nums)
        indexElements = {};
    for (var x = 0; x < COLUMNS; x++) {
        for (var y = 0; y < ROWS; y++) {
            var elementId = colId(x) + rowId(y);
            var xpos = x_offset + margin + cell_size / 2 + x * cell_size;
            var ypos = y_offset + margin + cell_size / 2 + y * cell_size;
            // params: cx, cy, circle_radius, params
            var circleElement = renderer.circle(xpos, ypos, circle_radius, {
                fill: COLORS[0][0],
                stroke: 'darkblue'
            });
            circleElements[elementId] = circleElement;
            if (index_nums) {
                var cellText = renderer.text(elementId, xpos - cell_size / 2,
                    ypos - cell_size / 2,
                    cell_size, cell_size, 0, {
                        'class': 'rowColumnText',
                        'font-size': cell_size * 0.4
                    }, false, 'center', 'center', 'centermiddle');
                indexElements[elementId] = cellText
            }
        }
    }
    if (showLabels) {
        for (var y = 0; y < ROWS; y++) {
            // params: text, x, y, width, height, angle, params, clip, halign, valign, rotateAround
            var ypos = y_offset + margin + y * cell_size;
            renderer.text(rowId(y), x_offset, ypos, margin, cell_size, 0, {
                'class': 'rowColumnText',
                'font-size': f_size
            }, false, 'center', 'center', 'centermiddle');
        }
        for (var x = 0; x < COLUMNS; x++) {
            var xpos = x_offset + margin + x * cell_size;
            renderer.text(colId(x), xpos, y_offset, cell_size, margin, 0, {
                'class': 'rowColumnText',
                'font-size': f_size
            }, false, 'center', 'center', 'centermiddle');
        }
    }
    if (id != null) {
        var ypos = y_offset + margin + ROWS * cell_size;
        var xpos = x_offset + margin;
        renderer.text(id, xpos, ypos, COLUMNS * cell_size, 2 * cell_size, 0, {
            'class': 'rowColumnText',
            'font-size': 2 * f_size
        }, false, 'center', 'center', 'centermiddle');
    }
    renderer.refresh();
    return new ImageUpdater(renderer, circleElements, indexElements);
}

function addMouseHandlers(renderer, circleElements) {
    for (var elementId in circleElements) {
        var circleElement = circleElements[elementId];
        renderer.on(circleElement, 'mousedown', function (event) {
            $('#debug').text('mousedown');
            freeline = true;
        });
        renderer.on(circleElement, 'mousemove', getColorSetter(elementId));
        renderer.on(circleElement, 'mousedown', getColorSetter(elementId));
    }
}