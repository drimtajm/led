"use strict"
var pictures = [
    new Picture(),
    new Picture()
]
var currentPicture = pictures[0];
var main_up;

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

function createPictureProgram(picture) {
    var pgm = picture.id + " = [";
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
    pgm += "].";
    return pgm
}

function createProgram() {
    var pgm = "";
    for (var index = 0; index < pictures.length; index++) {
        pgm += createPictureProgram(pictures[index]) + "\n";
    }
    var addComma = false;
    for (var index = 0; index < pictures.length; index++) {
        if (addComma) pgm += ", ";
        addComma = true;
        pgm += "Visa(" + pictures[index].id + ")"
    }
    pgm += ".\n"
    return pgm;
}

function ProgramUpdater() {
    this.refresh = function(picture) {
        $('#jqxTextArea').jqxTextArea('val', createProgram());
    }
    
    this.pictureChange = function(picture, what, who, value) {
        $('#jqxTextArea').jqxTextArea('val', createProgram());
    }
}

function initMain()
{
    $('#container').jqxDraw();
    var renderer = $('#container').jqxDraw('getInstance');
    var circleElements = createMatrix(renderer, true, null);
    addMouseHandlers(renderer, circleElements);
    main_up = new ImageUpdater(renderer, circleElements);
    main_up.refresh(currentPicture);
    currentPicture.observe(main_up);
    $('#container').on('mouseup', function (event) {
        $('#debug').text('mouseup')
        freeline = false;
    });
}

function updateMiniSelection() {
    for (var index = 0; index < pictures.length; index++) {
        var mid = '#mini_' + pictures[index].id;
        var el = $(mid)
        var color = 'white';
        if (pictures[index] == currentPicture) {
            color = 'green';
        }
        el.css('background-color', color);
    }
}

function createNewMini(pic) {
    var mid = 'mini_' + pic.id;
    $('#minitures').append('<div id="' + mid + '"></div>');
    var minielem = $('#' + mid);
    minielem.css({'width' : '85px',
                      'height': '80px',
                      'display' : 'inline-block'
                     });
    minielem.jqxDraw();
    minielem.data('pic', pic);
    var renderer2 = minielem.jqxDraw('getInstance');
    var elems2 = createMatrix(renderer2, false, pic.id);
    var up = new ImageUpdater(renderer2, elems2);
    up.refresh(pic);
    pic.observe(up);
    minielem.on('click', function () {
        currentPicture.unobserve(main_up);
        currentPicture = $(this).data('pic');
        currentPicture.observe(main_up);
        main_up.refresh(currentPicture);
        updateMiniSelection();
    })
}

function clear_all() {
    currentPicture.clear();
}

function clone_current_picture() {
    var npic = currentPicture.duplicate();
    pictures.push(npic);
    createNewMini(npic);
    npic.observe(progObserer);
    progObserer.refresh();
    currentPicture.unobserve(main_up);
    currentPicture = npic;
    currentPicture.observe(main_up);
    main_up.refresh(currentPicture);
    updateMiniSelection();
}

function delete_current_picture() {
    if (pictures.length == 1) return;
    
    var cidx = pictures.indexOf(currentPicture);
    pictures.splice(cidx, 1);
    currentPicture.unobserve(main_up);
    $('#mini_' + currentPicture.id).remove();

    var nextCidx = Math.min(cidx, pictures.length - 1);
    currentPicture = pictures[nextCidx];
    currentPicture.observe(main_up);
    main_up.refresh(currentPicture);
    updateMiniSelection();
    progObserer.refresh();
}

function changeColor(elementId) {
    var col = (currentPicture.colors[elementId] + 1) % COLORS.length;
    currentPicture.setPixel(elementId, col);
}

function setColor(elementId, color) {
    currentPicture.setPixel(elementId, color);
}
