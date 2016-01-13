"use strict"
var pictures = [
    new Picture(),
    new Picture()
]
var currentPicture = pictures[0];
var main_up;
var progObserver;
var picProgObserver;

var pgmArray;
var pgmSource;

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
    var order = picture.order;
    for (var idx in order) {
        var id = order[idx];
        if (colors[id] != 0) {
            if (addComma) pgm += ", ";
            if (MULTICOLOR) {
                pgm += "{\"" + id + "\", \"" + COLORS[colors[id]][1] + "\"}";
            } else {
                pgm += "\"" + id + "\"";
            }
            addComma = true;
        }
    }
    pgm += "]";
    return pgm
}

function createProgramSteps() {
    var pgm = "";
    var addComma = false;
    for (var index = 0; index < pgmSource.length; index++) {
        var step = pgmSource[index];
        if (addComma) pgm += ", ";
        addComma = true;
        pgm += step.action + "(" + step.pictureId + ", " + step.time + ")";
    }
    return pgm;
}

function createProgram() {
    var pgm = "";
    var addComma = false;
    for (var index = 0; index < pictures.length; index++) {
        if (addComma) pgm += ",\n";
        addComma = true;
        pgm += createPictureProgram(pictures[index]);
    }
    pgm += ",\n"
    pgm += createProgramSteps();
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

function PictureProgramUpdater() {
    this.refresh = function(picture) {
        $('#pictureCode').jqxTextArea('val', createPictureProgram(currentPicture));
    }
    
    this.pictureChange = function(picture, what, who, value) {
        $('#pictureCode').jqxTextArea('val', createPictureProgram(currentPicture));
    }
}

function initAll()
{
    createTestImage(currentPicture);
    initMain();
    for (var index = 0; index < pictures.length; index++) {
        createNewMini(pictures[index]);
    }
    updateMiniSelection();
    progObserver = new ProgramUpdater();
    for (var index = 0; index < pictures.length; index++) {
        pictures[index].observe(progObserver);
    }
    picProgObserver = new PictureProgramUpdater();
    currentPicture.observe(picProgObserver);
    pgmArray = Array();
    pgmSource = new $.jqx.observableArray(pgmArray, function (changed) {
        $('#programCode').jqxTextArea('val', createProgramSteps());
        progObserver.refresh(currentPicture);
    });
    
    for (var index = 0; index < pictures.length; index++) {
        var step = new ProgramStep();
        step.pictureId = pictures[index].id;
        pgmSource.push(step);
    }
}

function initMain()
{
    //$("#container").css("width", COLUMNS*50);
    $("#container").css("width", "100%");
    $("#container").css("height", ROWS*50);
    $("#container").jqxDraw();
    var renderer = $("#container").jqxDraw('getInstance');
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
        var color = '';
        if (pictures[index] == currentPicture) {
            color = 'green';
        }
        el.css('background-color', color);
    }
}

function switchCurrentPicture(newPicture)
{
    currentPicture.unobserve(main_up);
    currentPicture.unobserve(picProgObserver);

    currentPicture = newPicture;

    currentPicture.observe(main_up);
    main_up.refresh(currentPicture);
    currentPicture.observe(picProgObserver);
    picProgObserver.refresh(currentPicture);
    updateMiniSelection();
    
}

function progstepMini1(pic) {
    var mid = "prog_mini_" + pic.id;
    var minielem = $('<div id="' + mid + '"></div>');
    return minielem;
}

function progstepMini2(pic) {
    var mid = "prog_mini_" + pic.id;
    var minielem = $('#' + mid);
    minielem.css({'width' : MINIWIDTH,
                  'height': '40px',
                      'display' : 'inline-block'
                     });
    minielem.jqxDraw();
    minielem.data('pic', pic);
    var renderer2 = minielem.jqxDraw('getInstance');
    var elems2 = createMatrix(renderer2, false, null, true);
    var up = new ImageUpdater(renderer2, elems2);
    up.refresh(pic);
    pic.observe(up);
}

function createNewMini(pic) {
    var mid = "mini_" + pic.id;
    $("#miniatures").append('<div id="' + mid + '"></div>');
    var minielem = $("#" + mid);
    minielem.css({'width' : MINIWIDTH,
                      'height': '80px',
                      'display' : 'inline-block'
                     });
    minielem.jqxDraw();
    minielem.data('pic', pic);
    var renderer2 = minielem.jqxDraw('getInstance');
    var elems2 = createMatrix(renderer2, false, pic.id, true);
    var up = new ImageUpdater(renderer2, elems2);
    up.refresh(pic);
    pic.observe(up);
    minielem.on('click', function () {
        switchCurrentPicture($(this).data('pic'));
    })
}

function clear_all() {
    currentPicture.clear();
}

function clone_current_picture() {
    var npic = currentPicture.duplicate();
    pictures.push(npic);
    createNewMini(npic);
    npic.observe(progObserver);
    progObserver.refresh();
    switchCurrentPicture(npic);
}

function delete_current_picture() {
    if (pictures.length == 1) return;
    
    var cidx = pictures.indexOf(currentPicture);
    pictures.splice(cidx, 1);
    $("#mini_" + currentPicture.id).remove();
    var nextCidx = Math.min(cidx, pictures.length - 1);
    switchCurrentPicture(pictures[nextCidx]);
}

function setColor(elementId, color) {
    currentPicture.setPixel(elementId, color);
}
