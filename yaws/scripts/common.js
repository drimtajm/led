"use strict"
var pictures = [
    new_picture(false),
    new_picture(false)
]
var templatePictures = []
var currentPicture = pictures[0];
var currentTemplatePicture = null;
var main_up;
var progObserver;
var picProgObserver;

var pgmArray;
var pgmSource;
var selectedRow = null;


function submit_code() {
    saveProgram();
    console.info($('#submit_form').serialize())
    $.post("code.yaws", $('#submit_form').serialize(), function (data) {
            //alert(data)
        },
        "html");
}

function shutdown_matrix() {
    $.post("code.yaws", "close", null, "html");
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
    this.refresh = function (picture) {
        $('#all_code').text(createProgram());
    }

    this.pictureChange = function (picture, what, who, value) {
        this.refresh(picture);
    }
}

function PictureProgramUpdater() {
    this.refresh = function (picture) {
        $('#pictureCode').val(createPictureProgram(picture));
    }

    this.pictureChange = function (picture, what, who, value) {
        this.refresh(picture);
    }
}

function initAll() {
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
    picProgObserver.refresh(currentPicture);
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

function initMain() {
    //$("#container").css("width", COLUMNS*50);
    $("#container").css("width", "100%");
    $("#container").css("height", ROWS * 50);
    $("#container").jqxDraw();
    var renderer = $("#container").jqxDraw('getInstance');
    main_up = createMatrix(renderer, true, null, false, true);
    addMouseHandlers(main_up.renderer, main_up.elements);
    addMouseHandlers(main_up.renderer, main_up.indexElements);
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
            color = 'lightgreen';
        }
        el.css('background-color', color);
    }
}



function switchCurrentPicture(newPicture) {
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
    minielem.css({
        'width': MINIWIDTH,
        'height': '40px',
        'display': 'inline-block'
    });
    minielem.jqxDraw();
    minielem.data('pic', pic);
    var renderer2 = minielem.jqxDraw('getInstance');
    var up = createMatrix(renderer2, false, null, true);
    up.refresh(pic);
    pic.observe(up);
}

function createNewMini(pic) {
    var mid = "mini_" + pic.id;
    $("#miniatures").append('<div id="' + mid + '"></div>');
    var minielem = $("#" + mid);
    minielem.css({
        'width': MINIWIDTH,
        'height': '80px',
        'display': 'inline-block'
    });
    minielem.jqxDraw();
    minielem.data('pic', pic);
    var renderer2 = minielem.jqxDraw('getInstance');
    var up = createMatrix(renderer2, false, pic.id, true, false);
    up.refresh(pic);
    pic.observe(up);
    minielem.on('click', function () {
        switchCurrentPicture($(this).data('pic'));
    })
}

function saveProgram() {
    var pgm = Object();
    var pics = Array();
    pgm['nextPictureId'] = nextPictureId;
    pgm['pictures'] = pics;
    for (var i = 0, l = pictures.length; i < l; i++) {
        var p = pictures[i];
        var pix = Array();
        for (var j = 0; j < p.order.length; j++) {
            var pid = p.order[j];
            pix.push([pid, p.colors[pid]])
        }
        pics.push({
            id: p.id,
            pixels: pix
        });
    }
    var steps = Array();
    pgm['steps'] = steps;
    for (var i = 0, l = pgmSource.length; i < l; i++) {
        var p = pgmSource[i];
        var sco = {
            pictureId: p.pictureId,
            time: p.time,
            action: p.action
        };
        steps.push(sco);
    }
    var str = JSON.stringify(pgm);
    $('#saved_data').text(str);
}

function updateTemplateSelection() {
    for (var index = 0; index < templatePictures.length; index++) {
        var mid = '#template_mini_' + templatePictures[index].id;
        var el = $(mid)
        var color = '';
        if (templatePictures[index] == currentTemplatePicture) {
            color = 'lightgreen';
        }
        el.css('background-color', color);
    }
}

function createTemplateMini(pic) {
    var mid = "template_mini_" + pic.id;
    $("#templatesPanel").jqxPanel('append', $('<div id="' + mid + '"></div>'));
    var minielem = $("#" + mid);
    minielem.css({
        'width': MINIWIDTH,
        'height': '80px',
        'display': 'inline-block'
    });
    minielem.jqxDraw();
    minielem.data('pic', pic);
    var renderer2 = minielem.jqxDraw('getInstance');
    var up = createMatrix(renderer2, false, pic.id, true, false);
    up.refresh(pic);

    minielem.on('click', function () {
        currentTemplatePicture = $(this).data('pic');
        updateTemplateSelection();
    })
}

function loadTemplates(templates) {
    templatePictures = [];
    currentTemplatePicture = null;
    var spics = templates['pictures'];
    for (var i = 0, l = spics.length; i < l; i++) {
        var sp = spics[i];
        var p = new_picture(true);
        p.id = sp['id'];
        var pixels = sp['pixels'];
        for (var j = 0; j < pixels.length; j++) {
            p.setPixel(pixels[j][0], pixels[j][1]);
        }
        templatePictures.push(p)
    }
    $("#templatesPanel").jqxPanel('clearcontent');
    for (var index = 0; index < templatePictures.length; index++) {
        createTemplateMini(templatePictures[index]);
    }
}

function addSelectedTemplate() {
    if (currentTemplatePicture == null) return;
    var npic = currentTemplatePicture.duplicate();
    addAndSelectPicture(npic);
    addNewStep(npic);
    $("#programPanel").jqxDataTable('updateBoundData');
}

function loadProgram(savedPgm) {
    pictures = [];
    var spics = savedPgm['pictures'];
    for (var i = 0, l = spics.length; i < l; i++) {
        var sp = spics[i];
        var p = new_picture(false);
        p.id = sp['id'];
        var pixels = sp['pixels'];
        for (var j = 0; j < pixels.length; j++) {
            p.setPixel(pixels[j][0], pixels[j][1]);
        }
        pictures.push(p)
    }
    nextPictureId = savedPgm['nextPictureId'];
    var steps = savedPgm['steps'];
    pgmSource.splice(0, pgmSource.length);
    for (var j = 0; j < steps.length; j++) {
        var sstep = steps[j];
        var step = new ProgramStep();
        step.pictureId = sstep['pictureId'];
        step.time = sstep['time'];
        step.action = sstep['action'];
        pgmSource.push(step);
    }

    $("#miniatures").html('');
    for (var index = 0; index < pictures.length; index++) {
        createNewMini(pictures[index]);
    }
    switchCurrentPicture(pictures[0]);
    selectedRow = -1;
    $("#programPanel").jqxDataTable('updateBoundData');
}

function showAnimationOrder(showOrder) {
    main_up.showOrder = showOrder;
    main_up.refresh(currentPicture);
}

function clearAll() {
    currentPicture.clear();
}

function addAndSelectPicture(npic) {
    pictures.push(npic);
    createNewMini(npic);
    npic.observe(progObserver);
    progObserver.refresh();
    switchCurrentPicture(npic);
}

function newPicture() {
    var npic = new_picture(false);
    addAndSelectPicture(npic);
    addNewStep(npic);
    $("#programPanel").jqxDataTable('updateBoundData');
}

function cloneCurrentPicture() {
    var npic = currentPicture.duplicate();
    addAndSelectPicture(npic);
    addNewStep(npic);
    $("#programPanel").jqxDataTable('updateBoundData');
}

function deleteCurrentPicture() {
    if (pictures.length == 1) return;

    for (var i = 0; i < pgmSource.length;) {
        if (pgmSource[i].pictureId == currentPicture.id) {
            pgmSource.splice(i, 1);
        } else {
            i++;
        }
    }
    selectedRow = null;

    var cidx = pictures.indexOf(currentPicture);
    pictures.splice(cidx, 1);
    $("#mini_" + currentPicture.id).remove();
    var nextCidx = Math.min(cidx, pictures.length - 1);
    switchCurrentPicture(pictures[nextCidx]);
    progObserver.refresh();
    $("#programPanel").jqxDataTable('updateBoundData');
}

function setColor(elementId, color) {
    currentPicture.setPixel(elementId, color);
}

function addNewStep(pic) {
    var step = new ProgramStep();
    step.pictureId = pic.id;
    step.time = 2;
    step.action = 'Visa';
    pgmSource.push(step);
}

function createNewProgramStep() {
    addNewStep(currentPicture);
}

function deleteSelectedRow() {
    if (selectedRow != null) {
        pgmSource.splice(selectedRow, 1);
        selectedRow = null;
    }
}

function moveSelectedRowUp() {
    if (selectedRow != null) {
        if (selectedRow >= 1) {
            var tmp = pgmSource[selectedRow - 1];
            pgmSource[selectedRow - 1] = pgmSource[selectedRow];
            pgmSource[selectedRow] = tmp;
            selectedRow = selectedRow - 1;
        }
    }
}

function moveSelectedRowDown() {
    if (selectedRow != null) {
        if (selectedRow <= pgmSource.length - 2) {
            var tmp = pgmSource[selectedRow + 1];
            pgmSource[selectedRow + 1] = pgmSource[selectedRow];
            pgmSource[selectedRow] = tmp;
            selectedRow = selectedRow + 1;
        }
    }
}